library(readr)
library(ggplot2)
library(xtable)
library(dplyr)
library(tidyr)
library(scales)
library(ggsci)
library(stringr)
library(bit64)
library(RColorBrewer)
library(forcats)
library(ztable)
library(arrow)
library(cowplot)
library(grid)

source("base_setup.R")
source("plot_setup.R")
source("table_setup.R")
source("tikz_setup.R")
source("util.R")

perf_derive_metrics <- function(df) {
  df <- rows_delete(df, tibble(event=c(as.factor("context-switches"),as.factor("page-faults"))))
  df <- df %>%
    pivot_wider(id_cols=c(algorithm,dataset,dataset_size,target_chunk_size,iteration),names_from=event,values_from=value) %>%
    # We need to deal with dataset_size==0 because of the empty dataset...
    mutate(dataset_size=ifelse(dataset_size==0,NA_integer64_,dataset_size))

  # Annoying bug with ifelse and bit64: https://stackoverflow.com/questions/42248090/error-ifelsed-0-na-d-where-d-is-integer64-return-nonsense-in-num-form
  class(df$dataset_size) <- "integer64"

  df <- df %>%
    mutate(instructions_per_cycle=instructions/cycles) %>%
    mutate(kbranches=(branches/10^3)) %>%
    mutate(mbranches=(branches/10^6)) %>%
    mutate(gbranches=(branches/10^9)) %>%
    mutate(branches_per_byte=(branches/dataset_size)) %>%
    mutate(branch_miss_percentage=(`branch-misses`/branches)*100) %>%
    mutate(cache_miss_percentage=(`cache-misses`/`cache-references`)*100) %>%
    mutate(l1_dcache_miss_percentage=(`L1-dcache-misses`/`L1-dcache-loads`)*100) %>%
    mutate(instructions_per_byte=instructions/dataset_size) %>%
    mutate(usec_per_byte=(`task-clock`*1000)/dataset_size) %>%
    mutate(bytes_per_sec=dataset_size/(`task-clock`/1000)) %>%
    mutate(kibytes_per_sec=bytes_per_sec/(2^10)) %>%
    mutate(mibytes_per_sec=bytes_per_sec/(2^20)) %>%
    mutate(gibytes_per_sec=bytes_per_sec/(2^30)) %>%
    pivot_longer(cols=!c(algorithm,dataset,dataset_size,target_chunk_size,iteration),names_to = "event",values_to="value")
  
  #df$algorithm <- factor(df$algorithm, levels = ALGORITHM_ORDER)
  
  return(df)
}



######################################################################
# COMPUTATIONAL PERFORMANCE
######################################################################
perf_data <- tibble()
infiles <- Sys.glob(sprintf("%s/perf_*",csv_dir))
for (f in infiles) {
  tmp <- read_csv(f,col_types = "fcIiicd")

  if (isempty(perf_data)) {
    perf_data <- tmp
  } else {
    perf_data <- rows_append(perf_data,tmp)
  }
  rm(tmp)
}
perf_data <- perf_data %>%
  algorithm_as_factor() %>%
  perf_derive_metrics()

# Check for asymmetry, as per https://lemire.me/blog/2023/04/06/are-your-memory-bound-benchmarking-timings-normally-distributed/
perf_data %>%
  filter(dataset != "zero" & dataset != "empty") %>%
  filter(event=="usec_per_byte" & target_chunk_size==2048) %>%
  group_by(algorithm,dataset) %>%
  summarize(dataset_size=mean(dataset_size),target_chunk_size=mean(target_chunk_size),
            sd=sd(value),
            mean=mean(value),
            min=min(value),
            max=max(value),
            min_sd=(min(value) - mean(value))/sd(value),
            max_sd=(max(value) - mean(value))/sd(value)) %>%
  filter(abs(min_sd) > 2 | max_sd > 2) %>%
  print(n=Inf)

# Does the empty dataset help us estimate the overhead of perf?
perf_data %>%
  group_by(algorithm,dataset,event) %>%
  summarize(dataset_size=mean(dataset_size),target_chunk_size=mean(target_chunk_size),
            sd=sd(value),
            mean=mean(value)) %>%
  filter(event=="task-clock" & dataset %in% c("random","empty"))
# ...no :(

# TODO NOP offset

# Filter out the empty dataset because it's usually weird.
perf_data <- perf_data %>% filter(dataset != "empty")

######################################################################
#- Computational efficiency of QuickCDC variants on different Datasets in a bar plot
#  - x = Datasets, y = Bytes per second
#- Algorithms: QUICK_*
#  - Target chunk size: e.g. 2 KiB
#- Idea: How does QuickCDC perform w.r.t. caching and jumping on different datasets?
d <- perf_data %>%
  filter(algorithm %in% c(QUICKCDC_RABIN_ALGORITHMS, QUICKCDC_RABIN_ALGORITHMS_NOSKIP) |
           algorithm == "rabin_64"
         #| algorithm %in% QUICKCDC_RABIN_ALGORITHMS_NOSKIP
  ) %>%
  filter(dataset != "zero" & dataset != "empty")

# Calculate some statistics...
d <- d %>%
  group_by(algorithm,dataset,target_chunk_size,event) %>%
  summarize(dataset_size=mean(dataset_size), n=n(),
            val=mean(value),
            sd=sd(value),
            se=standard_error(value),
            mmd=mean_min_dev(value))

algos <- c("rabin_64",
           "quick_2_rabin_64_noskip", "quick_3_rabin_64_noskip",
           "quick_2_rabin_64", "quick_3_rabin_64",
           "quick_hash_2_rabin_64","quick_hash_3_rabin_64"
)
algo_labels <- c("Vanilla", "A-2-NS", "A-3-NS", "A-2", "A-3", "HM-2", "HM-3")
d$algorithm <- factor(d$algorithm, levels = algos)
d$dataset <- factor(d$dataset, levels = c("random", "lnx", "pdf", "web", "code"))

d <- d %>% drop_na(algorithm) %>% rename_datasets() %>%
  filter(event == "mibytes_per_sec" & target_chunk_size == 2048)

p <- d %>%
  ggplot(aes(y = val, fill = algorithm, x = dataset)) +
  geom_bar(position = position_dodge(), stat = "identity",
           colour = "black",
           linewidth = .3) + 
  geom_errorbar(aes(ymin = val - mmd, ymax = val + mmd),
                linewidth = .5, 
                width = .3,
                position = position_dodge(.9)) +
  labs(x = "Dataset", y = "Throughput (MiB/s)") +
  theme(legend.position = "none") + 
  guides(fill = guide_legend(ncol = 4)) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(12, "Set1"))(length(algos))) +
  scale_fill_jama(name = "",
                  breaks = algos,
                  labels = algo_labels)

print_plot(p,"perf_quickcdc_rabin_variants_different_datasets_2kib", height = 2)

dummy_plot <- p + theme_void() + 
  theme(legend.position = "bottom", text = element_text(size = 11))
legend <- cowplot::get_legend(dummy_plot)

if (!dev.cur()) dev.new()
grid.newpage()
grid.draw(legend)
legend_plot <- recordPlot()
dev.off()
dev.new()

print_plot(legend_plot, "perf_quickcdc_variants_different_datasets_2kib_legendonly", width=3.5, height=0.5)

######################################################################
#- Computational efficiency of QuickCDC variants on different Datasets in a bar plot
#  - x = Datasets, y = Bytes per second
#- Algorithms: QUICK_*
#  - Target chunk size: e.g. 2 KiB
#- Idea: How does QuickCDC perform w.r.t. caching and jumping on different datasets?
d <- perf_data %>%
  filter(algorithm %in% c(QUICKCDC_GEAR_ALGORITHMS, QUICKCDC_GEAR_ALGORITHMS_NOSKIP) |
           algorithm == "gear_nc_1"
         #| algorithm %in% QUICKCDC_RABIN_ALGORITHMS_NOSKIP
  ) %>%
  filter(dataset != "zero" & dataset != "empty")

# Calculate some statistics...
d <- d %>%
  group_by(algorithm,dataset,target_chunk_size,event) %>%
  summarize(dataset_size=mean(dataset_size), n=n(),
            val=mean(value), sd=sd(value), se=standard_error(value),
            mmd=mean_min_dev(value))

algos <- c("gear_nc_1",
           "quick_2_noskip", "quick_3_noskip",
           "quick_2", "quick_3", "quick_hash_2","quick_hash_3"
)
algo_labels <- c("Vanilla", "A-2-NS", "A-3-NS", "A-2", "A-3", "HM-2", "HM-3")
d$algorithm <- factor(d$algorithm, levels = algos)
d$dataset <- factor(d$dataset, levels = c("random", "lnx", "pdf", "web", "code"))

d <- d %>% drop_na(algorithm) %>% rename_datasets() %>%
  filter(event == "mibytes_per_sec" & target_chunk_size == 2048)

# Plot
p <- d %>%
  ggplot(aes(y=val,fill=algorithm,x=dataset)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           linewidth=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=val-mmd, ymax=val+mmd),
                linewidth=.5,    # Thinner lines
                width=.3,
                position=position_dodge(.9)) +
  labs(x="Dataset",y="Throughput (MiB/s)") +
  theme(legend.position="none") +
  guides(fill = guide_legend(ncol = 4)) +
  scale_fill_brewer(palette = "Set3") +
  scale_fill_jama(name="",
                  breaks=algos,
                  labels=algo_labels)

print_plot(p,"perf_quickcdc_gear_variants_different_datasets_2kib", height = 2)



######################################################################
#- Computational efficiency of QuickCDC variants on different Datasets in a bar plot
#  - x = Datasets, y = Bytes per second
#- Algorithms: QUICK_*
#  - Target chunk size: e.g. 2 KiB
#- Idea: How does QuickCDC perform w.r.t. caching and jumping on different datasets?

d <- perf_data %>%
  filter(algorithm %in% QUICKCDC_ALGORITHMS |
           algorithm == "gear_nc_1" |
           algorithm == "rabin_64"
  ) %>%
  filter(target_chunk_size == 2048) %>%
  filter(dataset != "zero" & dataset != "empty") %>%
  filter(event=="mibytes_per_sec") %>%
  filter(!grepl("hash", algorithm, fixed=TRUE))

# Calculate some statistics...
d <- d %>%
  group_by(algorithm,dataset,target_chunk_size,event) %>%
  summarize(dataset_size=mean(dataset_size), n=n(),
            val=mean(value), sd=sd(value), se=standard_error(value),
            mmd=mean_min_dev(value))

# Plot
p <- d %>%
  filter(!grepl("rabin",algorithm,fixed=TRUE)) %>%
  ggplot(aes(y=val,fill=algorithm,x=dataset)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           linewidth=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=val-mmd, ymax=val+mmd),
                linewidth=.5,    # Thinner lines
                width=.3,
                position=position_dodge(.9)) +
  labs(x="Dataset",y="Throughput (MiB/s)") +
  scale_fill_jama(name="", # Legend label
                  breaks=c("gear_nc_1","quick_2", "quick_3", "quick_2_noskip","quick_3_noskip"),
                  labels=c("Gear","A-2", "A-3","A-2-NS","A-3-NS"))

print_plot(p,"perf_quickcdc_gear_variants_different_datasets_2kib_skip_vs_cache")

# Plot
p <- d %>%
  filter(grepl("rabin",algorithm,fixed=TRUE)) %>%
  ggplot(aes(y=val,fill=algorithm,x=dataset)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           linewidth=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=val-mmd, ymax=val+mmd),
                linewidth=.5,    # Thinner lines
                width=.3,
                position=position_dodge(.9)) +
  labs(x="Dataset",y="Throughput (MiB/s)") +
  scale_fill_jama(name="", # Legend label
                  breaks=c("rabin_32","quick_2_rabin_32", "quick_3_rabin_32", "quick_2_rabin_32_noskip","quick_3_rabin_32_noskip"),
                  labels=c("Rabin","A-2", "A-3","A-2-NS","A-3-NS"))

print_plot(p,"perf_quickcdc_rabin_variants_different_datasets_2kib_skip_vs_cache")

################
#- Performance of QuickCDC variants on low/high entropy datasets, table
#- Idea: What influence do design decisions (hashmap vs. flat array; front/end feature vector length) have on performance?
#  - Dataset: Random, Code/Web

d <- perf_data %>%
  filter(algorithm %in% QUICKCDC_RABIN_ALGORITHMS | algorithm == "rabin_32") %>%
  filter(dataset %in% c("random","zero")) %>%
  group_by(algorithm,dataset,target_chunk_size,event) %>%
  summarize(dataset_size=mean(dataset_size), n=n(),
            val=mean(value), sd=sd(value),
            se=standard_error(value),
            max=max(value),
            mmd=mean_min_dev(value)) %>%
  filter(target_chunk_size == 8192 | target_chunk_size == 512) %>%
  filter(event %in% c("task-clock","mibytes_per_sec","usec_per_byte","branch_miss_percentage","cache_miss_percentage","l1_dcache_miss_percentage","instructions_per_cycle","instructions_per_byte"))

# Create table
t <- d %>%
  pivot_wider(id_cols=c(algorithm,dataset,dataset_size,target_chunk_size,n),names_from=event,values_from=c("val","se","max"),names_glue = "{event}_{.value}",names_vary="slowest") %>%
  mutate(dataset_size=NULL, n=NULL) %>%
  arrange(target_chunk_size,dataset,algorithm) %>%
  select(algorithm,dataset,target_chunk_size,
         #starts_with("task-clock"),
         #starts_with("mibytes_per_sec"),
         mibytes_per_sec_val,
         mibytes_per_sec_max,
         #starts_with("usec_per_byte"),
         #starts_with("instructions_per_byte"),
         instructions_per_byte_val,
         instructions_per_byte_se,
         #starts_with("instructions_per_cycle"),
         instructions_per_cycle_val,
         instructions_per_cycle_se,
         #starts_with("l1_dcache_miss_percentage"),
         l1_dcache_miss_percentage_val,
         l1_dcache_miss_percentage_se,
         #starts_with("branch_miss_percentage")
         #branch_miss_percentage_val,
         #branch_miss_percentage_se
  )

addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- '&&& \\multicolumn{2}{c}{Throughput (MiB/s)} &
\\multicolumn{2}{c}{Inst./B} &
\\multicolumn{2}{c}{IPC} &
\\multicolumn{2}{c}{L1 DCache Miss (\\%)}\\\\
\\cmidrule(lr){4-11}
Algorithm & Dataset & Target CS & $\\mu$ & Max & $\\mu$ & SE & $\\mu$ & SE & $\\mu$ & SE\\\\'

print(xtable(t, digits=4), file="tab/perf_quickcdc_rabin_variants.tex", add.to.row=addtorow,include.colnames=F,floating=FALSE)

rm(d,p,t,addtorow,algos)
gc()

######################################################################
#- Computational efficiency of QuickCDC variants on Targets in a line plot
#  - x = Target chunk size, y = Bytes per second
#- Algorithms: QUICK_*
#  - Dataset: e.g. CODE
#- Idea: How does QuickCDC perform on a dataset where it can cache and jump well, w.r.t. different target chunk sizes = jump lengths?
#  - Would be interesting to see the relationship between chunk size and speed, I think one high-correlation dataset is enough to make a point
d <- perf_data %>%
  filter(dataset == "code") %>%
  filter(algorithm %in% QUICKCDC_RABIN_ALGORITHMS | algorithm == "rabin_64") %>%
  filter(target_chunk_size %in% POWER_OF_TWO_SIZES) %>%
  mutate(target_chunk_size = as.factor(target_chunk_size))
 
# Calculate some statistics...
d <- d %>%
  group_by(algorithm,dataset,target_chunk_size,event) %>%
  summarize(bytes=mean(dataset_size), n=n(),
            val=mean(value), sd=sd(value), se=standard_error(value),
            mmd=mean_min_dev(value))

p <- d %>%
  filter(event=="mibytes_per_sec") %>%
  ggplot(aes(y=val,group=algorithm,color=algorithm,x=target_chunk_size)) +
  geom_errorbar(aes(ymin=val-mmd, ymax=val+mmd), colour="black", width=.25, position=position_dodge(0.1)) +
  geom_line(position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=1, shape=21, fill="white") + # 21 is filled circle
  labs(x="Target Chunk Size",y="Throughput (MiB/s)") +
  theme(legend.position = "none") +
  scale_color_jama(name="", # Legend label
                   breaks=c("rabin_64","quick_2_rabin_64", "quick_3_rabin_64", "quick_hash_2_rabin_64","quick_hash_3_rabin_64"),
                   labels=c("Vanilla","A-2", "A-3","HM-2","HM-3"))
#breaks=c("gear_nc_1","quick_2", "quick_3", "quick_hash_2","quick_hash_3"),
#labels=c("Gear","A-2", "A-3","HM-2","HM-3"))

print_plot(p,"perf_quickcdc_rabin_variants_different_targets_code", height=2)

# legend
dummy_plot <- p + theme_void() + 
  theme(legend.position = "bottom", text = element_text(size = 11))
legend <- cowplot::get_legend(dummy_plot)

if (!dev.cur()) dev.new()
grid.newpage()
grid.draw(legend)
legend_plot <- recordPlot()
dev.off()
dev.new()

print_plot(legend_plot, "perf_quickcdc_variants_different_targets_code_legendonly", width=3.5, height=0.3)

rm(d,p)
gc()

##################
### Same for Gear

d <- perf_data %>%
  filter(dataset == "code") %>%
  filter(algorithm %in% QUICKCDC_GEAR_ALGORITHMS | algorithm == "gear_nc_1") %>%
  filter(target_chunk_size %in% POWER_OF_TWO_SIZES) %>%
  mutate(target_chunk_size = as.factor(target_chunk_size))

# Calculate some statistics...
d <- d %>%
  group_by(algorithm,dataset,target_chunk_size,event) %>%
  summarize(bytes=mean(dataset_size), n=n(),
            val=mean(value), sd=sd(value), se=standard_error(value),
            mmd=mean_min_dev(value))

p <- d %>%
  filter(event=="mibytes_per_sec") %>%
  ggplot(aes(y=val,group=algorithm,color=algorithm,x=target_chunk_size)) +
  geom_errorbar(aes(ymin=val-mmd, ymax=val+mmd), colour="black", width=.25, position=position_dodge(0.1)) +
  geom_line(position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=1, shape=21, fill="white") + # 21 is filled circle
  labs(x="Target Chunk Size",y="Throughput (MiB/s)") +
  theme(legend.position = "none") +
  scale_color_jama(name="", # Legend label
                   breaks=c("gear_nc_1","quick_2", "quick_3", "quick_hash_2","quick_hash_3"),
                   labels=c("Vanilla","A-2", "A-3","HM-2","HM-3"))


print_plot(p,"perf_quickcdc_gear_variants_different_targets_code", height=2)

rm(d,p)
gc()

######################################################################
#- Performance of Gear-variations, probably table
#- Idea: How much does manual Vectorization (SIMD) help?
d <- perf_data %>%
  filter(dataset == "random") %>%
  filter(algorithm %in% c("gear64","gear64_simd")) %>%
  filter(target_chunk_size %in% POWER_OF_TWO_SIZES)

# Compute statistics
d <- d %>%
  group_by(algorithm,dataset,target_chunk_size,event) %>%
  summarize(dataset_size=mean(dataset_size), n=n(),
            val=mean(value), sd=sd(value),
            se=standard_error(value),
            max=max(value),
            mmd=mean_max_dev(value))

#ggplot(d %>% filter(event=="branches"),aes(x=target_chunk_size,y=val,color=algorithm)) +
#  geom_errorbar(aes(ymin=val-se, ymax=val+se), colour="black", width=100, position=position_dodge(0.1)) +
#  geom_line(position=position_dodge(0.1)) +
#  geom_point(position=position_dodge(0.1), size=3, shape=21, fill="white") # 21 is filled circle
#
#ggplot(d %>% filter(target_chunk_size==2048 ),aes(y=val,fill=algorithm,x=event)) +
#  geom_bar(position=position_dodge(), stat="identity",
#           colour="black", # Use black outlines,
#           linewidth=.3) +      # Thinner lines
#  geom_errorbar(aes(ymin=val-se, ymax=val+se),
#                linewidth=.3,    # Thinner lines
#                width=.2,
#                position=position_dodge(.9))

# Create table
t <- d %>%
  filter(event %in% c('mibytes_per_sec',
                      #"task-clock",
                      "gbranches",
                      #"branch_miss_percentage",
                      "cache_miss_percentage",
                      "instructions_per_byte",
                      "instructions_per_cycle")) %>%
  ungroup() %>%
  mutate(dataset=NULL,dataset_size=NULL,n=NULL) %>%
  pivot_wider(id_cols=c(algorithm,target_chunk_size),names_from=event,values_from=c("val","se","max"),names_glue = "{event}_{.value}",names_vary="slowest") %>%
  arrange(target_chunk_size,algorithm) %>%
  select(algorithm,target_chunk_size,
         mibytes_per_sec_val,
         mibytes_per_sec_max,
         #`task-clock_val`,
         #`task-clock_mmd`,
         instructions_per_byte_val,
         instructions_per_byte_se,
         instructions_per_cycle_val,
         instructions_per_cycle_se,
         #starts_with("task-clock"),
         #starts_with("instructions_per_byte"),
         #starts_with("instructions_per_cycle")#,
         #starts_with("branches"),
         #starts_with("branch_miss_percentage")
         gbranches_val,
         gbranches_se
  )
t$algorithm <- recode(t$algorithm, gear64="Scalar", gear64_simd="SIMD")

addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- '&& \\multicolumn{2}{c}{Throughput (MiB/s)} & \\multicolumn{2}{c}{Inst./B} & \\multicolumn{2}{c}{IPC} & \\multicolumn{2}{c}{Branches ($\\times 10^9$)}\\\\
\\cmidrule(lr){3-10}
Algorithm & Target CS (B) & $\\mu$ & Max & $\\mu$ & SE (±) & $\\mu$ & SE (±) & $\\mu$ & SE (±)\\\\'

print(xtable(t, digits=3), file="tab/perf_gear_simd_comparison_random.tex", add.to.row=addtorow,include.colnames=F,floating=FALSE)

p <- d %>%
  filter(event %in% c('mibytes_per_sec'
                      #"task-clock",
                      #"gbranches",
                      #"branch_miss_percentage",
                      #"cache_miss_percentage",
                      #"instructions_per_byte",
                      #"instructions_per_cycle"
  )) %>%
  ungroup() %>%
  mutate(target_chunk_size = as.factor(target_chunk_size)) %>%
  mutate(dataset=NULL,dataset_size=NULL,n=NULL) %>%
  mutate(algorithm = fct_recode(algorithm, Scalar="gear64",SIMD="gear64_simd")) %>%
  ggplot(aes(group=algorithm, x=target_chunk_size, y=val, fill=algorithm)) +
  # Plot error bars first so the barplot is over it, i.e., hides the lower whisker.
  geom_errorbar(aes(ymin=val*0.5, ymax = max),
                position = position_dodge(1),
                linewidth=.5,    # Thinner lines
                width=.3,) +
  geom_bar(stat="identity",
           position = position_dodge(1),
           colour="black", # Use black outlines,
           linewidth=.3) +      # Thinner lines
  labs(x="Target Chunk Size",y="Throughput (MiB/s)") +
  theme(legend.position="bottom") +
  scale_fill_jama(name="")

print_plot(p, "perf_gear_simd_throughput_comparison_chunk_sizes_random")

rm(d,t,p,addtorow)
gc()


######################################################################
# Performance overview
d <- perf_data %>%
  filter(dataset == "random") %>%
  filter(algorithm %in% ALGORITHMS_TO_COMPARE) %>%
  filter(target_chunk_size == 2048) %>%
  group_by(algorithm,target_chunk_size,event) %>%
  summarize(dataset_size=mean(dataset_size), n=n(),
            val=mean(value),
            sd=sd(value),
            se=standard_error(value),
            mmd=mean_max_dev(value),
            max=max(value)) %>% 
  rename_algorithms()

t <- d %>%
  filter(event %in% c("mibytes_per_sec",
                      "instructions_per_byte",
                      "instructions_per_cycle",
                      "branches_per_byte",
                      "branch_miss_percentage")) %>%
  ungroup() %>%
  mutate(n=NULL, target_chunk_size=NULL,dataset_size = NULL) %>%
  pivot_wider(id_cols=c(algorithm),names_from=event,values_from=c("val","se","max"),names_glue = "{event}_{.value}",names_vary="slowest") %>%
  arrange(algorithm) %>%
  select(algorithm,
         mibytes_per_sec_val,
         mibytes_per_sec_max,
         instructions_per_byte_val,
         instructions_per_byte_se,
         instructions_per_cycle_val,
         instructions_per_cycle_se,
         branches_per_byte_val,
         branches_per_byte_se,
         branch_miss_percentage_val,
         branch_miss_percentage_se
         #starts_with("mibytes_per_sec"),
         #starts_with("instructions_per_byte"),
         #starts_with("instructions_per_cycle")
  )

addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- '& \\multicolumn{2}{c}{Throughput (MiB/s)} & \\multicolumn{2}{c}{Inst./B} & \\multicolumn{2}{c}{IPC} & \\multicolumn{2}{c}{Branches/B}  & \\multicolumn{2}{c}{Branch Miss \\%}\\\\
\\cmidrule(lr){2-11}
Algorithm & $\\mu$ & Max & $\\mu$ & SE (±) & $\\mu$ & SE (±) & $\\mu$ & SE (±) & $\\mu$ & SE (±)\\\\'

print(xtable(t, digits=3), file="tab/perf_overview_random_2kib.tex", add.to.row=addtorow,include.colnames=F,floating=FALSE)

algorithm_order <- d %>%
  filter(event %in% c("mibytes_per_sec")) %>%
  group_by(algorithm) %>%
  summarize(average_val = mean(val, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(desc(average_val))) %>%
  pull(algorithm) # Extract the ordered algorithm names

# Step 2: Use this order to set the levels of the algorithm factor in d
d$algorithm <- factor(d$algorithm, levels = algorithm_order)

# Now, plotting
p <- d %>%
  filter(event %in% c("mibytes_per_sec")) %>%
  mutate(algorithm = factor(algorithm, levels = unique(algorithm))) %>%
  ggplot(aes(x=val, y=algorithm)) +
  geom_bar(stat="identity", linewidth=.3, colour="black") +
  labs(x="Throughput (MiB/s)", y=NULL) +
  theme(legend.position="bottom")

print_plot(p, "perf_overview_throughput_random_2kib")

rm(d,t,p,addtorow)
gc()
