library(readr)
library(ggplot2)
library(xtable)
library(dplyr)
library(tidyr)
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

######################################################################
#- Computational efficiency of QuickCDC variants on different Datasets in a bar plot
#  - x = Datasets, y = Bytes per second
#- Algorithms: QUICK_*
#  - Target chunk size: e.g. 2 KiB
#- Idea: How does QuickCDC perform w.r.t. caching and jumping on different datasets?
d <- perf_data %>%
  rename_datasets() %>%
  filter(algorithm %in% c(QUICKCDC_RABIN_ALGORITHMS, QUICKCDC_RABIN_ALGORITHMS_NOSKIP) |
           algorithm == "rabin_32"
  ) %>%
  filter(!grepl("hash", algorithm, fixed=TRUE))

# Calculate some statistics...
d <- d %>%
  group_by(algorithm,dataset,target_chunk_size,event) %>%
  summarize(dataset_size=mean(dataset_size), n=n(),
            val=median(value),
            sd=sd(value),
            se=standard_error(value),
            mmd=mean_max_dev(value),
            q25=quantile(value,probs=0.25,names=FALSE),
            q75=quantile(value, probs=0.75, names=FALSE)
  )

algos <- c("rabin_32",
           "quick_2_rabin_32_noskip", "quick_3_rabin_32_noskip",
           "quick_2_rabin_32", "quick_3_rabin_32")
algo_labels <- c("Vanilla", "A-2-NS", "A-3-NS", "A-2", "A-3")
d$algorithm <- factor(d$algorithm, levels = algos)

d <- d %>%
  drop_na(algorithm)  %>%
  filter(event == "mibytes_per_sec" & target_chunk_size == 2048)

p <- d %>%
  ggplot(aes(y = val,
           fill = algorithm,
           x = dataset)) +
  geom_bar(position = position_dodge(), stat = "identity",
           colour = "black",
           linewidth = .3) +
  geom_errorbar(aes(ymin = q25, ymax = q75),
                linewidth = .5,
                width = .3,
                position = position_dodge(.9)
  ) +
  labs(x = "Dataset", y = "Throughput (MiB/s)") +
  theme(legend.position = "right") +
  guides(fill = guide_legend(ncol = 1)) +
  scale_fill_jama(name = "",
                  breaks = algos,
                  labels = algo_labels)
print_plot(p,"perf_quickcdc_rabin_variants_different_datasets_2kib", height = 1.7)

######################################################################
# The same, but around Gear
d <- perf_data %>%
  rename_datasets() %>%
  filter(algorithm %in% c(QUICKCDC_GEAR_ALGORITHMS, QUICKCDC_GEAR_ALGORITHMS_NOSKIP) |
           algorithm == "gear_nc_1"
  ) %>%
  filter(!grepl("hash", algorithm, fixed=TRUE))

# Calculate some statistics...
d <- d %>%
  group_by(algorithm,dataset,target_chunk_size,event) %>%
  summarize(dataset_size=mean(dataset_size), n=n(),
            val=median(value),
            sd=sd(value),
            se=standard_error(value),
            mmd=mean_min_dev(value),
            q25=quantile(value,probs=0.25,names=FALSE),
            q75=quantile(value, probs=0.75, names=FALSE)
  )

algos <- c("gear_nc_1",
           "quick_2_noskip", "quick_3_noskip",
           "quick_2", "quick_3")
algo_labels <- c("Vanilla", "A-2-NS", "A-3-NS", "A-2", "A-3")
d$algorithm <- factor(d$algorithm, levels = algos)

d <- d %>%
  drop_na(algorithm) %>%
  filter(event == "mibytes_per_sec" & target_chunk_size == 2048)

# Plot
p <- d %>%
  ggplot(aes(y=val,fill=algorithm,x=dataset)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           linewidth=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=q25, ymax=q75),
                linewidth=.5,    # Thinner lines
                width=.3,
                position=position_dodge(.9)) +
  labs(x="Dataset",y="Throughput (MiB/s)") +
  theme(legend.position="right") +
  guides(fill = guide_legend(ncol = 1)) +
  scale_fill_jama(name=NULL,
                  breaks=algos,
                  labels=algo_labels)

print_plot(p,"perf_quickcdc_gear_variants_different_datasets_2kib", height = 1.7)

################
#- Performance of QuickCDC variants on low/high entropy datasets, table
#- Idea: What influence do design decisions (hashmap vs. flat array; front/end feature vector length) have on performance?
#  - Dataset: Random, Code/Web
#
d <- perf_data %>%
  filter(algorithm %in% QUICKCDC_RABIN_ALGORITHMS | algorithm == "rabin_32") %>%
  filter(dataset %in% c("random","code")) %>%
  group_by(algorithm,dataset,target_chunk_size,event) %>%
  summarize(dataset_size=mean(dataset_size), n=n(),
            val=mean(value), sd=sd(value),
            se=standard_error(value),
            max=max(value),
            mmd=mean_min_dev(value),
            med=median(value),
            iqd=(quantile(value, probs=0.75, names=FALSE) - quantile(value, probs=0.25, names=FALSE))
  ) %>%
  filter(target_chunk_size == 8192 | target_chunk_size == 512) %>%
  filter(event %in% c("task-clock","mibytes_per_sec","usec_per_byte","branch_miss_percentage","cache_miss_percentage","l1_dcache_miss_percentage","instructions_per_cycle","instructions_per_byte")) %>%
  rename_algorithms() %>%
  rename_datasets()

# Create table
t <- d %>%
  pivot_wider(
    id_cols=c(algorithm,dataset,dataset_size,target_chunk_size,n),
    names_from=event,values_from=c("med","iqd","val","se","max"),
    names_glue = "{event}_{.value}",
    names_vary="slowest") %>%
  mutate(dataset_size=NULL, n=NULL) %>%
  arrange(target_chunk_size,dataset,algorithm) %>%
  select(algorithm,dataset,target_chunk_size,
         #starts_with("task-clock"),
         #starts_with("mibytes_per_sec"),
         mibytes_per_sec_med,
         mibytes_per_sec_iqd,
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
  )  %>%
  mutate(
    target_chunk_size = tex_format_number(target_chunk_size),
    mibytes_per_sec_med = tex_format_number(round(mibytes_per_sec_med,digits=1)),
    mibytes_per_sec_iqd = tex_format_percentage(mibytes_per_sec_iqd, digits=1, percentage_sign=FALSE),

    instructions_per_byte_val = tex_format_number(round(instructions_per_byte_val, digits=2)),
    instructions_per_byte_se = tex_format_percentage(instructions_per_byte_se, percentage_sign=FALSE),

    instructions_per_cycle_val = tex_format_number(round(instructions_per_cycle_val, digits=2)),
    instructions_per_cycle_se = tex_format_percentage(instructions_per_cycle_se, percentage_sign=FALSE),

    l1_dcache_miss_percentage_val = tex_format_number(round(l1_dcache_miss_percentage_val, digits=2)),
    l1_dcache_miss_percentage_se = tex_format_percentage(l1_dcache_miss_percentage_se, percentage_sign=FALSE),
  )

addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- '&&& \\multicolumn{2}{c}{Throughput (MiB/s)} &
\\multicolumn{2}{c}{Inst./B} &
\\multicolumn{2}{c}{IPC} &
\\multicolumn{2}{c}{L1 DCache Miss (\\%)}\\\\
\\cmidrule(lr){4-5}
\\cmidrule(lr){6-7}
\\cmidrule(lr){8-9}
\\cmidrule(lr){10-11}
Algorithm & Dataset & Target (B) & Median & IQR & Mean & SE & Mean & SE & Mean & SE\\\\'

print(
  xtable(t, align = c("r", "l", "l", "r", "r", "r","r", "r","r", "r","r", "r")),
  file="tab/perf_quickcdc_rabin_variants.tex",
  add.to.row=addtorow,include.colnames=F,floating=FALSE,
  sanitize.text.function=function(s){s}, sanitize.colnames.function=NULL)

rm(d,p,t,addtorow,algos)
gc()


###################
# The same, around gear_nc_1

d <- perf_data %>%
  filter(algorithm %in% QUICKCDC_GEAR_ALGORITHMS | algorithm == "gear_nc_1") %>%
  filter(dataset %in% c("random","code")) %>%
  group_by(algorithm,dataset,target_chunk_size,event) %>%
  summarize(dataset_size=mean(dataset_size), n=n(),
            val=mean(value), sd=sd(value),
            se=standard_error(value),
            max=max(value),
            mmd=mean_min_dev(value),
            med=median(value),
            iqd=(quantile(value, probs=0.75, names=FALSE) - quantile(value, probs=0.25, names=FALSE))
  ) %>%
  filter(target_chunk_size == 8192 | target_chunk_size == 512) %>%
  filter(event %in% c("task-clock","mibytes_per_sec","usec_per_byte","branch_miss_percentage","cache_miss_percentage","l1_dcache_miss_percentage","instructions_per_cycle","instructions_per_byte")) %>%
  rename_algorithms() %>%
  rename_datasets()

# Create table
t <- d %>%
  pivot_wider(
    id_cols=c(algorithm,dataset,dataset_size,target_chunk_size,n),
    names_from=event,values_from=c("med","iqd","val","se","max"),
    names_glue = "{event}_{.value}",
    names_vary="slowest") %>%
  mutate(dataset_size=NULL, n=NULL) %>%
  arrange(target_chunk_size,dataset,algorithm) %>%
  select(algorithm,dataset,target_chunk_size,
         #starts_with("task-clock"),
         #starts_with("mibytes_per_sec"),
         mibytes_per_sec_med,
         mibytes_per_sec_iqd,
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
  ) %>%
  mutate(
    target_chunk_size = tex_format_number(target_chunk_size),
    mibytes_per_sec_med = tex_format_number(round(mibytes_per_sec_med,digits=1)),
    mibytes_per_sec_iqd = tex_format_percentage(mibytes_per_sec_iqd, digits=1, percentage_sign=FALSE),

    instructions_per_byte_val = tex_format_number(round(instructions_per_byte_val, digits=2)),
    instructions_per_byte_se = tex_format_percentage(instructions_per_byte_se, percentage_sign=FALSE),

    instructions_per_cycle_val = tex_format_number(round(instructions_per_cycle_val, digits=2)),
    instructions_per_cycle_se = tex_format_percentage(instructions_per_cycle_se, percentage_sign=FALSE),

    l1_dcache_miss_percentage_val = tex_format_number(round(l1_dcache_miss_percentage_val, digits=2)),
    l1_dcache_miss_percentage_se = tex_format_percentage(l1_dcache_miss_percentage_se, percentage_sign=FALSE),
  )

addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- '&&& \\multicolumn{2}{c}{Throughput (MiB/s)} &
\\multicolumn{2}{c}{Inst./B} &
\\multicolumn{2}{c}{IPC} &
\\multicolumn{2}{c}{L1D Miss (\\%)}\\\\
\\cmidrule(lr){4-5}
\\cmidrule(lr){6-7}
\\cmidrule(lr){8-9}
\\cmidrule(lr){10-11}
Algorithm & Dataset & Target (B) & Median & IQR & Mean & SE & Mean & SE & Mean & SE\\\\'

print(
  xtable(t, align = c("r", "l", "l", "r", "r", "r","r", "r","r", "r","r", "r")),
  file="tab/perf_quickcdc_gear_variants.tex",
  add.to.row=addtorow,include.colnames=F,floating=FALSE,
  sanitize.text.function=function(s){s}, sanitize.colnames.function=NULL)

rm(d,t,addtorow)
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
  filter(algorithm %in% QUICKCDC_RABIN_ALGORITHMS | algorithm == "rabin_32") %>%
  filter(target_chunk_size %in% POWER_OF_TWO_SIZES) %>%
  mutate(target_chunk_size = as.factor(target_chunk_size))

# Calculate some statistics...
d <- d %>%
  group_by(algorithm,dataset,target_chunk_size,event) %>%
  summarize(bytes=mean(dataset_size), n=n(),
            val=mean(value),
            sd=sd(value),
            se=standard_error(value),
            mmd=mean_min_dev(value),
            med=median(value),
            q25=quantile(value,probs=0.25,names=FALSE),
            q75=quantile(value, probs=0.75, names=FALSE),
            iqd=(quantile(value, probs=0.75, names=FALSE) - quantile(value, probs=0.25, names=FALSE))
            )

p <- d %>%
  filter(event=="mibytes_per_sec") %>%
  ggplot(aes(y=med,group=algorithm,color=algorithm,x=target_chunk_size)) +
  geom_errorbar(aes(ymin=q25, ymax=q75), colour="black", width=.25, position=position_dodge(0.1)) +
  geom_line(position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=1, shape=21, fill="white") + # 21 is filled circle
  labs(x="Target Chunk Size",y="Throughput (MiB/s)") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0,1000) +
  scale_color_jama(name=NULL,
                   breaks=c("rabin_32","quick_2_rabin_32", "quick_3_rabin_32", "quick_hash_2_rabin_32","quick_hash_3_rabin_32"),
                   labels=c("Vanilla","A-2", "A-3","HM-2","HM-3"))

print_plot(p,"perf_quickcdc_rabin_variants_different_targets_code", height=2, width=2)

# legend
print_plot(get_legend_plot(p, 5), "perf_quickcdc_variants_different_targets_code_legendonly", width=4, height=0.5)

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
            mmd=mean_min_dev(value),
            med=median(value),
            q25=quantile(value,probs=0.25,names=FALSE),
            q75=quantile(value, probs=0.75, names=FALSE),
            iqd=(quantile(value, probs=0.75, names=FALSE) - quantile(value, probs=0.25, names=FALSE))
  )

p <- d %>%
  filter(event=="mibytes_per_sec") %>%
  ggplot(aes(y=med,group=algorithm,color=algorithm,x=target_chunk_size)) +
  geom_errorbar(aes(ymin=q25, ymax=q75), colour="black", width=.25, position=position_dodge(0.1)) +
  geom_line(position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=1, shape=21, fill="white") + # 21 is filled circle
  labs(x="Target Chunk Size",y="Throughput (MiB/s)") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0,1000)+
  scale_color_jama(name=NULL, # Legend label
                   breaks=c("gear_nc_1","quick_2", "quick_3", "quick_hash_2","quick_hash_3"),
                   labels=c("Vanilla","A-2", "A-3","HM-2","HM-3"))


print_plot(p,"perf_quickcdc_gear_variants_different_targets_code", height=2, width=2)

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
            mmd=mean_max_dev(value),
            med=median(value),
            q25=quantile(value,probs=0.25,names=FALSE),
            q75=quantile(value, probs=0.75, names=FALSE),
            iqd=(quantile(value, probs=0.75, names=FALSE) - quantile(value, probs=0.25, names=FALSE))
  )

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
  filter(target_chunk_size %in% c(512, 8192)) %>%
  mutate(dataset=NULL,dataset_size=NULL,n=NULL) %>%
  pivot_wider(
    id_cols=c(algorithm,target_chunk_size),
    names_from=event,
    values_from=c("med","iqd", "val","se","max"),
    names_glue = "{event}_{.value}",
    names_vary="slowest") %>%
  arrange(target_chunk_size,algorithm) %>%
  select(algorithm,target_chunk_size,
         mibytes_per_sec_med,
         mibytes_per_sec_iqd,
         #`task-clock_val`,
         #`task-clock_mmd`,
         instructions_per_byte_val,
         #instructions_per_byte_se,
         instructions_per_cycle_val,
         #instructions_per_cycle_se,
         #starts_with("task-clock"),
         #starts_with("instructions_per_byte"),
         #starts_with("instructions_per_cycle")#,
         #starts_with("branches"),
         #starts_with("branch_miss_percentage")
         gbranches_val,
         #gbranches_se
  ) %>%
  mutate(
    algorithm=recode(algorithm, gear64="Scalar", gear64_simd="SIMD")
  ) %>%
  mutate(
    target_chunk_size = tex_format_number(target_chunk_size),

    mibytes_per_sec_med = tex_format_number(round(mibytes_per_sec_med,digits=1)),
    mibytes_per_sec_iqd = tex_format_percentage(mibytes_per_sec_iqd, digits=1, percentage_sign=FALSE),

    instructions_per_byte_val = tex_format_number(round(instructions_per_byte_val, digits=2)),
    #instructions_per_byte_se = tex_format_percentage(instructions_per_byte_se, percentage_sign=FALSE),

    instructions_per_cycle_val = tex_format_number(round(instructions_per_cycle_val, digits=2)),
    #instructions_per_cycle_se = tex_format_percentage(instructions_per_cycle_se, percentage_sign=FALSE),

    gbranches_val = tex_format_number(round(gbranches_val, digits=2)),
    #gbranches_se = tex_format_percentage(gbranches_se, percentage_sign=FALSE),
  )

addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- '&& \\multicolumn{2}{c}{\\makecell{Throughput\\\\(MiB/s)}} &&&\\\\
\\cmidrule(lr){3-4}
Alg. & $\\mu$ (B) & Median & IQR & Inst./B & IPC & \\makecell{Br.\\\\($\\times 10^9$)} \\\\'

print(
  xtable(t, align=c("l","l","r","r","r","r","r","r")),
  file="tab/perf_gear_simd_comparison_random.tex",
  add.to.row=addtorow,include.colnames=F,floating=FALSE,
  sanitize.text.function=function(s){s}, sanitize.colnames.function=NULL)

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
  ggplot(aes(group=algorithm, x=target_chunk_size, y=med, fill=algorithm)) +
  geom_bar(stat="identity",
           position = position_dodge(1),
           colour="black", # Use black outlines,
           linewidth=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=q25, ymax = q75),
                position = position_dodge(1),
                linewidth=.5,    # Thinner lines
                width=.3,) +
  labs(x="Target Chunk Size (B)",y="Throughput (MiB/s)", fill=NULL) +
  theme(legend.position="right") +
  scale_fill_brewer(type="div", palette = 4, direction = -1)

print_plot(p, "perf_gear_simd_throughput_comparison_chunk_sizes_random", height=2, width=4)

rm(d,t,p,addtorow)
gc()


######################################################################
# Performance overview
algorithms_to_compare_tmp <- c("fsc","ae","ram","mii","pci","rabin_32","buzhash_64","gear", "gear64", "gear64_simd","bfbc","bfbc_custom_div")
d <- perf_data %>%
  filter(dataset == "random" | (algorithm == "bfbc" & dataset == "code")) %>%
  filter(algorithm %in% algorithms_to_compare_tmp) %>%
  filter(target_chunk_size == 2048) %>%
  mutate(algorithm = case_when(
    algorithm == "bfbc" & dataset == "code" ~ "BFBC-L",
    TRUE ~ algorithm
  )) %>%
  group_by(algorithm, target_chunk_size, event) %>%
  summarize(
    dataset_size = mean(dataset_size),
    n = n(),
    val = mean(value),
    sd = sd(value),
    se = standard_error(value),
    mmd = mean_max_dev(value),
    max = max(value),
    med = median(value),
    q25 = quantile(value, probs = 0.25, names = FALSE),
    q75 = quantile(value, probs = 0.75, names = FALSE),
    iqd = (quantile(value, probs = 0.75, names = FALSE) - quantile(value, probs = 0.25, names = FALSE))
  ) %>%
  rename_algorithms()

t <- d %>%
  filter(event %in% c("mibytes_per_sec",
                      "instructions_per_byte",
                      "instructions_per_cycle",
                      "branches_per_byte",
                      "branch_miss_percentage")) %>%
  ungroup() %>%
  mutate(n=NULL, target_chunk_size=NULL, dataset_size=NULL) %>%
  pivot_wider(
    id_cols=c(algorithm),
    names_from=event,
    values_from=c("iqd", "val", "med", "se", "max"),
    names_glue = "{event}_{.value}",
    names_vary="slowest") %>%
  arrange(-mibytes_per_sec_med) %>%
  select(algorithm,
         mibytes_per_sec_med,
         mibytes_per_sec_iqd,
         instructions_per_byte_val,
         instructions_per_cycle_val,
         branches_per_byte_val,
         branch_miss_percentage_val
  ) %>%
  mutate(
    mibytes_per_sec_med = tex_format_number(round(mibytes_per_sec_med)),  # Round to integer
    mibytes_per_sec_iqd = tex_format_number(round(mibytes_per_sec_iqd)), # Round to integer
    instructions_per_byte_val = tex_format_number(round(instructions_per_byte_val, digits=2)),
    instructions_per_cycle_val = tex_format_number(round(instructions_per_cycle_val, digits=2)),
    branches_per_byte_val = tex_format_number(round(branches_per_byte_val, digits=2)),
    branch_miss_percentage_val = tex_format_number(round(branch_miss_percentage_val, digits=2))
  )

addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- '& \\multicolumn{2}{c}{\\makecell{Throughput\\\\(MiB/s)}} & & & & \\\\
\\cmidrule(lr){2-3}
Alg. & Median & IQR & Inst./B & IPC & Br./B & BM-\\% \\\\'

print(
  xtable(t, align=c("l","l","r","r","r","r","r","r")),
  file="tab/perf_overview_random_2kib.tex", add.to.row=addtorow,include.colnames=F,floating=FALSE,
  sanitize.text.function=function(s){s}, sanitize.colnames.function=NULL)

# Plot throughput as an overview
p <- d %>%
  filter(algorithm != "FSC") %>%
  filter(event == "mibytes_per_sec") %>%
  ungroup() %>%
  mutate(algorithm = fct_reorder(algorithm, val, .desc = TRUE)) %>%
  ggplot(aes(x = algorithm, y = med)) +  # Put algorithm on x-axis and med on y-axis
  geom_bar(stat = "identity", linewidth = 0.3, fill = "darkgray", colour = "black") +
  geom_errorbar(aes(ymin = q25, ymax = q75), width = 0.5) +  # Adjust error bars to fit vertical orientation
  labs(x = NULL, y = "Throughput (MiB/s)") +  # Adjust labels
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

print_plot(p, "perf_overview_throughput_random_2kib", width=4, height=2)

rm(d,t,p,addtorow)
gc()



##########################################################################
# BFBC vs BFBC* on CODE/RAND

d <- perf_data %>%
  filter(dataset %in% c("random", "code")) %>%
  filter(algorithm %in% c("bfbc","bfbc_custom_div"))  %>%
  rename_algorithms() %>%
  rename_datasets() %>%
  filter(target_chunk_size == 2048) %>%
  group_by(algorithm, dataset, target_chunk_size,event) %>%
  summarize(dataset_size=mean(dataset_size), n=n(),
            val=mean(value),
            sd=sd(value),
            se=standard_error(value),
            mmd=mean_max_dev(value),
            max=max(value),
            med=median(value),
            q25=quantile(value, probs=0.25, names=FALSE),
            q75=quantile(value, probs=0.75, names=FALSE),
            iqd=(quantile(value, probs=0.75, names=FALSE) - quantile(value, probs=0.25, names=FALSE))
  )

t <- d %>%
  filter(event %in% c("mibytes_per_sec",
                      "instructions_per_byte",
                      "instructions_per_cycle",
                      "branches_per_byte",
                      "branch_miss_percentage")) %>%
  ungroup() %>%
  mutate(n=NULL, target_chunk_size=NULL, dataset_size = NULL) %>%
  pivot_wider(
    id_cols=c(algorithm, dataset),
    names_from=event,
    values_from=c("iqd", "val","med","se","max"),
    names_glue = "{event}_{.value}",
    names_vary="slowest") %>%
  arrange(dataset, algorithm) %>%
  select(algorithm,
         dataset,
         mibytes_per_sec_med,
         mibytes_per_sec_iqd,
         instructions_per_byte_val,
         #instructions_per_byte_se,
         instructions_per_cycle_val,
         #instructions_per_cycle_se,
         branches_per_byte_val,
         #branches_per_byte_se,
         branch_miss_percentage_val,
         #branch_miss_percentage_se
         #starts_with("mibytes_per_sec"),
         #starts_with("instructions_per_byte"),
         #starts_with("instructions_per_cycle")
  ) %>%
  mutate(
    mibytes_per_sec_med = tex_format_number(round(mibytes_per_sec_med,digits=1)),
    mibytes_per_sec_iqd = tex_format_percentage(mibytes_per_sec_iqd, digits=1, percentage_sign=FALSE),

    instructions_per_byte_val = tex_format_number(round(instructions_per_byte_val, digits=2)),
    #instructions_per_byte_se = tex_format_percentage(instructions_per_byte_se, percentage_sign=FALSE),

    instructions_per_cycle_val = tex_format_number(round(instructions_per_cycle_val, digits=2)),
    #instructions_per_cycle_se = tex_format_percentage(instructions_per_cycle_se, percentage_sign=FALSE),

    branches_per_byte_val = tex_format_number(round(branches_per_byte_val, digits=2)),
    #branches_per_byte_se = tex_format_percentage(branches_per_byte_se, percentage_sign=FALSE),

    branch_miss_percentage_val = tex_format_number(round(branch_miss_percentage_val, digits=2)),
    #branch_miss_percentage_se = tex_format_percentage(branch_miss_percentage_se, percentage_sign=FALSE),
  )

addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- '&& \\multicolumn{2}{c}{\\makecell{Throughput\\\\(MiB/s)}} & & &  & \\\\
\\cmidrule(lr){3-4}
Alg. & Dataset & \\rotatebox{90}{Median} & \\rotatebox{90}{IQR} & \\rotatebox{90}{Inst./B} & \\rotatebox{90}{IPC} & \\rotatebox{90}{Br./B} & \\rotatebox{90}{BM-\\%} \\\\'

print(
  xtable(t, align=c("l","l","l","r","r","r","r","r","r")),
  file="tab/perf_bfbc_vs_custom_bfbc_2kib.tex", add.to.row=addtorow,include.colnames=F,floating=FALSE,
  sanitize.text.function=function(s){s}, sanitize.colnames.function=NULL)




##########################################################################
# Performance on different datasets etc

d <- perf_data %>%
  filter(algorithm %in% ALGORITHMS_TO_COMPARE) %>%
  filter(event == "mibytes_per_sec") %>%
  filter(target_chunk_size %in% POWER_OF_TWO_SIZES) %>%
  rename_algorithms() %>%
  rename_datasets() %>%
  group_by(algorithm,target_chunk_size,dataset,event) %>%
  summarize(dataset_size=mean(dataset_size), n=n(),
            val=mean(value),
            sd=sd(value),
            se=standard_error(value),
            mmd=mean_max_dev(value),
            max=max(value),
            med=median(value),
            q25=quantile(value, probs=0.25, names=FALSE),
            q75=quantile(value, probs=0.75, names=FALSE),
            iqd=(quantile(value, probs=0.75, names=FALSE) - quantile(value, probs=0.25, names=FALSE))
  )

d %>%
  mutate(target_chunk_size = as.factor(target_chunk_size)) %>%
  ggplot(aes(x=target_chunk_size, y=med, ymin=q25, ymax=q75, linetype=dataset, group=dataset)) +
  geom_line() +
  geom_errorbar(width=.5) +
  facet_wrap(~algorithm, scales="free_y")






















