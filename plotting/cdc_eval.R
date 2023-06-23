library(readr)
library(ggplot2)
library(xtable)
library(plyr)
library(dplyr)
library(tidyr)
library(scales)
library(ggsci)
library(stringr)
library(bit64)

source("base_setup.R")
source("plot_setup.R")
source("table_setup.R")
source("tikz_setup.R")

options(pillar.sigfig = 4)

# Function to calculate standard error
standard_error <- function(x) sd(x) / sqrt(length(x))

# Function to calculate mean absolute deviation
mean_abs_dev <- function(x) mean(abs(x-mean(x)))

# Function to calculate mean-min as an error metric, as per https://lemire.me/blog/2023/04/06/are-your-memory-bound-benchmarking-timings-normally-distributed/
mean_min_dev <- function(x) mean(x)-min(x)
mean_max_dev <- function(x) max(x) - mean(x)

csv_dir = "./csv"

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

algorithm_as_factor <- function(df) {
  df$algorithm <- as.factor(df$algorithm)
  df$algorithm <- recode(df$algorithm,
                                `gear_nc_1}`="gear_nc_1",
                                `gear_nc_2}`="gear_nc_2",
                                `gear_nc_3}`="gear_nc_3")
  return(df)
}

QUICKCDC_ALGORITHMS <- c("quick_2","quick_3","quick_hash_2","quick_hash_3",
                         "quick_2_rabin_64","quick_3_rabin_64","quick_hash_2_rabin_64","quick_hash_3_rabin_64")
QUICKCDC_RABIN_ALGORITHMS <- c("quick_2_rabin_64","quick_3_rabin_64","quick_hash_2_rabin_64","quick_hash_3_rabin_64")
GEAR_ALGORITHMS <- c("gear","gear_nc_1","gear_nc_2","gear_nc_3","gear64","gear64_simd")
RABIN_ALGORITHMS <- c("rabin_16","rabin_32","rabin_48","rabin_64","rabin_128","rabin_256")
BUZHASH_ALGORITHMS <- c("buzhash_16","buzhash_32","buzhash_48","buzhash_64","buzhash_128","buzhash_256")
ADLER32_ALGORITHMS <- c("adler32_16","adler32_32","adler32_48","adler32_64","adler32_128","adler32_256")
BFBC_ALGORITHMS <- c("bfbc","bfbc_custom_div")

ALGORITHMS_TO_COMPARE <- c("fsc","ae","ram","mii","pci","rabin_32","adler32_256","buzhash_64","gear","bfbc","bfbc_custom_div")

POWER_OF_TWO_SIZES = c(512,1024,2048,4096,8192)

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
  filter(algorithm %in% QUICKCDC_RABIN_ALGORITHMS | algorithm == "rabin_64") %>%
  filter(dataset != "zero" & dataset != "empty")

# Calculate some statistics...
d <- d %>% 
  group_by(algorithm,dataset,target_chunk_size,event) %>% 
  summarize(dataset_size=mean(dataset_size), n=n(), 
            val=mean(value), sd=sd(value), se=standard_error(value),
            mmd=mean_min_dev(value))

# Plot
p <- d %>%
  filter(event=="mibytes_per_sec" & target_chunk_size==2048) %>%
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
                  #breaks=c("gear_nc_1","quick_2", "quick_3", "quick_hash_2","quick_hash_3"),
                  #labels=c("Gear","A-2", "A-3","HM-2","HM-3"))
                  breaks=c("rabin_64","quick_2_rabin_64", "quick_3_rabin_64", "quick_hash_2_rabin_64","quick_hash_3_rabin_64"),
                  labels=c("Rabin","A-2", "A-3","HM-2","HM-3"))

print_plot(p,"perf_quickcdc_rabin_variants_different_datasets_2kib")

################
#- Performance of QuickCDC variants on low/high entropy datasets, table
#- Idea: What influence do design decisions (hashmap vs. flat array; front/end feature vector length) have on performance?
#  - Dataset: Random, Code/Web

d <- perf_data %>%
  filter(algorithm %in% QUICKCDC_RABIN_ALGORITHMS | algorithm == "rabin_64") %>%
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
         branch_miss_percentage_val,
         branch_miss_percentage_se
         )

addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- '&&& \\multicolumn{2}{c}{Throughput (MiB/s)} & \\multicolumn{2}{c}{Inst./B} & \\multicolumn{2}{c}{IPC} & \\multicolumn{2}{c}{L1 DCache Miss (\\%)} & \\multicolumn{2}{c}{Branch Misses (\\%)}\\\\
\\cmidrule(lr){4-13}
Algorithm & Dataset & Target CS & $\\mu$ & Max & $\\mu$ & SE & $\\mu$ & SE & $\\mu$ & SE & $\\mu$ & SE\\\\'

print(xtable(t, digits=4), file="tab/perf_quickcdc_rabin_variants.tex", add.to.row=addtorow,include.colnames=F,floating=FALSE)

rm(d,p,t,addtorow)
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
  scale_color_jama(name="", # Legend label
                   breaks=c("rabin_64","quick_2_rabin_64", "quick_3_rabin_64", "quick_hash_2_rabin_64","quick_hash_3_rabin_64"),
                   labels=c("Rabin","A-2", "A-3","HM-2","HM-3"))
                  #breaks=c("gear_nc_1","quick_2", "quick_3", "quick_hash_2","quick_hash_3"),
                  #labels=c("Gear","A-2", "A-3","HM-2","HM-3"))

print_plot(p,"perf_quickcdc_rabin_variants_different_targets_code")

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

rm(d,t,addtorow)
gc()


######################################################################
# Performance overview
d <- perf_data %>%
  filter(dataset == "random" | dataset=="code") %>%
  filter(algorithm %in% ALGORITHMS_TO_COMPARE) %>%
  filter(target_chunk_size == 2048) %>%
  group_by(algorithm,dataset,target_chunk_size,event) %>% 
  summarize(dataset_size=mean(dataset_size), n=n(), 
            val=mean(value),
            sd=sd(value),
            se=standard_error(value),
            mmd=mean_max_dev(value),
            max=max(value))


t <- d %>%
  filter(event %in% c("mibytes_per_sec",
                      "instructions_per_byte",
                      "instructions_per_cycle",
                      "branches_per_byte")) %>%
  ungroup() %>%
  mutate(n=NULL, target_chunk_size=NULL,dataset_size = NULL) %>%
  pivot_wider(id_cols=c(algorithm,dataset),names_from=event,values_from=c("val","se","max"),names_glue = "{event}_{.value}",names_vary="slowest") %>%
  arrange(dataset,algorithm) %>%
  select(algorithm,dataset,
         mibytes_per_sec_val,
         mibytes_per_sec_max,
         instructions_per_byte_val,
         instructions_per_byte_se,
         instructions_per_cycle_val,
         instructions_per_cycle_se,
         branches_per_byte_val,
         branches_per_byte_se
         #starts_with("mibytes_per_sec"),
         #starts_with("instructions_per_byte"),
         #starts_with("instructions_per_cycle")
         )

addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- '&& \\multicolumn{2}{c}{Throughput (MiB/s)} & \\multicolumn{2}{c}{Inst./B} & \\multicolumn{2}{c}{IPC} & \\multicolumn{2}{c}{Branches/B}\\\\
\\cmidrule(lr){3-10}
Algorithm & Dataset & $\\mu$ & Max & $\\mu$ & SE (±) & $\\mu$ & SE (±) & $\\mu$ & SE (±)\\\\'

print(xtable(t, digits=3), file="tab/perf_overview_random_code_2kib.tex", add.to.row=addtorow,include.colnames=F,floating=FALSE)


######################################################################
# DEDUPLICATION RATIO
######################################################################

dedup_data <- tibble()
infiles <- Sys.glob(sprintf("%s/dedup_*",csv_dir))
for (f in infiles) {
  tmp <- read_csv(f,col_types = "fcIiI")
  
  if (isempty(dedup_data)) {
    dedup_data <- tmp
  } else {
    dedup_data <- rows_append(dedup_data,tmp)
  }
  rm(tmp)
}

dedup_data <- dedup_data %>%
  algorithm_as_factor() %>%
  mutate(unique_ratio=unique_chunks_size_sum/dataset_size) %>%
  mutate(dedup_ratio=1-unique_ratio) %>%
  mutate(target_chunk_size = as.factor(target_chunk_size))
######################################################################

d <- dedup_data %>%
  filter(algorithm %in% ALGORITHMS_TO_COMPARE)

p <- d %>%
  filter(dataset %in% c("code","web","pdf","lnx")) %>%
  ggplot(aes(x=target_chunk_size, y=dedup_ratio, color=algorithm, group=algorithm)) +
  geom_line(position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=1, shape=21, fill="white") +
  facet_wrap(~dataset,nrow=2,ncol=2) +
  scale_color_futurama() #name="Supplement type", # Legend label, use darker colors
#breaks=c("OJ", "VC"),
#labels=c("Orange juice", "Ascorbic acid"))

print_plot(p,"dedup_overview_dataset_facets", height = 4)

rm(p,d)
gc()

######################################################################
# Compare different window sizes for rabin, adler, buzhash

comparison_plot <- function(df) {
  df %>%
    filter(dataset%in% c("code","web","pdf","lnx")) %>%
    mutate(window_size = str_split_i(algorithm,"_",-1)) %>%
    mutate(algo_group = str_split_i(algorithm,"_",1)) %>%
    ggplot(aes(x=target_chunk_size, y=dedup_ratio, color=window_size, group=window_size)) +
    geom_line(position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=1, shape=21, fill="white") +
    facet_wrap(~dataset,nrow=3,ncol=2) + 
    scale_color_jama(name="Window Size (B)")
}

p <- dedup_data %>%
  filter(algorithm %in% RABIN_ALGORITHMS) %>%
  comparison_plot()

print_plot(p,"dedup_rabin_window_sizes_dataset_facets",height=4)

p <- dedup_data %>%
  filter(algorithm %in% BUZHASH_ALGORITHMS) %>%
  comparison_plot()

print_plot(p,"dedup_buzhash_window_sizes_dataset_facets",height=4)

p <- dedup_data %>%
  filter(algorithm %in% ADLER32_ALGORITHMS) %>%
  comparison_plot()

print_plot(p,"dedup_adler32_window_sizes_dataset_facets",height=4)

####################
# Overview: Evaluate on WEB, facet wrap by algorithm group

p <- dedup_data %>%
  filter(dataset=="web") %>%
  filter(algorithm %in% RABIN_ALGORITHMS |
           algorithm %in% BUZHASH_ALGORITHMS |
           algorithm %in% ADLER32_ALGORITHMS) %>%
  mutate(window_size = str_split_i(algorithm,"_",-1)) %>%
  mutate(algo_group = str_split_i(algorithm,"_",1)) %>%
  ggplot(aes(x=target_chunk_size, y=dedup_ratio, color=window_size, group=window_size)) +
  geom_line(position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=1, shape=21, fill="white") +
  labs(x="Target Chunk Size (B)",y="Deduplication Ratio") +
  facet_wrap(~algo_group,nrow=3,ncol=1) +
  scale_color_jama(name="Window Size (B)", # Legend label
    breaks=c("16", "32", "48", "64", "128", "256"),
    labels=c("16","32","48","64","128","256"))

print_plot(p,"dedup_comparison_window_sizes_web_algorithm_facets",height=5)

rm(p,d)
gc()

######################################################################
# Compare QuickCDC variants to plain gear-nc-1

d <- dedup_data %>%
  filter(algorithm %in% QUICKCDC_RABIN_ALGORITHMS | algorithm == "rabin_64")

# Create table
t <- d %>% 
  pivot_wider(id_cols=c(algorithm,dataset),names_from=target_chunk_size,values_from=dedup_ratio)

addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- '&& \\multicolumn{5}{c}{Deduplication Ratio}\\\\
\\cmidrule(lr){3-7}
algorithm & dataset & CS=512B & 1KiB & 2KiB & 4KiB & 8KiB\\\\'

print(xtable(t, digits=6), file="tab/dedup_quickcdc_variants_all_datasets.tex", add.to.row=addtorow,include.colnames=F,floating=FALSE)

rm(t,d,addtorow)
gc()

######################################################################
# HASH VALUE DISTRIBUTION
######################################################################

# Problem: For some hash functions, this is 64 bits, i.e., col_type I.
# Unfortunately, ggplot doesn't know how to deal with that.
# We could read it in as double, which kinda works, but also probably loses some
# precision at the higher values.
# To be specific, anything larger than 2^53 is less precise than an integer.
# I'm not sure this is a problem, though: Even if different integers get mapped
# to the same double, as long as the buckets are large enough, this should not
# distort the results.
#
# In order to deal with the different codomains of the various algorithms, we
# bin in 100 bins over their respective codomain, and then plot those bins.

load_hash_value_distribution <- function(filename) {
  algorithm_and_winsize = strsplit(strsplit(filename,c("hash_values_rand_small_"))[[1]][2], c(".csv.gz"))[[1]][1]
  algorithm = strsplit(algorithm_and_winsize, c("_"))[[1]][1]
  window_size = strsplit(algorithm_and_winsize, c("_"))[[1]][2]
  read_csv(sprintf("%s/%s", csv_dir, filename), col_types="d") %>%
    mutate(algorithm=algorithm,
           window_size=window_size,
           algorithm_and_winsize=algorithm_and_winsize)
}

load_hash_value_distribution_for_algorithm <- function(algorithm) {
  d <- tibble()
  infiles <- Sys.glob(sprintf("%s/hash_values_rand_small_%s_*", csv_dir, algorithm))
  for (f in infiles) {
    f = strsplit(f,c(sprintf("%s/",csv_dir)))[[1]][2]
    print(sprintf("loading %s...",f))
    tmp = load_hash_value_distribution(f)

    if (isempty(d)) {
      d <- tmp
    } else {
      d <- rows_append(d,tmp)
    }

    rm(tmp)
  }
  d <- d %>%
    mutate(algorithm=as.factor(algorithm),
           window_size=as.factor(window_size),
           algorithm_and_winsize=as.factor(algorithm_and_winsize))

  return(d)
}

bin_normalize_hash_value_distribution <- function(d, bins=100, codomain_min=0, codomain_max=2^32) {
  d %>%
    group_by(algorithm) %>%
    mutate(g=cut(hash_value,breaks=(codomain_max/bins)*seq(0,bins), labels=FALSE)) %>%
    group_by(algorithm,window_size,algorithm_and_winsize, g) %>%
    summarize(
      bucket_min=min(hash_value),
      bucket_max=max(hash_value),
      n=n()) %>%
    group_by(algorithm,window_size,algorithm_and_winsize) %>%
    mutate(
      min_hash_value=min(bucket_min),
      max_hash_value=max(bucket_max),
      n_norm=(n/sum(n))*bins,
      g_norm=g/bins,
      codomain_min=codomain_min,
      codomain_max=codomain_max)
}

HASH_VALUE_DISTRIBUTION_ALGORITHMS_TO_COMPARE = c("adler32_256","buzhash_64","rabin_32","gear64_64")

hash_value_distribution_single_algorithm_window_size_comparison_plot <- function(d, window_sizes=c("16","32","48","64","128","256")) {
  codomain_exponent = log(max(d$codomain_max), base=2)

  d %>%
    ggplot(aes(x=g_norm, y=n_norm, linetype=window_size, fill=window_size)) +
    geom_line(position=position_jitter(width=0,height=0.0025)) +
    geom_area(alpha=0.1,position="identity") +
    labs(
      x=sprintf("Normalized Codomain (*2^%d)", codomain_exponent),
      y="Density") +
    scale_fill_jama(name="Window Size (B)", # Legend label
                    breaks=c("16","32","48","64","128","256"),
                    labels=c("16","32","48","64","128","256")) +
    scale_linetype_discrete(name="Window Size (B)",
                            breaks=c("16","32","48","64","128","256"),
                            labels=c("16","32","48","64","128","256"))
}

hash_value_distribution_adler32_binned <- load_hash_value_distribution_for_algorithm("adler32") %>%
  bin_normalize_hash_value_distribution(bins=100, codomain_min=0, codomain_max=2^32)
gc()

hash_value_distribution_buzhash_binned <- load_hash_value_distribution_for_algorithm("buzhash") %>%
  bin_normalize_hash_value_distribution(bins=100, codomain_min=0, codomain_max=2^32)
gc()

hash_value_distribution_rabin_binned <- load_hash_value_distribution_for_algorithm("rabin") %>%
  bin_normalize_hash_value_distribution(bins=100, codomain_min=0, codomain_max=2^53)
gc()

hash_value_distribution_gear64_binned <- load_hash_value_distribution_for_algorithm("gear64") %>%
  bin_normalize_hash_value_distribution(bins=100, codomain_min=0, codomain_max=2^64)
gc()

hash_value_distribution_data_binned <- rbind(
  hash_value_distribution_adler32_binned,
  hash_value_distribution_buzhash_binned,
  hash_value_distribution_rabin_binned,
  hash_value_distribution_gear64_binned
)

p <- hash_value_distribution_data_binned %>%
  filter(algorithm == "adler32") %>%
  hash_value_distribution_single_algorithm_window_size_comparison_plot()
print_plot(p, "hash_value_distribution_adler32")

p <- hash_value_distribution_data_binned %>%
  filter(algorithm == "buzhash") %>%
  hash_value_distribution_single_algorithm_window_size_comparison_plot()
print_plot(p, "hash_value_distribution_buzhash")

p <- hash_value_distribution_data_binned %>%
  filter(algorithm == "rabin") %>%
  hash_value_distribution_single_algorithm_window_size_comparison_plot()
print_plot(p, "hash_value_distribution_rabin")

p <- hash_value_distribution_data_binned %>%
  filter(algorithm == "gear64") %>%
  hash_value_distribution_single_algorithm_window_size_comparison_plot()
print_plot(p, "hash_value_distribution_gear64")

p <- hash_value_distribution_data_binned %>%
  filter(algorithm_and_winsize %in% HASH_VALUE_DISTRIBUTION_ALGORITHMS_TO_COMPARE) %>%
  ggplot(aes(x=g_norm, y=n_norm, linetype=algorithm_and_winsize, fill=algorithm_and_winsize)) +
  geom_line(position=position_jitter(width=0,height=0.005)) +
  geom_area(alpha=0.1,position="identity") +
  labs(x="Normalized Codomain", y="Density") +
  scale_fill_jama(name="Algorithm"#, # Legend label
                  #breaks=c("16","32","48","64","128","256"),
                  #labels=c("16","32","48","64","128","256")
                  ) +
  scale_linetype_discrete(name="Algorithm"#,
                          #breaks=c("16","32","48","64","128","256"),
                          #labels=c("16","32","48","64","128","256")
                          )
print_plot(p, "hash_value_distribution_selection")


######################################################################
# FILE SIZES IN DATASETS
######################################################################

sizes_data_code <- scan(sprintf("%s/sizes_code.txt",csv_dir), what=numeric(), sep=",")
sizes_data_web <- scan(sprintf("%s/sizes_web.txt",csv_dir), what=numeric(), sep=",")
sizes_data_pdf <- scan(sprintf("%s/sizes_pdf.txt",csv_dir), what=numeric(), sep=",")
sizes_data_lnx <- scan(sprintf("%s/sizes_lnx.txt",csv_dir), what=numeric(), sep=",")

# Calculate the CDFs
cdf_func_code <- ecdf(sizes_data_code)
cdf_func_web <- ecdf(sizes_data_web)
cdf_func_pdf <- ecdf(sizes_data_pdf)
cdf_func_lnx <- ecdf(sizes_data_lnx)

# Create data frames with the unique values and their corresponding CDF values
cdf_data_code <- data.frame(values = unique(sizes_data_code), cdf_values = cdf_func_code(unique(sizes_data_code)), category = "code")
cdf_data_web <- data.frame(values = unique(sizes_data_web), cdf_values = cdf_func_web(unique(sizes_data_web)), category = "web")
cdf_data_pdf <- data.frame(values = unique(sizes_data_pdf), cdf_values = cdf_func_pdf(unique(sizes_data_pdf)), category = "pdf")
cdf_data_lnx <- data.frame(values = unique(sizes_data_lnx), cdf_values = cdf_func_lnx(unique(sizes_data_lnx)), category = "lnx")

# Combine data frames
cdf_data <- rbind(cdf_data_code, cdf_data_web, cdf_data_pdf, cdf_data_lnx)

# Define the breaks and labels
breaks <- c(512, 8*1024, 16*8*1024, 16*16*8*1024, 16*16*16*8*1024, 16*16*16*16*8*1024)
labels <- c("512 B", "8 KB", "128 KB", "2 MB", "32 MB", "512 MB")

# Create the plot with log scale on x-axis
p <- ggplot(cdf_data, aes(x = values, y = cdf_values, color = category)) +
  geom_line() +
  scale_x_log10(breaks = breaks, labels = labels) +
  geom_vline(xintercept = 512, linetype = "dashed") +
  xlab("File Size (Log Scale)") +
  ylab("Cumulative Probability") +
  labs(color = "Dataset")

print_plot(p,"dataset_file_sizes")

gc()


######################################################################
# CHUNK SIZE DISTRIBUTIONS
######################################################################
# Note: The complete CSD dataset is too large to be stored in one R vector.
# Because of that, we load and store them separately.

csd_data_random <- read_csv(sprintf("%s/csd_random.csv.gz",csv_dir),col_types = "fciI")
csd_data_code <- read_csv(sprintf("%s/csd_code.csv.gz",csv_dir),col_types = "fciI")
csd_data_web <- read_csv(sprintf("%s/csd_web.csv.gz",csv_dir),col_types = "fciI")
csd_data_pdf <- read_csv(sprintf("%s/csd_pdf.csv.gz",csv_dir),col_types = "fciI")
csd_data_lnx <- read_csv(sprintf("%s/csd_lnx.csv.gz",csv_dir),col_types = "fciI")
csd_data_zero <- read_csv(sprintf("%s/csd_zero.csv.gz",csv_dir),col_types = "fciI")

csd_data_random <- algorithm_as_factor(csd_data_random)
csd_data_code <- algorithm_as_factor(csd_data_code)
csd_data_web <- algorithm_as_factor(csd_data_web)
csd_data_pdf <- algorithm_as_factor(csd_data_pdf)
csd_data_lnx <- algorithm_as_factor(csd_data_lnx)
csd_data_zero <- algorithm_as_factor(csd_data_zero)

csd_data_full <- list(csd_data_random, csd_data_code, csd_data_web, csd_data_pdf, csd_data_lnx, csd_data_zero)

#csd_data <- tibble()
#infiles <- Sys.glob(sprintf("%s/csd_*",csv_dir))
#for (f in infiles) {
#  print(sprintf("reading %s...",f))
#  tmp <- read_csv(f,col_types = "fciI")
#  
#  if (isempty(csd_data)) {
#    csd_data <- tmp
#  } else {
#    csd_data <- rows_append(csd_data,tmp)
#  }
#  rm(tmp)
#}
#csd_data <- algorithm_as_factor(csd_data)
gc()


csd_density_plot <- function(df,spread=3) {
  target_cs <- mean(df$target_chunk_size)
  return(
    df %>%
      # Not sure how well ggplot handles bit64, so convert to double...
      mutate(chunk_size = as.double(chunk_size)) %>%
      # Filter out the tails, otherwise geom_density complains
      # (not 100% sure why, I think it's because the distributions are so different, or maybe because of the heavy tails.)
      # Or maybe it's because of the x-limits?
      filter(chunk_size >= target_cs*2^(spread*-1) & chunk_size <= target_cs*2^spread) %>%
      ggplot(aes(x=chunk_size,fill=algorithm)) +
      geom_density(alpha=0.3,n=1024,bounds=c(0,Inf)) +
      labs(x = "Chunk Size (B)", y = "Density") +
      scale_x_continuous(
        trans="log2",
        breaks = target_cs*2^seq(-2,2),
        labels = trans_format("log2", math_format(2 ^ .x)),
        limits = c(target_cs*2^(spread*-1),target_cs*2^spread)
      )
  )
}

csd_density_plot2 <- function(df,spread=3) {
  target_cs <- mean(df$target_chunk_size)
  return(
    df %>%
      # Not sure how well ggplot handles bit64, so convert to double...
      mutate(chunk_size = as.double(chunk_size)) %>%
      # Filter out the tails, otherwise geom_density complains
      # (not 100% sure why, I think it's because the distributions are so different, or maybe because of the heavy tails.)
      # Or maybe it's because of the x-limits?
      #chunk_size >= target_cs*2^(spread*-1) &
      filter( chunk_size <= target_cs*2^spread) %>%
      ggplot(aes(x=chunk_size,fill=algorithm)) +
      geom_density(alpha=0.3,n=1024,bounds=c(0,Inf)) +
      labs(x = "Chunk Size (B)", y = "Density") +
      coord_trans(x="log2") +
      scale_x_continuous(
        limits = c(target_cs*2^(spread*-1),target_cs*2^spread),
        breaks = target_cs*2^seq(-2,2),
        labels = trans_format("log2", math_format(2 ^ .x))
      )
  )
}

######################################################################
# Huge table about means

d <- bind_rows(lapply(csd_data_full, function(df) {
  df %>%
    group_by(algorithm, dataset, target_chunk_size) %>%
    summarize(n=n(), mean=mean(chunk_size), sd=sd(chunk_size)) %>%
    ungroup()
}))

# Create Table
t <- d %>% 
  # xtable cannot handle int64
  mutate(n=NULL,mean=as.integer(mean)) %>% 
  pivot_wider(id_cols = c("algorithm","dataset"),names_from=target_chunk_size,values_from=c("mean","sd"),names_vary = "slowest")

addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- '&& \\multicolumn{2}{c}{512B} &\\multicolumn{2}{c}{1KiB} &\\multicolumn{2}{c}{2KiB} &\\multicolumn{2}{c}{4KiB} &\\multicolumn{2}{c}{8KiB}\\\\
\\cmidrule(lr){3-4}\\cmidrule(lr){5-6}\\cmidrule(lr){7-8}\\cmidrule(lr){9-10}\\cmidrule(lr){11-12}
algorithm & dataset & $\\mu$ & SD & $\\mu$ & SD & $\\mu$ & SD & $\\mu$ & SD & $\\mu$ & SD\\\\'

print(xtable(t), file="tab/csd_means_sd_full.tex", add.to.row=addtorow,include.colnames=F,floating=FALSE)

#### QuickCDC variants only
t2 <- t %>%
  filter(algorithm %in% QUICKCDC_ALGORITHMS | algorithm == "gear_nc_1")

print(xtable(t), file="tab/csd_means_sd_quickcdc.tex", add.to.row=addtorow,include.colnames=F,floating=FALSE)

#### Rabin variants only
t2 <- t %>%
  filter(algorithm %in% RABIN_ALGORITHMS)

print(xtable(t2), file="tab/csd_means_sd_rabin.tex", add.to.row=addtorow,include.colnames=F,floating=FALSE)

#### Buzhash variants only
t2 <- t %>%
  filter(algorithm %in% BUZHASH_ALGORITHMS)

print(xtable(t2), file="tab/csd_means_sd_buzhash.tex", add.to.row=addtorow,include.colnames=F,floating=FALSE)

#### Adler32 variants only
t2 <- t %>%
  filter(algorithm %in% ADLER32_ALGORITHMS)

print(xtable(t2), file="tab/csd_means_sd_adler32.tex", add.to.row=addtorow,include.colnames=F,floating=FALSE)

#### Gear variants only
t2 <- t %>%
  filter(algorithm %in% GEAR_ALGORITHMS)

print(xtable(t2), file="tab/csd_means_sd_gear.tex", add.to.row=addtorow,include.colnames=F,floating=FALSE)

#### BFBC variants only
t2 <- t %>%
  filter(algorithm %in% BFBC_ALGORITHMS)

print(xtable(t2), file="tab/csd_means_sd_bfbc.tex", add.to.row=addtorow,include.colnames=F,floating=FALSE)

rm(t,t2,d,addtorow)
gc()

######################################################################
# Compare NC-gear variants

d <- csd_data %>%
  filter(algorithm %in% c("gear","gear_nc_1","gear_nc_2","gear_nc_3"))
eval_target_cs<-4096
eval_dataset<-"random"

###############
# Dataset eval_dataset, eval_target_cs target
p <- d  %>%
  filter(dataset==eval_dataset) %>%
  filter(target_chunk_size == eval_target_cs) %>%
  csd_density_plot()
print_plot(p,sprintf("csd_gear_nc_%s_%d_v1",eval_dataset,eval_target_cs))

p <- d  %>%
  filter(dataset==eval_dataset) %>%
  filter(target_chunk_size == eval_target_cs)  %>%
  csd_density_plot2(spread=3)

print_plot(p,sprintf("csd_gear_nc_%s_%d_v2",eval_dataset,eval_target_cs))

###############
# All Datasets, eval_target_cs target
p <- d %>%
  filter(target_chunk_size==eval_target_cs) %>%
  csd_density_plot()+
  facet_wrap(~dataset,dir="v")

print_plot(p,sprintf("csd_gear_nc_dataset_faceted_%d_v1",eval_target_cs),height=8)

p <- d %>%
  filter(target_chunk_size==eval_target_cs) %>%
  csd_density_plot2()+
  facet_wrap(~dataset,dir="v")

print_plot(p,sprintf("csd_gear_nc_dataset_faceted_%d_v2",eval_target_cs),height=8)

###############
# Dataset eval_dataset, all targets

p <- d %>%
  filter(dataset==eval_dataset) %>%
  csd_density_plot()+
  facet_wrap(~target_chunk_size,dir="v")

print_plot(p,sprintf("csd_gear_nc_%s_target_faceted_v1",eval_dataset),height=8)

p <- d %>%
  filter(dataset==eval_dataset) %>%
  csd_density_plot2()+
  facet_wrap(~target_chunk_size,dir="v")

print_plot(p,sprintf("csd_gear_nc_%s_target_faceted_v2",eval_dataset),height=8)

rm(d,p,eval_target_cs,eval_dataset)
gc()


######################################################################
# Compare Rabin variants

d <- csd_data %>%
  filter(algorithm %in% RABIN_ALGORITHMS)
eval_target_cs <- 4096
eval_dataset <- "random"

###############
# Dataset eval_dataset, eval_target_cs target
p <- d  %>%
  filter(dataset==eval_dataset) %>%
  filter(target_chunk_size == eval_target_cs) %>%
  csd_density_plot()
print_plot(p,sprintf("csd_rabin_%s_%d_v1",eval_dataset,eval_target_cs))

p <- d  %>%
  filter(dataset==eval_dataset) %>%
  filter(target_chunk_size == eval_target_cs)  %>%
  csd_density_plot2(spread=3)

print_plot(p,sprintf("csd_rabin_%s_%d_v2",eval_dataset,eval_target_cs))

###############
# All Datasets, eval_target_cs target
p <- d %>%
  filter(target_chunk_size==eval_target_cs) %>%
  csd_density_plot()+
  facet_wrap(~dataset,dir="v")

print_plot(p,sprintf("csd_rabin_dataset_faceted_%d_v1",eval_target_cs),height=8)

p <- d %>%
  filter(target_chunk_size==eval_target_cs) %>%
  csd_density_plot2()+
  facet_wrap(~dataset,dir="v")

print_plot(p,sprintf("csd_rabin_dataset_faceted_%d_v2",eval_target_cs),height=8)

###############
# Dataset eval_dataset, all targets

p <- d %>%
  filter(dataset==eval_dataset) %>%
  csd_density_plot()+
  facet_wrap(~target_chunk_size,dir="v")

print_plot(p,sprintf("csd_rabin_%s_target_faceted_v1",eval_dataset),height=8)

p <- d %>%
  filter(dataset==eval_dataset) %>%
  csd_density_plot2()+
  facet_wrap(~target_chunk_size,dir="v")

print_plot(p,sprintf("csd_rabin_%s_target_faceted_v2",eval_dataset),height=8)

rm(d,p)
gc()





#####################
q()


dd <- d %>% filter(target_chunk_size==2048)
ggplot(data = dd, aes(x=chunk_size,y=after_stat(density),group=algorithm,color=algorithm)) +
  geom_freqpoly(binwidth=128) +
  labs(x = "Chunk Size (B)", y = "Density")+
  xlim(128,8192) +
  coord_trans(x="log2") +
  scale_x_continuous(
    breaks = (2048/4)*2^seq(0,4),
    labels = trans_format("log2", math_format(2 ^ .x))
  )
  scale_x_continuous(
    trans="log2",
    breaks = (2048/4)*2^seq(0,4),
    labels = trans_format("log2", math_format(2 ^ .x))
  )

ggplot(data=dd %>% mutate(chunk_size = as.double(chunk_size)), aes(x=chunk_size,group=algorithm,color=algorithm,fill=algorithm)) +
  geom_histogram() +
  xlim(128,8192) +
  coord_trans(x="log2")



##########################################
d %>% 
  filter(dataset=="random") %>%
  filter(target_chunk_size==4096) %>%
  mutate(chunk_size = as.double(chunk_size)) %>%
  ggplot(aes(x=chunk_size,color=algorithm)) +
  stat_ecdf() +
  labs(x = "Chunk Size (B)", y = "ECDF") +
  scale_x_continuous(
    trans = "log2",
    breaks = (4096/4)*2^seq(0,4),
    labels = trans_format("log2", math_format(2 ^ .x))
  )



##########################################
df %>%
  # Not sure how well ggplot handles bit64, so convert to double...
  mutate(chunk_size = as.double(chunk_size)) %>%
  # Filter out the tails, otherwise geom_density complains (not 100% sure why, I think it's because the distributions are so different)
  filter(chunk_size > target_cs*2^(spread*-1) & chunk_size < target_cs*2^spread) %>%
  ggplot(aes(x=chunk_size,group=algorithm,fill=algorithm)) +
  geom_density(alpha = 0.3,
               trim=TRUE,
               bounds =c(1,Inf)) +
  labs(x = "Chunk Size (B)", y = "Density") +
  scale_x_continuous(
    trans="log2",
    breaks = target_cs*2^seq(-2,2),
    labels = trans_format("log2", math_format(2 ^ .x)),
    limits = c(target_cs*2^(spread*-1),target_cs*2^spread)
  )

##########################################
d %>% 
  filter(dataset=="random") %>%
  filter(target_chunk_size==4096) %>%
  mutate(chunk_size = as.double(chunk_size)) %>%
  #filter(chunk_size >= 128 & chunk_size <= 8192*4) %>%
  ggplot(aes(x=chunk_size,fill=algorithm)) +
  geom_density(alpha=0.3,n=1024,bounds=c(0,Inf)) +
  coord_trans(x="log2") +
  scale_x_continuous(
    limits=c(128,8192*4),
    breaks = 2048*2^seq(-2,2),
    labels = trans_format("log2", math_format(2 ^ .x)),
  )

##########################################
dd %>% mutate(chunk_size = as.double(chunk_size)) %>%
  ggplot(aes(x=chunk_size,y=after_stat(density),fill=algorithm,color=algorithm)) +
  geom_freqpoly(binwidth = 128) +
  coord_trans(x="log2") +
  scale_x_continuous(
    limits=c(128,8192*4),
    breaks = 2048*2^seq(-2,2),
    labels = trans_format("log2", math_format(2 ^ .x)),
  )

##########################################
dd %>% mutate(chunk_size = as.double(chunk_size)) %>%
  ggplot(aes(x=chunk_size,y=after_stat(density),fill=algorithm,color=algorithm)) +
  geom_freqpoly(bins=128,position="jitter") +
  scale_x_continuous(
    trans="log2",
    limits=c(128,8192*4),
    breaks = 2048*2^seq(-2,2),
    labels = trans_format("log2", math_format(2 ^ .x)),
  )

##########################################
csd_data_random %>% 
  filter(algorithm %in% c("gear","gear_nc_1","gear_nc_2","gear_nc_3")) %>%
  filter(dataset=="random") %>%
  filter(target_chunk_size==2048) %>%
  mutate(chunk_size = as.double(chunk_size)) %>%
  ggplot(aes(x=chunk_size,y=after_stat(ndensity),fill=algorithm)) +
  geom_density(alpha=0.3,n=1024,bounds=c(0,Inf)) +
  #geom_histogram(bins=2048,position="dodge") +
  #geom_freqpoly(bins=1024)+
  scale_x_continuous(
    trans=pseudo_log_trans(base=2),
    limits=c(128,8192*4),
    breaks = 2048*2^seq(-2,2),
    labels = trans_format("log2", math_format(2 ^ .x)),
  )

csd_data_random %>% 
  filter(algorithm %in% c("gear","gear_nc_1","gear_nc_2","gear_nc_3")) %>%
  filter(dataset=="random") %>%
  filter(target_chunk_size==2048) %>%
  mutate(chunk_size = as.double(chunk_size)) %>%
  filter(chunk_size >= 0 & chunk_size <= 2048*32) %>%
  ggplot(aes(x=chunk_size,y=after_stat(count),fill=algorithm,)) +
  #geom_density(alpha=0.3,n=2048,bounds=c(32,Inf)) +
  #geom_histogram(binwidth=32,position="dodge",alpha=0.2,boundary=0) +
  geom_area(stat="bin",breaks=unique(c(seq(12,128,8),seq(128,256,16),seq(256,2048*32,32))),alpha=0.3,position = "identity") +
  geom_freqpoly(breaks=unique(c(seq(12,128,8),seq(128,256,16),seq(256,2048*32,32))),color="black") +
  coord_trans(x=pseudo_log_trans(base=2)) +
  scale_x_continuous(
    limits=c(16,2048*32),
    breaks = 2048*2^seq(-7,2),
    labels = trans_format("log2", math_format(2 ^ .x)),
  )

csd_data_random %>% 
  filter(algorithm %in% c("gear","gear_nc_1","gear_nc_2","gear_nc_3")) %>%
  filter(dataset=="random") %>%
  filter(target_chunk_size==2048) %>%
  mutate(chunk_size = as.double(chunk_size)) %>%
  filter(chunk_size >= 0 & chunk_size <= 2048*32) %>%
  ggplot(aes(x=chunk_size,fill=algorithm,)) +
  #geom_density(alpha=0.3,n=2048,bounds=c(32,Inf)) +
  #geom_histogram(binwidth=32,position="dodge",alpha=0.2,boundary=0) +
  #geom_area(stat="bin",breaks=unique(c(seq(12,128,8),seq(128,256,16),seq(256,2048*32,32))),alpha=0.3,position = "identity") +
  #geom_freqpoly(breaks=unique(c(seq(12,128,8),seq(128,256,16),seq(256,2048*32,32))),color="black") +
  geom_bar(position="dodge") +
  coord_trans(x=pseudo_log_trans(base=2)) +
  scale_x_binned(
    limits=c(8,2048*32),
    breaks = 2048*2^seq(-7,4),
    labels = trans_format("log2", math_format(2 ^ .x)),
  )


csd_data_random %>% 
  filter(algorithm %in% c("gear","gear_nc_1","gear_nc_2","gear_nc_3")) %>%
  filter(dataset=="random") %>%
  filter(target_chunk_size==2048) %>%
  mutate(chunk_size = as.double(chunk_size)) %>%
  filter(chunk_size >= 0 & chunk_size <= 2048*32) %>%
  ggplot(aes(x=chunk_size,y=after_stat(ncount),fill=algorithm,)) +
  #geom_density(alpha=0.3,n=2048,bounds=c(32,Inf)) +
  #geom_histogram(binwidth=32,position="dodge",alpha=0.2,boundary=0) +
  geom_area(stat="bin",bins=256,alpha=0.3,position = "identity") +
  geom_freqpoly(bins=256,color="black") +
  scale_x_continuous(
    trans=pseudo_log_trans(base=2),
    limits=c(16,2048*32),
    breaks = 2048*2^seq(-7,2),
    labels = trans_format("log2", math_format(2 ^ .x)),
  )


csd_data_random %>% 
  filter(algorithm %in% c("gear","gear_nc_1","gear_nc_2","gear_nc_3")) %>%
  filter(dataset=="random") %>%
  filter(target_chunk_size==2048) %>%
  filter(chunk_size >= 2^11 & chunk_size < 2^12) %>%
  group_by(algorithm) %>%
  summarize(n=n())

csd_data_random %>% 
  filter(algorithm %in% c("gear","gear_nc_1","gear_nc_2","gear_nc_3")) %>%
  filter(dataset=="random") %>%
  filter(target_chunk_size==2048) %>%
  summarize(n=n())


##########################################
xlim(1,8192) +
  coord_trans(x="log2")


ggplot(data = dd %>% mutate(chunk_size = as.double(chunk_size)), aes(x=chunk_size,y=after_stat(density),group=algorithm,color=algorithm)) +
  geom_freqpoly(binwidth=128,boundary=0) +
  labs(x = "Chunk Size (B)", y = "Density")+
  scale_x_continuous(
    trans="log2",
    breaks = (2048/4)*2^seq(0,4),
    labels = trans_format("log2", math_format(2 ^ .x)),
    limits = c(512,8192*2)
  )

xlim(1,8192) +



  coord_trans(x="log2") +
  scale_x_continuous(
    breaks = (2048/4)*2^seq(0,4),
    labels = trans_format("log2", math_format(2 ^ .x))
  )
scale_x_continuous(
  trans="log2",
  breaks = (2048/4)*2^seq(0,4),
  labels = trans_format("log2", math_format(2 ^ .x))
)











chunk_size_freqpoly_density_log <- function(d,target_chunk_size=2^9) {
  p <- ggplot(data = d, aes(x=chunk_size,y=after_stat(density),color=algorithm)) +
    geom_freqpoly(bins=100) +
    labs(x = "Chunk Size (B)", y = "Density") +
    scale_x_continuous(
      trans="log2",
      breaks = (target_chunk_size/4)*2^seq(0,4),
      labels = trans_format("log2", math_format(2 ^ .x))
    )
  return(p)
}


chunk_size_ecdf_log <- function(d,target_chunk_size=2^9) {
  p <- ggplot(data = d, aes(x=chunk_size,color=algorithm)) +
    stat_ecdf() +
    labs(x = "Chunk Size (B)", y = "ECD") +
    scale_x_continuous(
      trans = "log2",
      breaks = (target_chunk_size/4)*2^seq(0,4),
      labels = trans_format("log2", math_format(2 ^ .x))
    )
  return(p)
}







