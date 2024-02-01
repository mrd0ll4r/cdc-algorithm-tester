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

source("base_setup.R")
source("plot_setup.R")
source("table_setup.R")
source("tikz_setup.R")
source("util.R")

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
  facet_wrap(~dataset,nrow=4,ncol=1) +
  ylab("Deduplication Ratio") +
  xlab("Target Chunk Size") +
  theme(legend.position="bottom") +
  guides(color = guide_legend(nrow = 4)) +
  scale_color_futurama()

print_plot(p,"dedup_overview_dataset_facets", height = 8)

rm(p,d)
gc()

######################################################################
# Compare different window sizes for rabin, adler, buzhash

comparison_plot <- function(df) {
  df %>%
    filter(dataset%in% c("code","web","pdf","lnx")) %>%
    filter(target_chunk_size %in% POWER_OF_TWO_SIZES) %>%
    mutate(window_size = str_split_i(algorithm,"_",-1)) %>%
    mutate(algo_group = str_split_i(algorithm,"_",1)) %>%
    ggplot(aes(x=target_chunk_size, y=dedup_ratio, color=window_size, group=window_size)) +
    geom_line(position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=1, shape=21, fill="white") +
    facet_wrap(~dataset,nrow=3,ncol=2) +
    scale_color_jama(name="Window Size (B)") +
    theme(legend.position = "bottom")
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
  filter(target_chunk_size %in% POWER_OF_TWO_SIZES) %>%
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
                   labels=c("16","32","48","64","128","256")) +
  theme(legend.position = "bottom")

print_plot(p,"dedup_comparison_window_sizes_web_algorithm_facets",height=5)

rm(p,d)
gc()

######################################################################
# Compare QuickCDC variants to plain gear-nc-1

d <- dedup_data %>%
  filter(target_chunk_size %in% POWER_OF_TWO_SIZES) %>%
  filter(!(dataset %in% c("random","zero"))) %>%
  filter(algorithm %in% QUICKCDC_RABIN_ALGORITHMS_NOSKIP | algorithm == "rabin_64") %>%
  # filter out hash variants, they should be identical to the array versions
  filter(!grepl("hash", algorithm, fixed=TRUE))

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
