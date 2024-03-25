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
infiles <- Sys.glob(sprintf("%s/dedup_*_cat*",csv_dir))
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
  filter(dataset %in% c("code","web","pdf","lnx")) %>%
  algorithm_as_factor() %>%
  mutate(unique_ratio=unique_chunks_size_sum/dataset_size) %>%
  mutate(dedup_ratio=1-unique_ratio) %>%
  mutate(target_chunk_size = as.factor(target_chunk_size))
######################################################################

d <- dedup_data %>%
  filter(algorithm %in% ALGORITHMS_TO_COMPARE)

for (dataset_name in unique(d$dataset)) {
  filtered_data <- d %>%
    filter(dataset == dataset_name) %>%
    rename_datasets() %>% 
    rename_algorithms()
  
  p <- filtered_data %>% 
    ggplot(aes(x=target_chunk_size, y=dedup_ratio, color=algorithm, group=algorithm)) +
    geom_line(position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=1, shape=21, fill="white") +
    ylab("Dedup. Ratio") +
    xlab("Target Chunk Size") +
    theme(legend.position="none") +
    guides(color = guide_legend(nrow = 4)) +
    scale_color_futurama() +
    ylim(0, 0.9)
  
  print_plot(p, paste("dedup_overview_", dataset_name, sep=""), height=1.6)
}

# Legend
dummy_plot <- p + theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(), # This removes the legend title
    legend.text = element_text(size = 11), # Sets the legend text size
    legend.direction = "horizontal" # Ensures the legend items are laid out horizontally
  ) +
  guides(color = guide_legend(ncol = 3))
legend <- cowplot::get_legend(dummy_plot)

if (!dev.cur()) dev.new()
grid.newpage()
grid.draw(legend)
legend_plot <- recordPlot()
dev.off()
dev.new()

print_plot(legend_plot, "dedup_overview_legendonly", height=1)

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

# Rabin
p <- dedup_data %>%
  filter(dataset=="web") %>%
  filter(target_chunk_size %in% POWER_OF_TWO_SIZES) %>%
  filter(algorithm %in% RABIN_ALGORITHMS) %>%
  mutate(window_size = str_split_i(algorithm,"_",-1)) %>%
  mutate(algo_group = str_split_i(algorithm,"_",1)) %>%
  ggplot(aes(x=target_chunk_size, y=dedup_ratio, color=window_size, group=window_size)) +
  geom_line(position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=1, shape=21, fill="white") +
  labs(x="Target Chunk Size (B)",y="Dedup. Ratio") +
  scale_color_jama(name="Window Size (B)", # Legend label
                   breaks=c("16", "32", "48", "64", "128", "256"),
                   labels=c("16","32","48","64","128","256")) +
  theme(legend.position = "none")

print_plot(p,"dedup_comparison_window_sizes_web_algorithm_rabin",height=1.6)

# Buzhash
p <- dedup_data %>%
  filter(dataset=="web") %>%
  filter(target_chunk_size %in% POWER_OF_TWO_SIZES) %>%
  filter(algorithm %in% BUZHASH_ALGORITHMS) %>%
  mutate(window_size = str_split_i(algorithm,"_",-1)) %>%
  mutate(algo_group = str_split_i(algorithm,"_",1)) %>%
  ggplot(aes(x=target_chunk_size, y=dedup_ratio, color=window_size, group=window_size)) +
  geom_line(position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=1, shape=21, fill="white") +
  labs(x="Target Chunk Size (B)",y="Dedup. Ratio") +
  scale_color_jama(name="Window Size (B)", # Legend label
                   breaks=c("16", "32", "48", "64", "128", "256"),
                   labels=c("16","32","48","64","128","256")) +
  theme(legend.position = "none")

print_plot(p,"dedup_comparison_window_sizes_web_algorithm_buzhash",height=1.6)

# Legend
dummy_plot <- p + theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10), # Adjusts the legend text size
    legend.direction = "horizontal", # Ensures the legend items are laid out horizontally
    legend.key.size = unit(0.5, "cm") # Adjusts the legend key size
  ) +
  guides(color = guide_legend(ncol = 3, override.aes = list(size = 3)))

legend <- cowplot::get_legend(dummy_plot)

if (!dev.cur()) dev.new()
grid.newpage()
grid.draw(legend)
legend_plot <- recordPlot()
dev.off()
dev.new()

print_plot(legend_plot, "dedup_comparison_window_sizes_legendonly", height=0.8)


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
