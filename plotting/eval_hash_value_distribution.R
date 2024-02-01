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
                    breaks=window_sizes,
                    labels=window_sizes) +
    scale_linetype_discrete(name="Window Size (B)",
                            breaks=window_sizes,
                            labels=window_sizes)
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
  filter(algorithm %in% c("adler32", "buzhash", "rabin", "gear64")) %>%
  ggplot(aes(x=g_norm, y=n_norm, linetype=window_size, fill=window_size)) +
  geom_line(position=position_jitter(width=0,height=0.005)) +
  geom_area(alpha=0.1,position="identity") +
  labs(x="Normalized Codomain", y="Density") +
  scale_fill_jama(name="Window Size (B)") +
  scale_linetype_discrete(name="Window Size (B)") +
  facet_wrap(~ algorithm, nrow = 2, scales = "free_y") +
  theme(legend.position="bottom")

print_plot(p, "hash_value_distributions", height = 4)


