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
library(grid)
library(jsonlite)

source("base_setup.R")
source("plot_setup.R")
source("table_setup.R")
source("tikz_setup.R")
source("util.R")

ALGORITHMS_TO_COMPARE <- c(ALGORITHMS_TO_COMPARE, GEAR_ALGORITHMS)
web_data_dir <- "../website/src/data"

######################################################################
# DEDUPLICATION RATIO
######################################################################

dedup_data <- tibble()
infiles <- c(Sys.glob(sprintf("%s/dedup_*_cat*", csv_dir)), sprintf("%s/dedup_random.csv.gz", csv_dir))
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
  filter(dataset %in% c("code","web","pdf","lnx", "random")) %>%
  algorithm_as_factor() %>%
  mutate(unique_ratio = unique_chunks_size_sum / dataset_size) %>%
  mutate(dedup_ratio = 1 - unique_ratio) %>%
  mutate(target_chunk_size = as.factor(target_chunk_size)) %>% 
  filter(algorithm %in% c("fsc","ae","ram","mii","pci","rabin_32","buzhash_64","gear","gear_nc_1","gear_nc_2","gear_nc_3","bfbc","bfbc_custom_div")) %>% 
  rename_algorithms() %>% 
  rename_datasets() %>% 
  select("algorithm", "dataset", "target_chunk_size", "dedup_ratio") %>%
  mutate(target_chunk_size = as.integer(as.character(target_chunk_size)))  # Convert target_chunk_size back to integer

write_json(dedup_data, sprintf("%s/dedup.json", web_data_dir), pretty = FALSE, auto_unbox = TRUE)

rm(dedup_data)
gc()

######################################################################
# CHUNK SIZE DISTRIBUTIONS
######################################################################

BIN_SIZE_FACTOR <- 100

df <- open_dataset(sprintf("%s/parquet/csd_cat", csv_dir), hive_style=TRUE, format="parquet") %>%
  #filter(target_chunk_size == 1024 | (target_chunk_size == 770 & algorithm == "mii")) %>%
  filter(algorithm %in% ALGORITHMS_TO_COMPARE) %>%
  mutate(max_chunk_size = target_chunk_size * 3,
         bin_size = ceiling(max_chunk_size / BIN_SIZE_FACTOR),
         bin = floor(chunk_size / bin_size) * bin_size) %>%
  group_by(dataset, algorithm, target_chunk_size, bin) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(dataset, algorithm, target_chunk_size) %>% 
  collect() %>%
  mutate(pdf = count / sum(count)) %>%
  summarise(pdf = list(pdf), .groups = 'drop')

df_rand <- open_dataset(sprintf("%s/parquet/csd", csv_dir), hive_style=TRUE, format="parquet") %>%
  filter(dataset == 'random') %>%
  filter(algorithm %in% ALGORITHMS_TO_COMPARE) %>%
  mutate(max_chunk_size = target_chunk_size * 3,
         bin_size = ceiling(max_chunk_size / BIN_SIZE_FACTOR),
         bin = floor(chunk_size / bin_size) * bin_size) %>%
  group_by(dataset, algorithm, target_chunk_size, bin) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(dataset, algorithm, target_chunk_size) %>% 
  collect() %>%
  mutate(pdf = count / sum(count)) %>%
  summarise(pdf = list(pdf), .groups = 'drop')

combined_df <- rbind(df, df_rand) %>% 
  group_by(algorithm, dataset, target_chunk_size)

rm(df, df_rand)

result <- combined_df %>%
  rename_algorithms() %>% rename_datasets() %>%
  rowwise() %>%
  mutate(json = toJSON(list(dataset = dataset, 
                            target = target_chunk_size, 
                            algorithm = algorithm, 
                            pdf = pdf), auto_unbox = TRUE))

json_output <- paste0("[", paste(result$json, collapse = ",\n"), "]")
writeLines(json_output, sprintf("%s/csd.json", web_data_dir))

rm(combined_df, result)
gc()

