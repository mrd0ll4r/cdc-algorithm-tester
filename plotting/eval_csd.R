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
library(purrr)
library(grid)
options(ztable.type = "latex", digits=1)

source("base_setup.R")
source("plot_setup.R")
source("table_setup.R")
source("tikz_setup.R")
source("util.R")

DATASET_ORDER <- c("random", "lnx", "pdf", "web", "code")
ALGORITHM_ORDER <- c("rabin_32", "buzhash_64", "gear", "gear_nc_1", "gear_nc_2", "gear_nc_3", "ae", "ram", "pci", "mii", "bfbc", "bfbc_custom_div")

######################################################################
# CHUNK SIZE DISTRIBUTIONS
######################################################################
# Note: The complete CSD dataset is too large to be stored in one R vector.
# However, we converted it to an Arrow dataset, which can be imported from disk
# and operated on without this limitation.

process_data <- function(csd_data) {
  df <- csd_data %>%
    filter(algorithm %in% ALGORITHM_ORDER) %>% 
    group_by(algorithm, dataset, target_chunk_size) %>%
    summarize(
      mean = mean(chunk_size),
      sd = sd(chunk_size),
      .groups = 'drop'
    ) %>% 
    collect() %>%
    pivot_wider(
      id_cols = c(algorithm, dataset),
      names_from = target_chunk_size,
      values_from = c(mean, sd),
      names_sep = "_"
    )
  
  target_sizes <- c(512, 737, 1024, 2048, 4096, 5152, 8192)
  col_order <- unlist(sapply(target_sizes, function(size) c(paste("mean", size, sep = "_"), paste("sd", size, sep = "_"))))
  
  as.data.frame(df[, c("algorithm", "dataset", col_order)])
}

df <- rbind(
  open_dataset(sprintf("%s/parquet/csd", csv_dir), hive_style=TRUE, format="parquet") %>% 
    filter(dataset %in% c("random", "zero")) %>% 
    process_data(),
  open_dataset(sprintf("%s/parquet/csd_cat", csv_dir), hive_style=TRUE, format="parquet") %>% 
    process_data()
)

df$dataset <- factor(df$dataset, levels = DATASET_ORDER)
df$algorithm <- factor(df$algorithm, levels = ALGORITHM_ORDER)
df <- df[order(df$dataset, df$algorithm), ]
df <- df %>% 
  mutate(
    mean_512 = ifelse(algorithm == "mii", NA, mean_512),
    sd_512 = ifelse(algorithm == "mii", NA, sd_512),
    mean_737 = ifelse(algorithm %in% c("rabin_32", "buzhash_64", "gear"), NA, mean_737),
    sd_737 = ifelse(algorithm %in% c("rabin_32", "buzhash_64", "gear"), NA, sd_737),
    mean_1024 = ifelse(algorithm == "mii", NA, mean_1024),
    sd_1024 = ifelse(algorithm == "mii", NA, sd_1024),
    mean_2048 = ifelse(algorithm == "mii", NA, mean_2048),
    sd_2048 = ifelse(algorithm == "mii", NA, sd_2048),
    mean_4096 = ifelse(algorithm == "mii", NA, mean_4096),
    sd_4096 = ifelse(algorithm == "mii", NA, sd_4096),
    mean_5152 = ifelse(algorithm %in% c("rabin_32", "buzhash_64", "gear"), NA, mean_5152),
    sd_5152 = ifelse(algorithm %in% c("rabin_32", "buzhash_64", "gear"), NA, sd_5152),
    mean_8192 = ifelse(algorithm == "mii", NA, mean_8192),
    sd_8192 = ifelse(algorithm == "mii", NA, sd_8192),
  )

color_scale_df <- df %>%
  mutate(
    sd_512 = ifelse(((df$sd_512 - df$mean_512) / df$mean_512 + 1) / 2 > 1, 1, ((df$sd_512 - df$mean_512) / df$mean_512 + 1) / 2),
    sd_737 = ifelse(((df$sd_737 - df$mean_737) / df$mean_737 + 1) / 2 > 1, 1, ((df$sd_737 - df$mean_737) / df$mean_737 + 1) / 2),
    sd_1024 = ifelse(((df$sd_1024 - df$mean_1024) / df$mean_1024 + 1) / 2 > 1, 1, ((df$sd_1024 - df$mean_1024) / df$mean_1024 + 1) / 2),
    sd_2048 = ifelse(((df$sd_2048 - df$mean_2048) / df$mean_2048 + 1) / 2 > 1, 1, ((df$sd_2048 - df$mean_2048) / df$mean_2048 + 1) / 2),
    sd_4096 = ifelse(((df$sd_4096 - df$mean_4096) / df$mean_4096 + 1) / 2 > 1, 1, ((df$sd_4096 - df$mean_4096) / df$mean_4096 + 1) / 2),
    sd_5152 = ifelse(((df$sd_5152 - df$mean_5152) / df$mean_5152 + 1) / 2 > 1, 1, ((df$sd_5152 - df$mean_5152) / df$mean_5152 + 1) / 2),
    sd_8192 = ifelse(((df$sd_8192 - df$mean_8192) / df$mean_8192 + 1) / 2 > 1, 1, ((df$sd_8192 - df$mean_8192) / df$mean_8192 + 1) / 2),
    
    mean_512 = ifelse(abs(df$mean_512 - 512) > 512, 512, abs(df$mean_512 - 512)),
    mean_737 = ifelse(abs(df$mean_737 - 737) > 737, 737, abs(df$mean_737 - 737)),
    mean_1024 = ifelse(abs(df$mean_1024 - 1024) > 1024, 1024, abs(df$mean_1024 - 1024)),
    mean_2048 = ifelse(abs(df$mean_2048 - 2048) > 2048, 2048, abs(df$mean_2048 - 2048)),
    mean_4096 = ifelse(abs(df$mean_4096 - 4096) > 4096, 4096, abs(df$mean_4096 - 4096)),
    mean_5152 = ifelse(abs(df$mean_5152 - 5152) > 5152, 5152, abs(df$mean_5152 - 5152)),
    mean_8192 = ifelse(abs(df$mean_8192 - 8192) > 8192, 8192, abs(df$mean_8192 - 8192)),
  )

cgroup=c("Algorithm", "Dataset", "512 B", "737 B", "1 KB", "2 KB", "4 KB", "5152 B", "8 KB")
n.cgroup=c(1, 1, 2, 2, 2, 2, 2, 2, 2)

rgroup=c("RANDOM", "LNX", "PDF", "WEB", "CODE")
n.rgroup=c(14, 14, 14, 14, 14, 4)

ztab <- color_scale_df %>% 
  rename_algorithms() %>% 
  ztable() %>%
  addcgroup(cgroup=cgroup, n.cgroup=n.cgroup) %>% 
  addrgroup(rgroup=rgroup,n.rgroup=n.rgroup,cspan.rgroup=1) %>% 
  makeHeatmap(margin=2)

for (col_name in c("mean_512", "sd_512", "mean_737", "sd_737", "mean_1024", "sd_1024", "mean_2048", "sd_2048", "mean_4096", "sd_4096", "mean_5152", "sd_5152", "mean_8192", "sd_8192")) {
  ztab$x[[col_name]] <- as.character(as.integer(df[[col_name]]))
}

writeLines(capture.output(ztab), "tab/csd_means_sd_full.tex")

### Overview table

get_cell_color <- function(att, algo, ds) {
  # Filtering the dataframe based on algorithm and dataset
  means_df <- color_scale_df %>% 
    filter(algorithm == algo, dataset == ds)
  
  # Applying conditional selection of columns based on the algorithm name
  if (any(grepl("^gear", algo))) {
    # If the algorithm name starts with "gear", select columns starting with 'att'
    # but not ending in '737' or '5152'
    means_df <- means_df %>%
      select(matches(paste0("^", att)), 
             -matches(paste0(att, "737$")), 
             -matches(paste0(att, "5152$")))
  } else {
    # Otherwise, just select columns that start with 'att'
    means_df <- means_df %>%
      select(matches(paste0("^", att)))
  }
  
  # Calculating the mean of the selected columns, row-wise, then ungroup
  means_df <- means_df %>%
    rowwise() %>%
    mutate(row_mean = mean(c_across(matches(paste0("^", att))), na.rm = TRUE)) %>%
    ungroup()
  
  # Return the overall mean of the row means
  mean(means_df$row_mean, na.rm = TRUE)
}


algorithms <- list('rabin_32', 'buzhash_64', 'gear', 'gear_nc_1', 'gear_nc_2', 'gear_nc_3', "ae", "ram", "pci", "mii", "bfbc", "bfbc_custom_div")

mean_random <- numeric(length(algorithms))
mean_lnx <- numeric(length(algorithms))
mean_pdf <- numeric(length(algorithms))
mean_web <- numeric(length(algorithms))
mean_code <- numeric(length(algorithms))
sd_random <- numeric(length(algorithms))
sd_lnx <- numeric(length(algorithms))
sd_pdf <- numeric(length(algorithms))
sd_web <- numeric(length(algorithms))
sd_code <- numeric(length(algorithms))

for (i in seq_along(algorithms)) {
  algo <- algorithms[[i]]
  
  mean_random[i] <- get_cell_color("mean", algo, "random")
  mean_lnx[i] <- get_cell_color("mean", algo, "lnx")
  mean_pdf[i] <- get_cell_color("mean", algo, "pdf")
  mean_web[i] <- get_cell_color("mean", algo, "web")
  mean_code[i] <- get_cell_color("mean", algo, "code")
  sd_random[i] <- get_cell_color("sd", algo, "random")
  sd_lnx[i] <- get_cell_color("sd", algo, "lnx")
  sd_pdf[i] <- get_cell_color("sd", algo, "pdf")
  sd_web[i] <- get_cell_color("sd", algo, "web")
  sd_code[i] <- get_cell_color("sd", algo, "code")
}

# Create the dataframe without the algorithms column
df <- data.frame(mean_random, mean_lnx, mean_pdf, mean_web, mean_code,
                 sd_random, sd_lnx, sd_pdf, sd_web, sd_code, stringsAsFactors = FALSE)

# Add the algorithms as a list column explicitly
df$algorithm <- algorithms %>% sapply(function(x) paste(x, collapse = ", "))
df <- df[, c("algorithm", "mean_random", "mean_lnx", "mean_pdf", "mean_web", "mean_code",
             "sd_random", "sd_lnx", "sd_pdf", "sd_web", "sd_code")]

cgroup=c("Algorithms", "Mean", "SD")
n.cgroup=c(1, 5, 5)

ztab <- df %>% 
  rename_algorithms() %>% 
  ztable() %>%
  addcgroup(cgroup=cgroup, n.cgroup=n.cgroup) %>% 
  makeHeatmap(margin=0, cols=c(2,3,4,5,6)) %>% 
  makeHeatmap(margin=0, cols=c(7,8,9,10,11))

writeLines(capture.output(ztab), "tab/csd_overview.tex")

rm(df, ztab, cgroup, rgroup, n.cgroup, n.rgroup, color_scale_df)
gc()

# TODO algorithm_as_factor cleanup

csd_density_plot <- function(df) {
  df = filter(df, !(algorithm %in% c("fsc"))) %>%
    collect()
  target_chunk_size = as.numeric(df$target_chunk_size[1])
  
  df_means <- df %>%
    group_by(algorithm) %>%
    summarize(mean_chunk_size = mean(chunk_size)) %>%
    collect()
  
  return(
    ggplot(df, aes(x = chunk_size, color = algorithm, linetype = algorithm)) +
      geom_freqpoly(bins=100, aes(y = after_stat(density))) +
      xlab("Chunk Size (B)") +
      ylab("Density") +
      xlim(c(0, target_chunk_size * 4)) +
      geom_point(data = df_means, aes(x = mean_chunk_size, y = 0, color = algorithm), size = 3, show.legend = FALSE)
  )
}

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

csd_data <- open_dataset(sprintf("%s/parquet/csd", csv_dir), hive_style=TRUE, format="parquet")

d <- csd_data %>%
  filter(algorithm %in% c("gear","gear_nc_1","gear_nc_2","gear_nc_3"))  %>%
  filter(dataset == "code") %>%
  filter(target_chunk_size == 8192) %>% 
  collect()

###############
# Dataset eval_dataset, eval_target_cs target
p <- d %>% 
  csd_density_plot()
print_plot(p,sprintf("csd_gear_nc_%s_%d",eval_dataset,eval_target_cs))

###############
# All Datasets, eval_target_cs target
p <- d %>%
  filter(target_chunk_size==eval_target_cs) %>%
  csd_density_plot()+
  facet_wrap(~dataset,dir="v")

print_plot(p,sprintf("csd_gear_nc_dataset_faceted_%d",eval_target_cs),height=8)

###############
# Dataset eval_dataset, all targets

p <- d %>%
  filter(dataset==eval_dataset) %>%
  csd_density_plot()+
  facet_wrap(~target_chunk_size,dir="v")

print_plot(p,sprintf("csd_gear_nc_%s_target_faceted",eval_dataset),height=8)

rm(d,p,eval_target_cs,eval_dataset)
gc()

################
# All datasets on target 737

p <- csd_data %>%
  filter(algorithm %in% ALGORITHMS_TO_COMPARE) %>%
  filter(target_chunk_size == 737) %>%
  filter(dataset == "random") %>%
  csd_density_plot()
print_plot(p,sprintf("csd_random_737",eval_dataset), height=8)

p <- csd_data %>%
  filter(algorithm %in% ALGORITHMS_TO_COMPARE) %>%
  filter(target_chunk_size == 737) %>%
  filter(dataset == "code") %>%
  csd_density_plot()
print_plot(p,sprintf("csd_code_737",eval_dataset),height=8)

p <- csd_data %>%
  filter(algorithm %in% ALGORITHMS_TO_COMPARE) %>%
  filter(target_chunk_size == 737) %>%
  filter(dataset == "web") %>%
  csd_density_plot()
print_plot(p,sprintf("csd_web_737",eval_dataset),height=8)

p <- csd_data %>%
  filter(algorithm %in% ALGORITHMS_TO_COMPARE) %>%
  filter(target_chunk_size == 737) %>%
  filter(dataset == "pdf") %>%
  csd_density_plot()
print_plot(p,sprintf("csd_pdf_737",eval_dataset),height=8)

p <- csd_data %>%
  filter(algorithm %in% ALGORITHMS_TO_COMPARE) %>%
  filter(target_chunk_size == 737) %>%
  filter(dataset == "lnx") %>%
  csd_density_plot()
print_plot(p,sprintf("csd_lnx_737",eval_dataset),height=8)

p <- csd_data %>%
  filter(algorithm %in% ALGORITHMS_TO_COMPARE) %>%
  filter(target_chunk_size == 737) %>%
  filter(dataset == "zero") %>%
  csd_density_plot()
print_plot(p,sprintf("csd_zero_737",eval_dataset),height=8)


######################################################################
# Plot Gear variants

for (dataset_name in c("random", "code")) {
  TARGET_CS <- 2048
  if (dataset_name == 'random') {
    dataset_path <- sprintf("%s/parquet/csd", csv_dir)
  } else {
    dataset_path <- sprintf("%s/parquet/csd_cat", csv_dir)
  }
  d <- open_dataset(dataset_path, hive_style=TRUE, format="parquet") %>% 
    filter(dataset == dataset_name) %>%
    filter(target_chunk_size == TARGET_CS) %>%
    filter(algorithm %in% GEAR_ALGORITHMS) %>%
    filter(algorithm != "gear64_simd" & algorithm != "gear64") %>% 
    collect()
  
  df_means <- d %>%
    group_by(algorithm) %>%
    summarize(mean_chunk_size = mean(chunk_size))
  
  labels <- c("gear" = "Vanilla", "gear_nc_1" = "NC-1", "gear_nc_2" = "NC-2", "gear_nc_3" = "NC-3")
  p <- d %>%
    ggplot(aes(x = chunk_size / 1000, color = algorithm, linetype = algorithm)) +
    geom_freqpoly(bins=100, aes(y = after_stat(density))) +
    xlab("Chunk Size (KB)") +
    ylab("Density") +
    xlim(c(0, TARGET_CS * 2 / 1000)) +
    geom_point(data = df_means, aes(x = mean_chunk_size / 1000, y = 0, color = algorithm), size = 2, show.legend = FALSE) +
    theme(legend.position = "none", legend.title = element_blank()) +
    scale_color_discrete(labels = labels) +
    scale_linetype_discrete(labels = labels)
  
  print_plot(p, paste("csd_gear_variants", dataset_name, sep="_"), height=1.8, width=2)
}

print_plot(get_legend_plot(p, 4), "csd_gear_variants_legendonly", width=3.5, height=0.5)


rm(labels,df_means,target_cs,d,p)


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
  csd_density_plot() +
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
  csd_density_plot() +
  facet_wrap(~target_chunk_size,dir="v")

print_plot(p,sprintf("csd_rabin_%s_target_faceted_v1",eval_dataset),height=8)

p <- d %>%
  filter(dataset==eval_dataset) %>%
  csd_density_plot2() +
  facet_wrap(~target_chunk_size,dir="v")

print_plot(p,sprintf("csd_rabin_%s_target_faceted_v2",eval_dataset),height=8)

rm(d,p)
gc()


################
# All datasets on target 1024 or 737

ALGORITHMS_TO_COMPARE <- c("fsc","ae","ram","mii","pci","rabin_32","buzhash_64","gear","bfbc","bfbc_custom_div")

df <- open_dataset(sprintf("%s/parquet/csd_cat", csv_dir), hive_style=TRUE, format="parquet") %>%
  filter(target_chunk_size == 1024 | (target_chunk_size == 737 & algorithm == "mii")) %>%
  filter(algorithm %in% ALGORITHMS_TO_COMPARE) %>%
  filter(!(algorithm %in% c("fsc")))

df_rand <- open_dataset(sprintf("%s/parquet/csd", csv_dir), hive_style=TRUE, format="parquet") %>%
  filter(dataset == 'random') %>%
  filter(target_chunk_size == 1024 | (target_chunk_size == 737 & algorithm == "mii")) %>%
  filter(algorithm %in% ALGORITHMS_TO_COMPARE) %>%
  filter(!(algorithm %in% c("fsc")))

combined_df <- rbind(collect(df), collect(df_rand))
combined_df$dataset <- factor(combined_df$dataset, levels = DATASET_ORDER)

# render per algorithm (lines are datasets)
for (algorithm_name in unique(df$algorithm)) {
  algo_df <- combined_df %>%
    filter(algorithm == algorithm_name) %>%
    rename_algorithms() %>%
    rename_datasets()
  
  p <- ggplot(algo_df, aes(x = chunk_size, color = dataset, linetype = dataset)) +
    geom_freqpoly(bins=100, aes(y = after_stat(density))) +
    xlab("Chunk Size (B)") +
    ylab("Density") +
    xlim(c(0, 1024 * 2)) +
    theme(legend.position = "bottom")
  
  print_plot(p, paste("csd", algorithm_name, sep="_"), height=2)
}

# render per dataset (lines are algorithms)
for (dataset_name in DATASET_ORDER) {
  dataset_df <- combined_df %>%
    filter(dataset == dataset_name) %>%
    rename_algorithms() %>%
    rename_datasets()
  
  p <- ggplot(dataset_df, aes(x = chunk_size, color = algorithm, linetype = algorithm)) +
    geom_freqpoly(bins=100, aes(y = after_stat(density))) +
    xlab("Chunk Size (B)") +
    ylab("Density") +
    xlim(c(0, 1024 * 2)) +
    theme(legend.position = "none")
  
  print_plot(p, paste("csd", dataset_name, sep="_"), height=2)
}

print_plot(get_legend_plot(p, 3), "csd_legendonly", width=4, height=2)


p <- combined_df %>% 
  rename_algorithms() %>% 
  rename_datasets() %>% 
  ggplot(aes(x = chunk_size, color = algorithm, linetype = algorithm)) +
  geom_freqpoly(bins=100, aes(y = after_stat(density))) +
  xlab("Chunk Size (B)") +
  ylab("Density") +
  xlim(c(0, 1024 * 2)) +
  facet_wrap(~dataset, ncol = 2, scales = "free_y") +
  theme(legend.position = "bottom")

print_plot(p, "csd", width=8, height=6)

rm(d,p)
gc()

#########################################
# Gear NC chunk size distributions

p <- ggplot(data_filtered, aes(x=g_norm, y=n_norm, linetype=window_size, fill=window_size)) +
  geom_line(position=position_jitter(width=0,height=0.005)) +
  geom_area(alpha=0.1,position="identity") +
  labs(x="Normalized Codomain", y="Density") +
  scale_fill_manual(values=hue_pal()(6), name="Window Size (B)") +
  scale_color_manual(values=hue_pal()(6), name="Window Size (B)")

