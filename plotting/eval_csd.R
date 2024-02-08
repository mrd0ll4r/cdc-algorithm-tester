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
options(ztable.type = "latex", digits=1)

source("base_setup.R")
source("plot_setup.R")
source("table_setup.R")
source("tikz_setup.R")
source("util.R")

######################################################################
# CHUNK SIZE DISTRIBUTIONS
######################################################################
# Note: The complete CSD dataset is too large to be stored in one R vector.
# However, we converted it to an Arrow dataset, which can be imported from disk
# and operated on without this limitation.

process_data <- function(csd_data) {
  df <- csd_data %>%
    filter(algorithm %in% 
             c("ae", "ram", "mii", "pci", "rabin_32", "adler32_256", "buzhash_64", "gear", "gear_nc_1", "gear_nc_2", "gear_nc_3", "gear64", "bfbc", "bfbc_custom_div")
    ) %>% 
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

df$dataset <- factor(df$dataset, levels = c("random", "lnx", "pdf", "web", "code", "zero"))
df$algorithm <- factor(df$algorithm, levels = c("rabin_32", "buzhash_64", "adler32_256", "gear", "gear_nc_1", "gear_nc_2", "gear_nc_3", "gear64", "ae", "ram", "pci", "mii", "bfbc", "bfbc_custom_div"))
df <- df[order(df$dataset, df$algorithm), ]
df <- df %>% 
  mutate(
    mean_512 = ifelse(algorithm == "mii", NA, mean_512),
    sd_512 = ifelse(algorithm == "mii", NA, sd_512),
    mean_737 = ifelse(algorithm %in% c("rabin_32", "buzhash_64"), NA, mean_737),
    sd_737 = ifelse(algorithm %in% c("rabin_32", "buzhash_64"), NA, sd_737),
    mean_1024 = ifelse(algorithm == "mii", NA, mean_1024),
    sd_1024 = ifelse(algorithm == "mii", NA, sd_1024),
    mean_2048 = ifelse(algorithm == "mii", NA, mean_2048),
    sd_2048 = ifelse(algorithm == "mii", NA, sd_2048),
    mean_4096 = ifelse(algorithm == "mii", NA, mean_4096),
    sd_4096 = ifelse(algorithm == "mii", NA, sd_4096),
    mean_5152 = ifelse(algorithm %in% c("rabin_32", "buzhash_64"), NA, mean_5152),
    sd_5152 = ifelse(algorithm %in% c("rabin_32", "buzhash_64"), NA, sd_5152),
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

rgroup=c("RANDOM", "LNX", "PDF", "WEB", "CODE", "ZERO")
n.rgroup=c(14, 14, 14, 14, 14, 4)

ztab <- color_scale_df %>% 
  ztable() %>%
  addcgroup(cgroup=cgroup, n.cgroup=n.cgroup) %>% 
  addrgroup(rgroup=rgroup,n.rgroup=n.rgroup,cspan.rgroup=1) %>% 
  makeHeatmap(margin=2)

for (col_name in c("mean_512", "sd_512", "mean_737", "sd_737", "mean_1024", "sd_1024", "mean_2048", "sd_2048", "mean_4096", "sd_4096", "mean_5152", "sd_5152", "mean_8192", "sd_8192")) {
  ztab$x[[col_name]] <- as.character(as.integer(df[[col_name]]))
}

writeLines(capture.output(ztab), "tab/csd_means_sd_full.tex")

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

target_cs <- 737
d <- csd_data %>%
  filter(target_chunk_size == 737) %>%
  filter(dataset == "code") %>%
  filter(algorithm %in% GEAR_ALGORITHMS) %>%
  filter(algorithm != "gear64_simd" & algorithm != "gear64")

df_means <- d %>%
  group_by(algorithm) %>%
  summarize(mean_chunk_size = mean(chunk_size))

labels <- c("gear" = "NC-0", "gear_nc_1" = "NC-1", "gear_nc_2" = "NC-2", "gear_nc_3" = "NC-3")
p <- ggplot(d, aes(x = chunk_size, color = algorithm, linetype = algorithm)) +
  geom_freqpoly(bins=100, aes(y = after_stat(density))) +
  xlab("Chunk Size (B)") +
  ylab("Density") +
  xlim(c(0, target_cs * 3)) +
  geom_point(data = df_means, aes(x = mean_chunk_size, y = 0, color = algorithm), size = 2, show.legend = FALSE) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_color_discrete(labels = labels) +
  scale_linetype_discrete(labels = labels)

print_plot(p, "csd_gear_variants", height=3)

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
# All datasets on target 737

ALGORITHMS_TO_COMPARE <- c("fsc","ae","ram","mii","pci","rabin_32","adler32_256","buzhash_64","gear","bfbc","bfbc_custom_div")

csd_density_plot <- function(df) {
  df <- filter(df, !(algorithm %in% c("fsc"))) %>%
    collect()
  target_cs <- as.numeric(df$target_chunk_size[1])

  # Calculate the density for each 'dataset' group
  density_df <- df %>%
    group_by(dataset) %>%
    do({
      dens <- density(.$chunk_size)  # Calculate the density for the 'chunk_size' column
      data.frame(x = dens$x, y = dens$y)  # Convert the density object to a data frame
    })

  # Find the min and max density for each 'dataset' group
  summary_df <- density_df %>%
    group_by(dataset) %>%
    summarize(y_base = 0 - max(y) * 0.003)

  # Calculate mean chunk size for each algorithm within each dataset
  df_means <- df %>%
    group_by(algorithm, dataset) %>%
    summarize(mean_chunk_size = mean(chunk_size), .groups = 'drop')

  # Join the y_base and y_max values to df_means
  df_means <- df_means %>%
    left_join(summary_df, by = "dataset") %>%
    left_join(ymax_df, by = "dataset")

  # Create the density plot with faceting and custom limits
  return(df %>%
           ggplot(aes(x = chunk_size, color = algorithm, linetype = algorithm)) +
           geom_freqpoly(bins=100, aes(y = after_stat(density))) +
           xlab("Chunk Size (B)") +
           ylab("Density") +
           xlim(c(0, target_cs * 3)) +
           facet_wrap(~dataset, ncol = 2, scales = "free_y") +
           theme(legend.position = "bottom") +
           geom_point(data = df_means, aes(x = mean_chunk_size, y = 0 - 0.03 * y_max, color = algorithm), size = 2, show.legend = FALSE)
  )
}

df_list <- list()
ymax_df <- data.frame(dataset = character(), y_max = numeric(), stringsAsFactors = FALSE)

for (dataset in c("random", "zero" , "web", "code", "lnx", "pdf")) {
  df_list[[dataset]] <- read_csv(sprintf("%s/csd_%s_737.csv.gz", csv_dir, dataset), col_types = "cccii") %>%
    filter(algorithm %in% ALGORITHMS_TO_COMPARE) %>%
    filter(!(algorithm %in% c("fsc")))

  p <- df_list[[dataset]] %>%
    ggplot(aes(x = chunk_size, color = algorithm, linetype = algorithm)) +
    geom_freqpoly(bins=100, aes(y = after_stat(density))) +
    xlim(c(0, 737 * 3))
  p <- ggplot_build(p)

  y_max <- max(p$data[[1]]$y, na.rm = TRUE)
  ymax_df <- rbind(ymax_df, data.frame(dataset = dataset, y_max = y_max))
}

d <- bind_rows(df_list)
rm(df_list)

p <- csd_density_plot(d)

print_plot(p, "csd_737", width=8, height=4)

rm(d,p)
gc()

