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
# CHUNK SIZE DISTRIBUTIONS
######################################################################
# Note: The complete CSD dataset is too large to be stored in one R vector.
# However, we converted it to an Arrow dataset, which can be imported from disk
# and operated on without this limitation.

csd_data <- open_dataset(sprintf("%s/parquet/csd", csv_dir), hive_style=TRUE, format="parquet")

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

######################################################################
# Huge table about means

d <- csd_data %>%
  group_by(algorithm, dataset, target_chunk_size) %>%
  summarize(n=n(), mean=mean(chunk_size), sd=sd(chunk_size)) %>%
  collect() %>%
  algorithm_as_factor()

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
