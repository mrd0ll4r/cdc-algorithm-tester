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
  xlab("File Size (Log Scale)") +
  ylab("Cum. Prob.") +
  labs(color = "Dataset") +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print_plot(p,"dataset_file_sizes", height=2.5)

gc()
