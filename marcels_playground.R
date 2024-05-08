data <- read.csv(gzfile("csv/csd_code.csv.gz"))

library(dplyr)
library(ggplot2)
library(tidyr)

df <- data %>% 
  filter(algorithm == "rabin_256")

ggplot(df, aes(x = chunk_size)) +
  geom_histogram(binwidth = 400, fill = 'blue', color = 'black', alpha = 0.3) +
  xlab("Chunk Size") +
  ylab("Frequency") +
  xlim(c(0, 8192 * 2)) +
  geom_vline(aes(xintercept = target_chunk_size), linetype = "dashed") +
  geom_vline(aes(xintercept = target_chunk_size), color = "red") +
  ggtitle("Frequency Distribution of Chunk Size") +
  facet_wrap(~ target_chunk_size)
