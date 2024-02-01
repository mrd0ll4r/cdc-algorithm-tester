# Number of significat digits to print
options(pillar.sigfig = 4)

# ggplot theme
theme_set(theme_bw())

# Function to calculate standard error
standard_error <- function(x) sd(x) / sqrt(length(x))

# Function to calculate mean absolute deviation
mean_abs_dev <- function(x) mean(abs(x-mean(x)))

# Function to calculate mean-min as an error metric, as per https://lemire.me/blog/2023/04/06/are-your-memory-bound-benchmarking-timings-normally-distributed/
mean_min_dev <- function(x) mean(x)-min(x)
mean_max_dev <- function(x) max(x) - mean(x)

csv_dir = "./csv"

algorithm_as_factor <- function(df) {
  df$algorithm <- as.factor(df$algorithm)
  df$algorithm <- recode(df$algorithm,
                         `gear_nc_1}`="gear_nc_1",
                         `gear_nc_2}`="gear_nc_2",
                         `gear_nc_3}`="gear_nc_3")
  return(df)
}

QUICKCDC_ALGORITHMS <- c("quick_2","quick_3","quick_hash_2","quick_hash_3",
                         "quick_2_rabin_64","quick_3_rabin_64","quick_hash_2_rabin_64","quick_hash_3_rabin_64",
                         "quick_2_noskip","quick_3_noskip","quick_hash_2_noskip","quick_hash_3_noskip",
                         "quick_2_rabin_64_noskip","quick_3_rabin_64_noskip","quick_hash_2_rabin_64_noskip","quick_hash_3_rabin_64_noskip"
)
QUICKCDC_RABIN_ALGORITHMS <- c("quick_2_rabin_64","quick_3_rabin_64","quick_hash_2_rabin_64","quick_hash_3_rabin_64")
QUICKCDC_GEAR_ALGORITHMS <- c("quick_2","quick_3","quick_hash_2","quick_hash_3")
QUICKCDC_RABIN_ALGORITHMS_NOSKIP <- c("quick_2_rabin_64_noskip","quick_3_rabin_64_noskip","quick_hash_2_rabin_64_noskip","quick_hash_3_rabin_64_noskip")
QUICKCDC_GEAR_ALGORITHMS_NOSKIP <- c("quick_2_noskip","quick_3_noskip","quick_hash_2_noskip","quick_hash_3_noskip")


GEAR_ALGORITHMS <- c("gear","gear_nc_1","gear_nc_2","gear_nc_3","gear64","gear64_simd")
RABIN_ALGORITHMS <- c("rabin_16","rabin_32","rabin_48","rabin_64","rabin_128","rabin_256")
BUZHASH_ALGORITHMS <- c("buzhash_16","buzhash_32","buzhash_48","buzhash_64","buzhash_128","buzhash_256")
ADLER32_ALGORITHMS <- c("adler32_16","adler32_32","adler32_48","adler32_64","adler32_128","adler32_256")
BFBC_ALGORITHMS <- c("bfbc","bfbc_custom_div")

ALGORITHMS_TO_COMPARE <- c("fsc","ae","ram","mii","pci","rabin_32","adler32_256","buzhash_64","gear","bfbc","bfbc_custom_div")

POWER_OF_TWO_SIZES = c(512,1024,2048,4096,8192)
