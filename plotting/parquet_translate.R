library(dplyr)
library(bit64)
library(arrow)

csv_dir = "./csv"

csd_datasets=c("code","lnx","pdf","web","zero","random")

for (ds in csd_datasets) {
  print(paste("converting",ds))

  d = read_csv_arrow(
    sprintf("%s/csd_%s.csv.gz", csv_dir, ds),
    col_types=schema(algorithm=utf8(), dataset=utf8(), target_chunk_size=int32(), chunk_size=int64()),
    as_data_frame=FALSE)

  d %>%
    group_by(dataset, algorithm, target_chunk_size) %>%
    write_dataset(paste0(csv_dir,"/parquet/csd"),format="parquet", hive_style = TRUE, compression="gzip")
}
