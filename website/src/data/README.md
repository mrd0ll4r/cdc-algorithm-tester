dedup_data %>%
    algorithm_as_factor() %>%
    mutate(unique_ratio=unique_chunks_size_sum/dataset_size) %>%
    mutate(dedup_ratio=1-unique_ratio) %>%
    mutate(target_chunk_size = as.factor(target_chunk_size)) %>% filter(algorithm %in% c("fsc","ae","ram","mii","pci","rabin_32","buzhash_64","gear","gear_nc_1","gear_nc_2","gear_nc_3","bfbc","bfbc_custom_div")) %>% rename_algorithms() %>% rename_datasets() %>% select("algorithm","dataset","target_chunk_size","dedup_ratio") %>% write.csv("dedup.csv", row.names=FALSE)