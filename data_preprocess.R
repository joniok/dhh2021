library("geofi")
library("tidyverse")

out_path = "electoral_districts/shiny_data/" # output path

# Map shape files and electoral disctricts of year 2020
d1 <- get_municipalities(year = 2020)

saveRDS(d1, file = paste0(out_path, "map_data.rds")) # export as RDS for shiny


# Data files
data_files <- list.files("data/", full.names = TRUE)

df_list <- lapply(data_files, read.csv, stringsAsFactors = FALSE)
names(df_list) <- stringr::str_replace_all(data_files, "data//|.csv", "")


# Merge datasets

districts <- list(`2004-2013` = df_list[["city_speaker_districts_counts_04_13"]],
                  `1986-1995` = df_list[["city_speaker_districts_counts_86_95"]]) %>% 
  purrr::map_df(I, .id = "period")


groups <- list(`2004-2013` = df_list[["city_counts_statistics_by_parties_04_13"]],
               `1986-1995` = df_list[["city_counts_statistics_by_parties_86_95"]]) %>%
  purrr::map_df(I, .id = "period") %>%
  mutate(party = factor(party))

## Process data

### by electoral district
saveRDS(districts, file = paste0(out_path, "districts.rds"))

### by parliamentary group
saveRDS(groups, file = paste0(out_path, "groups.rds"))
