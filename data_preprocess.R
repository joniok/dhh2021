library("geofi")
library("tidyverse")

out_path = "electoral_districts/shiny_data/" # output path

# Map shape files and electoral disctricts of year 2020
d1 <- get_municipalities(year = 2020) %>%
  select(kunta, vuosi, nimi, vaalipiiri_code, vaalipiiri_name_fi, geom) %>%
  rename(electoral_district = vaalipiiri_name_fi)

saveRDS(d1, file = paste0(out_path, "map_data.rds")) # export as RDS for shiny


# Data files
data_files <- list.files("data/", full.names = TRUE)

df_list <- lapply(data_files, read.csv, stringsAsFactors = FALSE)
names(df_list) <- stringr::str_replace_all(data_files, "data//|.csv", "")


# Merge data sets

## electoral districts merged with shape files
districts <- list(`2004-2013` = df_list[["city_speaker_districts_counts_04_13"]],
                  `1986-1995` = df_list[["city_speaker_districts_counts_86_95"]]) %>% 
  purrr::map_df(I, .id = "period") %>%
  rename(speaker_district_count = mention_count) %>%
  left_join(d1, by = "electoral_district")

## parliamentary groups merged with electoral districts including the shape files
app_plot_data <- list(`2004-2013` = df_list[["city_counts_statistics_by_parties_04_13"]],
               `1986-1995` = df_list[["city_counts_statistics_by_parties_86_95"]]) %>%
  purrr::map_df(I, .id = "period") %>%
  rename(speaker_party_count = mention_count) %>%
  mutate(party = factor(party)) %>%
  left_join(districts, by = c("period", "city", "year"))

## Export processed data

saveRDS(app_plot_data, file = paste0(out_path, "app_plot_data.rds"))
