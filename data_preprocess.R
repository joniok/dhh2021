library("geofi")
library("tidyverse")

out_path = "electoral_districts/shiny_data/" # output path

# Map shape files and electoral disctricts of year 2020
d1 <- get_municipalities(year = 2020) %>%
  filter(maakunta_name_fi != "Ahvenanmaa") %>% # exclude Åland
  select(kunta, vuosi, nimi, vaalipiiri_code, vaalipiiri_name_fi, geom) %>%
  rename(electoral_district = vaalipiiri_name_fi)



# Data files
data_files <- list.files("data/", full.names = TRUE)

df_list <- lapply(data_files, read.csv, stringsAsFactors = FALSE)
names(df_list) <- stringr::str_replace_all(data_files, "data//|.csv", "")


# Merge data sets

## electoral districts 
districts <- list(`2004-2013` = df_list[["city_speaker_districts_counts_04_13"]],
                  `1986-1995` = df_list[["city_speaker_districts_counts_86_95"]]) %>% 
  purrr::map_df(I, .id = "period") %>%
  filter(!grepl("Ahvenanmaa", electoral_district)) %>% # Exclude Åland electoral district
  rename(speaker_district_count = mention_count) 

## parliamentary groups merged with electoral districts
groups <- list(`2004-2013` = df_list[["city_counts_statistics_by_parties_04_13"]],
               `1986-1995` = df_list[["city_counts_statistics_by_parties_86_95"]]) %>%
  purrr::map_df(I, .id = "period") %>%
  rename(speaker_party_count = mention_count) %>%
  filter(!grepl("Ahvenanmaa", party)) %>% # Exclude Åland representatives
  mutate(party = factor(party)) %>%
  left_join(districts, by = c("period", "city", "year"))



# merge groups and district data
historical_names <- read.csv("historical_electoral_districts.csv", stringsAsFactors = FALSE)

groups_with_districts <- groups %>%
  filter(electoral_district != "NaN") %>%
  mutate(electoral_district = plyr::mapvalues(electoral_district,
                                              from = historical_names$previous_name,
                                              to = historical_names$current_name)) %>% # harmonize electoral district names (2020)
  group_by(period, year, party, electoral_district) %>% # aggregate values
  summarise(speaker_district_count = sum(speaker_district_count, na.rm = TRUE),
            speaker_party_count = sum(speaker_party_count, na.rm = TRUE),
            .groups = 'keep')

annual_counts <- groups_with_districts %>%
  group_by(period, year, electoral_district) %>%
  summarise(district_total = sum(speaker_district_count),
            party_total = sum(speaker_party_count), .groups = "keep")

# add missing rows to data
by_cols =  c("period", "year", "party", "electoral_district")

dummy_data <- list(period = unique(app_plot_data$period),
                   year = unique(app_plot_data$year),
                   party = unique(app_plot_data$party),
                   electoral_district = unique(app_plot_data$electoral_district)
) %>% 
  purrr::cross() %>%
  plyr::ldply(data.frame) %>%
  mutate(speaker_party_count = 0,
         speaker_district_count = 0) %>%
  anti_join(groups_with_districts, by = all_of(by_cols))


app_plot_data <- groups_with_districts %>%
  bind_rows(dummy_data) %>%
  left_join(annual_counts, by = c("period", "year", "electoral_district")) %>%
  mutate(speaker_district_prop = speaker_district_count / district_total,
         speaker_party_prop = speaker_party_count / party_total) %>%
  left_join(d1, by = "electoral_district") # add shapefiles of 2020 disctricts
  
  
  ## Export processed data
  app_plot_data_sf <- sf::st_as_sf(app_plot_data)

saveRDS(app_plot_data_sf, file = paste0(out_path, "app_plot_data.rds"))
