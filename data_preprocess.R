library("geofi")
library("tidyverse")
library("sf")

out_path = "politics_and_place/" # output path

# Map shape files and electoral disctricts of year 2020
d1 <- get_municipalities(year = 2020) %>%
  filter(maakunta_name_fi != "Ahvenanmaa") %>% # exclude Åland
  select(kunta, vuosi, nimi, vaalipiiri_code, vaalipiiri_name_fi, geom) %>%
  rename(electoral_district = vaalipiiri_name_fi,
         vaalipiiri_vuosi = vuosi,
         city = nimi)



# Data files
data_files <- list.files("data/", pattern = ".csv", full.names = TRUE)

df_list <- lapply(data_files, read.csv, stringsAsFactors = FALSE)
names(df_list) <- stringr::str_replace_all(data_files, "data//|.csv", "")

# merge groups and district data
historical_names <- read.csv("historical_electoral_districts.csv", stringsAsFactors = FALSE)

# Merge data sets
## electoral districts 

### Main data
districts_raw <- list(`2004-2013` = df_list[["city_speaker_districts_counts_04_13"]],
                      `1986-1995` = df_list[["city_speaker_districts_counts_86_95"]]) %>% 
  purrr::map_df(I, .id = "period") %>%
  filter(!grepl("Ahvenanmaa", electoral_district)) %>% # Exclude Åland electoral district
  filter(city != "Maarianhamina") %>%
  filter(electoral_district != "NaN") %>%
  mutate(electoral_district = plyr::mapvalues(electoral_district,
                                              from = historical_names$previous_name,
                                              to = historical_names$current_name)) %>% # harmonize electoral district names (2020)
  rename(speaker_district_count = mention_count,
         speaker_electoral_district = electoral_district) %>%
  left_join(d1, by = "city") %>%
  st_as_sf() # convert to shape file

### excluded cities 
excluded_cities_districts <- list(period = unique(districts_raw$period),
                                  year = unique(districts_raw$year),
                                  speaker_electoral_district = unique(districts_raw$speaker_electoral_district),
                                  city = d1$city
) %>% 
  purrr::cross() %>%
  plyr::ldply(data.frame) %>%
  mutate(speaker_district_count = 0) %>%
  anti_join(districts_raw, by = c("period", "year", "city", "speaker_electoral_district")) %>%
  left_join(d1, by = "city") %>%
  st_as_sf()

### Add annual counts
districts_counts <-districts_raw %>%
  tibble() %>%
  group_by(period, year, city) %>%
  summarise(speaker_district_total = sum(speaker_district_count, na.rm=TRUE), .groups = "keep")


### aggregate to electoral level
districts <- districts_raw %>%
  bind_rows(excluded_cities_districts) %>%
  left_join(districts_counts, by = c("period", "year", "city")) %>%
  mutate(speaker_electoral_district = factor(speaker_electoral_district)) %>%
  st_as_sf() # convert to shape file

saveRDS(districts, file = paste0(out_path, "districts.rds"))



## parliamentary groups 
parl_groups_raw <- list(`2004-2013` = df_list[["city_counts_statistics_by_parties_04_13"]],
                        `1986-1995` = df_list[["city_counts_statistics_by_parties_86_95"]]) %>%
  purrr::map_df(I, .id = "period") %>%
  filter(!grepl("Ahvenanmaa", party)) %>%# Exclude Åland representatives
  rename(speaker_party_count = mention_count) %>%
  left_join(d1, by = "city") %>%
  st_as_sf() # convert to shape file


### aggregate to 
parl_groups <- parl_groups_raw %>%
  districts <- districts_raw %>%
  group_by(period, year, party) %>%
  summarise_at(.vars = "speaker_party_count", .funs = sum ) %>%
  select(-vaalipiiri_vuosi, -vaalipiiri_code, -kunta)

### excluded cities 



