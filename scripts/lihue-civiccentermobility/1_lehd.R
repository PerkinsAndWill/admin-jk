library(tidyverse)
library(tigris)
library(sf)
library(rgdal)
library(mapview)

gdb_path <- "G:/Current/KAUAI_Lihue_Civic_Center_Mobility_Plan_2022.0969/Analysis/GDB/LihueMobility_GDB.gdb"
ogrListLayers(gdb_path)

soi_15 <- st_read(gdb_path, layer="Walkshed_15") %>% 
  st_zm(drop=TRUE) %>% 
  st_transform(coord_global)
# write_rds(soi_15, "data/lihue-civiccentermobility/lehd_analysis/walkshed_15min.rds")

block_groups_lst <- c("150070405002", "150070405004",
                      "150070405005", "150070405003")

block_groups <- block_groups(state = "Hawaii", year=2019)
soi_block_groups <- block_groups %>% 
  filter(GEOID %in% c("150070405002", "150070405004",
                      "150070405005", "150070405003"))

jobs_csv <- read.csv("data/lihue-civiccentermobility/lodes_jobs_2019/lodes_jobs_data.csv")
od_csv <- read.csv("data/lihue-civiccentermobility/lodes_od_2019/lodes_od_data.csv")

destination <- od_csv %>% 
  filter(to_id %in% block_groups_lst) %>% 
  mutate(from_id = case_when(from_id %in% block_groups_lst ~999999999999,
                             TRUE ~from_id)) %>% 
  group_by(from_id) %>% 
  summarise(total_jobs = sum(total_jobs)) %>% 
  mutate(internal = if_else(from_id == 999999999999, TRUE, FALSE)) %>% 
  arrange(desc(total_jobs)) %>% 
  mutate(all_jobs = sum(total_jobs, na.rm = TRUE)) %>% 
  filter(!from_id == 999999999999) %>% 
  slice(1:30) %>% 
  mutate(top30_jobs = sum(total_jobs, na.rm = TRUE))

origin <- od_csv %>% 
  filter(from_id %in% block_groups_lst) %>% 
  mutate(to_id = case_when(to_id %in% block_groups_lst ~999999999999,
                             TRUE ~to_id)) %>% 
  group_by(to_id) %>% 
  summarise(total_jobs = sum(total_jobs)) %>% 
  mutate(internal = if_else(to_id == 999999999999, TRUE, FALSE)) %>% 
  arrange(desc(total_jobs)) %>% 
  mutate(all_jobs = sum(total_jobs, na.rm = TRUE)) %>% 
  filter(!to_id == 999999999999) %>% 
  slice(1:30) %>% 
  mutate(top30_jobs = sum(total_jobs, na.rm = TRUE))

block_groups_filter <- block_groups %>% 
  filter(GEOID %in% block_groups_lst |
           GEOID %in% destination$from_id |
           GEOID %in% origin$to_id) %>% 
  select(GEOID, NAMELSAD, geometry) %>% 
  st_transform(coord_global)
# write_rds(block_groups_filter, "data/lihue-civiccentermobility/lehd_analysis/filtered_block_groups.rds")

block_groups_point <- block_groups %>% 
  st_centroid(.) %>% 
  mutate(point = st_coordinates(.)) %>% 
  st_drop_geometry() %>%
  as_tibble() %>%
  mutate(X = point[,"X"], Y = point[,"Y"]) %>%
  select(GEOID, X, Y) #%>%
  # select(GEOID, geometry)
soi_point <- soi_15 %>% 
  st_transform(coord_global) %>% 
  st_centroid(.) %>% 
  mutate(point = st_coordinates(.)) %>% 
  st_drop_geometry() %>%
  as_tibble() %>%
  mutate(X = point[,"X"], Y = point[,"Y"]) %>%
  select(Name, X, Y) #%>%
  # select(Name, Shape)

destination_tbl <- destination %>% 
  mutate(flow_id = 1:n()) %>% 
  mutate(from_id = as.character(from_id)) %>% 
  left_join(block_groups_point, by=c("from_id"="GEOID")) %>% 
  rename(from_x = X, from_y = Y) %>%
  mutate(to_id = "999999999999",
         to_x = soi_point$X, to_y = soi_point$Y)
destination_a <- destination_tbl %>% 
  select(flow_id, total_jobs, from_x, from_y) %>% 
  rename(X = from_x, Y = from_y)
destination_b <- destination_tbl %>% 
  select(flow_id, total_jobs, to_x, to_y) %>% 
  rename(X = to_x, Y = to_y)
destination_c <- bind_rows(destination_a, destination_b) %>% 
  st_as_sf(coords = c("X", "Y")) %>% 
  st_set_crs(coord_global) %>% 
  group_by(flow_id, total_jobs) %>% 
  summarise(do_union=TRUE) %>% 
  ungroup() %>% 
  st_cast("LINESTRING")
# mapview(c, zcol = "total_jobs")
# write_rds(destination_c, "data/lihue-civiccentermobility/lehd_analysis/lehd2019_jobs_to_lihue.rds")

origin_c %>% filter(total_jobs %in% c(112, 45))

origin_tbl <- origin %>% 
  mutate(flow_id = 1:n()) %>% 
  mutate(to_id = as.character(to_id)) %>% 
  left_join(block_groups_point, by=c("to_id"="GEOID")) %>%
  rename(to_x = X, to_y = Y) %>%
  mutate(from_id = "999999999999",
         from_x = soi_point$X, from_y = soi_point$Y)
origin_a <- origin_tbl %>% 
  select(flow_id, total_jobs, from_x, from_y) %>% 
  rename(X = from_x, Y = from_y)
origin_b <- origin_tbl %>% 
  select(flow_id, total_jobs, to_x, to_y) %>% 
  rename(X = to_x, Y = to_y)
origin_c <- bind_rows(origin_a, origin_b) %>% 
  st_as_sf(coords = c("X", "Y")) %>% 
  st_set_crs(coord_global) %>% 
  group_by(flow_id, total_jobs) %>% 
  summarise(do_union=TRUE) %>% 
  ungroup() %>% 
  st_cast("LINESTRING")
# mapview(c, zcol = "total_jobs")
# write_rds(origin_c, "data/lihue-civiccentermobility/lehd_analysis/lehd2019_jobs_from_lihue.rds")


## LODES - Workplace Area Characteristics (WAC)
wac_2019 <- read.csv("data/lihue-civiccentermobility/lodes_2019/hi_wac_S000_JT00_2019.csv")
wac_summary <- wac_2019 %>% 
  mutate(w_geocode = str_sub(as.character(w_geocode), start=1, end=12)) %>% 
  filter(w_geocode %in% block_groups_lst) %>% 
  select(-c(createdate, w_geocode)) %>% 
  # group_by(w_geocode) %>% 
  summarise_all(sum) %>% 
  select(C000, 
         CNS01,CNS02,CNS03,CNS04,CNS05,CNS06,CNS07,CNS08,CNS09,CNS10,
         CNS11,CNS12,CNS13,CNS14,CNS15,CNS16,CNS17,CNS18,CNS19,CNS20) %>% 
  pivot_longer(!C000, names_to = "job_id", values_to = "num_jobs") %>% 
  mutate(job_name = case_when(job_id == "CNS01" ~"Agriculture, Forestry, Fishing and Hunting",
                              job_id == "CNS02" ~"Mining, Quarrying, and Oil and Gas Extraction",
                              job_id == "CNS03" ~"Utilities",
                              job_id == "CNS04" ~"Construction",
                              job_id == "CNS05" ~"Manufacturing",
                              job_id == "CNS06" ~"Wholesale Trade",
                              job_id == "CNS07" ~"Retail Trade",
                              job_id == "CNS08" ~"Transportation and Warehousing",
                              job_id == "CNS09" ~"Information",
                              job_id == "CNS10" ~"Finance and Insurance",
                              job_id == "CNS11" ~"Real Estate and Rental and Leasing",
                              job_id == "CNS12" ~"Professional, Scientific, and Technical Services",
                              job_id == "CNS13" ~"Management of Companies and Enterprises",
                              job_id == "CNS14" ~"Administrative and Support and Waste Management and Remediation Services",
                              job_id == "CNS15" ~"Educational Services",
                              job_id == "CNS16" ~"Health Care and Social Assistance",
                              job_id == "CNS17" ~"Arts, Entertainment, Recreation",
                              job_id == "CNS18" ~"Accomodation and Food Services",
                              job_id == "CNS19" ~"Other Services [except Public Administration]",
                              job_id == "CNS20" ~"Public Administration")) %>% 
  rename(total_jobs = C000) %>% 
  select(job_id, job_name, num_jobs, total_jobs) %>% 
  mutate(jobs_pct = num_jobs/total_jobs)
write_rds(wac_summary,"data/lihue-civiccentermobility/lehd_analysis/wac_2019_summary.rds")

## LODES - Residence Area Characteristics (RAC)
rac_2019 <- read.csv("data/lihue-civiccentermobility/lodes_2019/hi_rac_S000_JT00_2019.csv")
rac_summary <- rac_2019 %>% 
  mutate(h_geocode = str_sub(as.character(h_geocode), start=1, end=12)) %>% 
  filter(h_geocode %in% block_groups_lst) %>% 
  select(-c(createdate, h_geocode)) %>% 
  # group_by(w_geocode) %>% 
  summarise_all(sum) %>% 
  select(C000, 
         CNS01,CNS02,CNS03,CNS04,CNS05,CNS06,CNS07,CNS08,CNS09,CNS10,
         CNS11,CNS12,CNS13,CNS14,CNS15,CNS16,CNS17,CNS18,CNS19,CNS20) %>% 
  pivot_longer(!C000, names_to = "job_id", values_to = "num_jobs") %>% 
  mutate(job_name = case_when(job_id == "CNS01" ~"Agriculture, Forestry, Fishing and Hunting",
                              job_id == "CNS02" ~"Mining, Quarrying, and Oil and Gas Extraction",
                              job_id == "CNS03" ~"Utilities",
                              job_id == "CNS04" ~"Construction",
                              job_id == "CNS05" ~"Manufacturing",
                              job_id == "CNS06" ~"Wholesale Trade",
                              job_id == "CNS07" ~"Retail Trade",
                              job_id == "CNS08" ~"Transportation and Warehousing",
                              job_id == "CNS09" ~"Information",
                              job_id == "CNS10" ~"Finance and Insurance",
                              job_id == "CNS11" ~"Real Estate and Rental and Leasing",
                              job_id == "CNS12" ~"Professional, Scientific, and Technical Services",
                              job_id == "CNS13" ~"Management of Companies and Enterprises",
                              job_id == "CNS14" ~"Administrative and Support and Waste Management and Remediation Services",
                              job_id == "CNS15" ~"Educational Services",
                              job_id == "CNS16" ~"Health Care and Social Assistance",
                              job_id == "CNS17" ~"Arts, Entertainment, Recreation",
                              job_id == "CNS18" ~"Accomodation and Food Services",
                              job_id == "CNS19" ~"Other Services [except Public Administration]",
                              job_id == "CNS20" ~"Public Administration")) %>% 
  rename(total_jobs = C000) %>% 
  select(job_id, job_name, num_jobs, total_jobs) %>% 
  mutate(jobs_pct = num_jobs/total_jobs)
write_rds(rac_summary,"data/lihue-civiccentermobility/lehd_analysis/rac_2019_summary.rds")

