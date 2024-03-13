library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)
library(nntools)

data_path <- file.path("C:/Users/kimji/Michael Baker International",
                       "PRT Network Planning - Bus Line Redesign - Bus Line Redesign/03 - Existing Conditions/Data")

# Load Data ====================================================================
## Raw Ridership ===============================================================
ridership_apr2023 <- read.csv(file.path(data_path,
                                        "APC & AVL",
                                        "prt_apc_2023-04-01_2023-04-30.csv")) %>% 
  as_tibble() %>% 
  filter(!(trip_start_time=="" | is.na(latitude) | is.na(longitude))) %>% 
  mutate(survey_date = lubridate::ymd(str_split_fixed(survey_date,"T",2)[,1]),
         service_period = lubridate::wday(survey_date, label=TRUE)) %>% 
  mutate(service_period = case_when(service_period=="Sat" ~"Saturday",
                                    service_period=="Sun" ~"Sunday",
                                    TRUE ~"Weekday"))

load_by_hour <- ridership_apr2023 %>% 
  mutate(time_actual_arrive = as.POSIXct(str_remove(str_split_fixed(time_actual_arrive,"T",2)[,2],"Z"), format="%H:%M:%S"),
         arrive_hour = hour(time_actual_arrive)) %>% 
  group_by(survey_date, trip_key) %>% 
  mutate(arrive_hour = ifelse(is.na(arrive_hour), lag(arrive_hour, n=1L), arrive_hour)) %>% 
  mutate(arrive_hour = ifelse(arrive_hour < 4, arrive_hour+24, arrive_hour)) %>% 
  fill(arrive_hour, .direction="updown") %>% 
  ungroup() %>% 
  filter(!is.na(arrive_hour))

## GTFS Stops ==================================================================
gtfs_stops_geom <- read_rds("data/prt/gtfs_stops_geom.rds")

gtfs_stops_coords <- gtfs_stops_geom %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  bind_cols(gtfs_stops_geom$stop_code, .) %>% 
  rename(stop_code = 1) %>% 
  distinct()

gtfs_stop_seq <- gtfs_stops_geom %>% 
  st_drop_geometry() %>% 
  group_by(service_day, route_id, direction_id, shape_id) %>% 
  mutate(n_trips = n_distinct(trip_id),
         n_stops = n_distinct(stop_code)) %>% 
  group_by(service_day, route_id, direction_id) %>% 
  filter(n_trips == max(n_trips)) %>% 
  filter(n_stops == max(n_stops)) %>% 
  ungroup() %>% 
  select(service_day, route_id, route_short_name, route_long_name, direction_id, stop_code, stop_name, stop_sequence) %>% 
  distinct(route_short_name, stop_code, stop_name) %>% 
  left_join(gtfs_stops_coords)

## Line load calculations ======================================================
route_n_patterns <- load_by_hour %>% 
  distinct() %>% 
  group_by(service_period, route_name, survey_date, direction_name, trip_key, vehicle_number, trip_start_time) %>% 
  summarise(start_clever_id = clever_id[row_number()==1],
            end_clever_id = clever_id[row_number()==n()],
            n = n()) %>% 
  ungroup() %>% 
  mutate(pattern_id = paste(service_period, route_name, direction_name, start_clever_id, n, sep="-")) %>% 
  count(service_period, route_name, direction_name, pattern_id)

route_max_patterns <- route_n_patterns %>% 
  group_by(route_name, service_period, direction_name) %>% 
  filter(n == max(n)) %>% 
  ungroup()

filtered_route_patterns <- load_by_hour %>% 
  distinct() %>% 
  group_by(service_period, route_name, survey_date, direction_name, trip_key, vehicle_number, trip_start_time) %>% 
  mutate(pattern_id = paste(service_period, route_name, direction_name, clever_id[row_number()==1], n(), sep="-")) %>% 
  ungroup() %>% 
  filter(pattern_id %in% route_max_patterns$pattern_id)

route_line_load <- filtered_route_patterns %>% 
  group_by(survey_date, route_name, direction_name, trip_key, trip_start_time, service_period, vehicle_number) %>% 
  mutate(stop_sequence = 1:n()) %>% 
  group_by(service_period, route_name, direction_name, arrive_hour, clever_id) %>% # stop_sequence,
  summarise(stop_sequence = mean(stop_sequence),
            median_load = median(load),
            average_load = mean(load),
            max_load = max(load),
            median_trip_max_load = median(trip_max_load),
            average_trip_max_load = mean(trip_max_load),
            max_trip_max_load = max(trip_max_load)) %>% 
  ungroup()

# Export to Excel
route_line_load %>% 
  mutate(service_period = factor(service_period, levels=c("Weekday","Saturday","Sunday")),
         clever_id = as.character(clever_id)) %>% 
  arrange(service_period, route_name, direction_name, arrive_hour, stop_sequence) %>% 
  left_join(gtfs_stop_seq, by=c("route_name"="route_short_name","clever_id"="stop_code")) %>% 
  select(service_period:clever_id, stop_name, stop_sequence, X, Y, median_load:max_trip_max_load) %>% 
  clipr::write_clip()
