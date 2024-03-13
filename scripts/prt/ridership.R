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

ridership_apr2019 <- read_excel(file.path(data_path,
                                          "nn_processed",
                                          "april 2019 ridership.xlsx")) %>% 
  mutate(day_of_week = case_when(day_type=="WEEKDAY" ~"Weekday",
                                 day_type=="SAT." ~"Saturday",
                                 day_type=="SUN." ~"Sunday"),
         month = "April",
         year = 2019) %>% 
  select(route,month,year,day_of_week,avg_riders)

## Service Stats ===============================================================
dashboard_stats_2023 <- read_excel(file.path(get_sharepoint_dir(),
                                            "NexTransit Network Redesign 921196.002 - Documents/General/T3 Existing Conditions",
                                            "PRT Operstat.xlsx"),
                                  sheet="Dashboard Stats",
                                  skip=5) %>% 
  clean_names() %>% #filter(route=="53") %>% #view()
  select(1,2,16:27) %>%  # weekday, saturday, sunday
  rename(wkdy_rev_hrs = 3,
         wkdy_rev_mi = 4,
         wkdy_trips = 5,
         wkdy_peak_veh = 6,
         sat_rev_hrs = 7,
         sat_rev_mi = 8,
         sat_trips = 9,
         sat_peak_veh = 10,
         sun_rev_hrs = 11,
         sun_rev_mi = 12,
         sun_trips = 13,
         sun_peak_veh = 14) %>% #clipr::write_clip()
  mutate_at(vars(3:14), as.numeric) %>% #view()
  pivot_longer(cols = c(3:14),
               names_to = "variable") %>% 
  filter(str_detect(variable, "rev_hrs|rev_mi")) %>% 
  mutate(service_type = case_when(str_detect(variable, "wkdy") ~"Weekday",
                                  str_detect(variable, "sat") ~"Saturday",
                                  str_detect(variable, "sun") ~"Sunday")) %>% 
  mutate(variable = case_when(str_detect(variable, "rev_hrs") ~"revenue_hours",
                              str_detect(variable, "rev_mi") ~"revenue_miles")) %>% 
  filter(!is.na(route)) %>% 
  pivot_wider(names_from = "variable",
              values_from = "value")

dashboard_stats_2019 <- read_excel(file.path(data_path,
                                             "nn_processed",
                                             "Schedule_Pick_Route_Day_Summary.xlsx"),
                                   sheet = "Schedule_Pick_Route_Day_Summary") %>% 
  select(route,
         service_type = schedule_type,
         trips,
         revenue_miles = miles_in_service,
         service_hours = hours_in_service,
         layover_hours = hours_layover) %>% 
  distinct() %>% 
  group_by(route, service_type) %>% 
  filter(trips==max(trips,na.rm=TRUE)) %>% 
  summarise(revenue_miles = mean(revenue_miles,na.rm=TRUE),
            service_hours = mean(service_hours,na.rm=TRUE),
            layover_hours = mean(layover_hours,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(revenue_hours = service_hours + layover_hours)

dashboard_stats <- bind_rows(dashboard_stats_2019 %>% 
                               select(-service_hours, -layover_hours) %>% 
                               mutate(year=2019,month="April"),
                             dashboard_stats_2023 %>% 
                               mutate(year=2023,month="April")) %>% 
  select(-route_name) %>% 
  mutate(day_of_week = factor(service_type, levels=c("Weekday","Saturday","Sunday"))) %>% 
  arrange(year,route,day_of_week) %>% 
  select(year, month, route, day_of_week,
         revenue_miles, revenue_hours)

route_list <- dashboard_stats_2023 %>% 
  distinct(route, route_name, service_type) %>% 
  rename(route_id=route, day_of_week=service_type)


# Calculate Ridership ==========================================================
ridership_apr2023_summary <- ridership_apr2023 %>% 
  group_by(survey_date, route_name) %>% 
  summarise(total_daily_ons = sum(passengers_on),
            total_daily_offs = sum(passengers_off)) %>% 
  ungroup() %>% 
  complete(route_name,
           survey_date = seq.Date(as.Date("2023-04-01"), as.Date("2023-04-30"), by="days"),
           fill = list(total_daily_ons=0, total_daily_offs=0)) %>% 
  arrange(route_name, survey_date) %>% 
  mutate(survey_day = lubridate::wday(survey_date, label=TRUE),
         service_period = case_when(survey_day=="Sat" ~"Saturday",
                                  survey_day=="Sun" ~"Sunday",
                                  TRUE ~"Weekday")) %>% 
  group_by(service_period, route_name) %>% 
  summarise(ridership_daily_avg = mean(total_daily_ons,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(service_period = factor(service_period, levels=c("Weekday","Saturday","Sunday"))) %>% 
  arrange(service_period, route_name)

ridership_rdc_count <- read_excel(file.path(data_path,
                                            "nn_processed",
                                            "APC vs RDC Ridership.xlsx"),
                                  skip = 1) %>%
  clean_names() %>%
  select(service_period=service_type, route_name, rdc_count)

ridership_calibration <- ridership_rdc_count %>% 
  left_join(ridership_apr2023_summary, by=c("service_period","route_name")) %>% 
  mutate(rdc_factor = (rdc_count - ridership_daily_avg) / ridership_daily_avg) %>% 
  select(service_period, route_name, rdc_factor)

# Re-Calibrate data
ridership_apr2023_adjusted <- 
  ridership_apr2023 %>% 
  left_join(ridership_calibration, by=c("service_period", "route_name")) %>% 
  mutate(passengers_on = passengers_on * (1+rdc_factor),
         passengers_off = passengers_off * (1+rdc_factor)) %>% 
  select(-rdc_factor)

ridership_apr2023_summary_adjusted <- 
  ridership_apr2023_adjusted %>% 
  group_by(survey_date, route_name) %>% 
  summarise(total_daily_ons = sum(passengers_on),
            total_daily_offs = sum(passengers_off)) %>% 
  ungroup() %>% 
  complete(route_name,
           survey_date = seq.Date(as.Date("2023-04-01"), as.Date("2023-04-30"), by="days"),
           fill = list(total_daily_ons=0, total_daily_offs=0)) %>% 
  arrange(route_name, survey_date) %>% 
  mutate(survey_day = lubridate::wday(survey_date, label=TRUE),
         service_period = case_when(survey_day=="Sat" ~"Saturday",
                                  survey_day=="Sun" ~"Sunday",
                                  TRUE ~"Weekday")) %>% 
  group_by(service_period, route_name) %>% 
  summarise(ridership_daily_avg = mean(total_daily_ons,na.rm=TRUE),
            ridership_total = sum(total_daily_ons,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(service_period = factor(service_period, levels=c("Weekday","Saturday","Sunday"))) %>% 
  arrange(service_period, route_name)

ridership_df <- bind_rows(ridership_apr2019 %>% 
                            rename(route_name=route, ridership_daily_avg=avg_riders),
                          ridership_apr2023_summary_adjusted %>% 
                            mutate(year=2023, month="April") %>% 
                            select(-ridership_total) %>% 
                            rename(day_of_week=service_period)) %>% 
  filter(route_name %in% unique(ridership_apr2023_summary_adjusted$route_name)) %>% 
  select(year, month, day_of_week, route_name, ridership_daily_avg) %>% 
  left_join(dashboard_stats, by=c("year","month","day_of_week","route_name"="route")) %>% 
  rename(route_id = route_name) %>% 
  mutate(daily_boarding = ridership_daily_avg,
         daily_boarding_per_rev_hour = daily_boarding/revenue_hours,
         daily_boarding_per_rev_mile = daily_boarding/revenue_miles) %>% 
  left_join(route_list, by=c("route_id","day_of_week")) %>% 
  select(route_id, route_name, day_of_week, year, month,
         daily_boarding, daily_boarding_per_rev_hour, daily_boarding_per_rev_mile) %>% 
  pivot_longer(cols=c("daily_boarding","daily_boarding_per_rev_hour","daily_boarding_per_rev_mile"),
               names_to = "variable") %>% 
  mutate(variable = case_when(variable=="daily_boarding" ~"Daily Boardings",
                              variable=="daily_boarding_per_rev_hour" ~"Daily Boardings per Revenue Hour",
                              variable=="daily_boarding_per_rev_mile" ~"Daily Boardings per Revenue Mile"))

# Ridership bubbles ===========================================================
# ridership_apr2023 %>% as_tibble() %>% slice(1:10) %>% view()
  
stops_lon_lat <- ridership_apr2023_adjusted %>% 
  distinct(route_name, direction_name, clever_id, latitude, longitude)

ridership_stop_time_of_day <- 
  ridership_apr2023_adjusted %>% 
  # calculate weekday/time of day
  mutate(trip_hour = as.numeric(substr(trip_start_time,12,13)),
         survey_day = lubridate::wday(survey_date, label=TRUE),
         day_of_week = case_when(survey_day=="Sat" ~"Saturday",
                                 survey_day=="Sun" ~"Sunday",
                                 TRUE ~"Weekday"),
         time_of_day = case_when(trip_hour >= 4 & trip_hour < 6 ~"Early",
                                 trip_hour >= 6 & trip_hour < 9 ~"AM Peak",
                                 trip_hour >= 9 & trip_hour < 15 ~"Midday",
                                 trip_hour >= 15 & trip_hour < 18 ~"PM Peak",
                                 trip_hour >= 18 & trip_hour < 20 ~"Evening",
                                 trip_hour >= 20 & trip_hour < 23 ~"Late",
                                 trip_hour >= 23 | trip_hour < 4 ~"Owl")) %>%
  select(-longitude, -latitude) %>% 
  # assign clever_id, long, lat
  left_join(stops_lon_lat, by=c("route_name","direction_name","clever_id")) %>% 
  
  group_by(day_of_week, survey_date, time_of_day, route_name, direction_name, clever_id, longitude, latitude) %>% 
  summarise(daily_on = sum(passengers_on, na.rm=TRUE),
            daily_off = sum(passengers_off, na.rm=TRUE)) %>% 
  ungroup()
  
date_sequence <- seq(as.Date("2023-04-01"), as.Date("2023-04-30"), by="days")
time_cat_sequence <- c("Early","AM Peak","Midday","PM Peak","Evening","Late","Owl")

ridership_apr2023_stop <- 
  stops_lon_lat %>% 
  slice(rep(1:n(), each = length(date_sequence))) %>% 
  group_by(route_name, direction_name, clever_id) %>% 
  mutate(survey_date = rep(date_sequence)) %>% 
  slice(rep(1:n(), each = length(time_cat_sequence))) %>% 
  group_by(route_name, direction_name, clever_id, survey_date) %>% 
  mutate(time_of_day = rep(time_cat_sequence)) %>% 
  ungroup() %>% 
  
  left_join(ridership_stop_time_of_day,
            by=c("route_name","direction_name","clever_id","longitude","latitude","survey_date","time_of_day")) %>% 
  
  mutate(survey_day = lubridate::wday(survey_date, label=TRUE),
         day_of_week = case_when(survey_day=="Sat" ~"Saturday",
                                 survey_day=="Sun" ~"Sunday",
                                 TRUE ~"Weekday"),
         daily_on = ifelse(is.na(daily_on),0,daily_on),
         daily_off = ifelse(is.na(daily_off),0,daily_off)) %>% 
  
  group_by(day_of_week, time_of_day, route_name, direction_name, clever_id, longitude, latitude) %>% 
  summarise(avg_daily_on_adjusted = mean(daily_on, na.rm=TRUE),
            avg_daily_off_adjusted = mean(daily_off, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(day_of_week = factor(day_of_week,
                              levels=c("Weekday","Saturday","Sunday")),
         time_of_day = factor(time_of_day,
                              levels=c("Early","AM Peak","Midday","PM Peak","Evening","Late","Owl"))) %>% 
  arrange(day_of_week, route_name, direction_name, clever_id, time_of_day) %>% 
  select(day_of_week, route_name, direction_name, time_of_day, clever_id, 
         avg_daily_on_adjusted, avg_daily_off_adjusted, longitude, latitude)

write_rds(ridership_apr2023_stop, "output/prt/prt_ridership_stops_apr2023.rds")


# ridership_apr2023_stop <- 
#   # bind_rows(ridership_stop_time_of_day, ridership_stop_daily) %>% 
#   ridership_stop_time_of_day %>% 
#   # filter(route_name=="28X" & day_of_week=="Weekday") %>% 
#   # filter(route_name=="1" & clever_id==1407 & survey_date=="2023-04-08") %>% 
#   group_by(day_of_week, time_of_day, route_name, direction_name, clever_id, longitude, latitude) %>% 
#   summarise(avg_daily_on = mean(daily_on, na.rm=TRUE),
#             avg_daily_off = mean(daily_off, na.rm=TRUE)) %>% 
#   ungroup() %>% #summarise(sum(avg_daily_on))
#   # left_join(ridership_calibration, by=c("day_of_week"="service_type","route_name")) %>% 
#   mutate(avg_daily_on_adjusted = avg_daily_on,
#          avg_daily_off_adjusted = avg_daily_off) %>% 
#   mutate(day_of_week = factor(day_of_week,
#                               levels=c("Weekday","Saturday","Sunday")),
#          time_of_day = factor(time_of_day,
#                               levels=c("Early","AM Peak","Midday","PM Peak","Evening","Late","Owl"))) %>% 
#   arrange(day_of_week, route_name, direction_name, clever_id, time_of_day) %>% 
#   select(day_of_week, route_name, direction_name, time_of_day, clever_id, 
#          avg_daily_on_adjusted, avg_daily_off_adjusted, longitude, latitude)
  


ridership_apr2023_stop %>% 
  filter(route_name=="28X" & day_of_week=="Weekday") %>% summarise(sum(avg_daily_on_adjusted))

# Day of week charts ===========================================================
ridership_day_of_week_daily <- 
  ridership_apr2023 %>% 
  mutate(day_of_week = lubridate::wday(survey_date, label=TRUE, abbr = FALSE)) %>% 
  group_by(day_of_week, survey_date, route_name) %>% 
  summarise(daily_on = sum(passengers_on, na.rm=TRUE)) %>% 
  group_by(day_of_week, route_name) %>% 
  summarise(avg_daily_on = mean(daily_on, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(time_of_day = "Daily") #%>%
  # mutate(join_type = case_when(day_of_week=="Sunday" ~"Sunday",
  #                              day_of_week=="Saturday" ~"Saturday",
  #                              TRUE ~"Weekday")) %>% 
  # left_join(ridership_calibration, by=c("route_name","join_type"="service_type")) %>% 
  # mutate(avg_daily_on = avg_daily_on*(1+rdc_factor)) %>% 
  # select(-rdc_factor)

ridership_day_of_week_timecat <- 
  ridership_apr2023 %>% 
  mutate(trip_hour = as.numeric(substr(trip_start_time,12,13)),
         survey_day = lubridate::wday(survey_date, label=TRUE),
         day_of_week = lubridate::wday(survey_date, label=TRUE, abbr = FALSE),
         time_of_day = case_when(trip_hour >= 4 & trip_hour < 6 ~"Early",
                                 trip_hour >= 6 & trip_hour < 9 ~"AM Peak",
                                 trip_hour >= 9 & trip_hour < 15 ~"Midday",
                                 trip_hour >= 15 & trip_hour < 18 ~"PM Peak",
                                 trip_hour >= 18 & trip_hour < 20 ~"Evening",
                                 trip_hour >= 20 & trip_hour < 23 ~"Late",
                                 trip_hour >= 23 | trip_hour < 4 ~"Owl",
                                 TRUE ~"-")) %>% 
  group_by(day_of_week, survey_date, time_of_day, route_name) %>% 
  summarise(daily_on = sum(passengers_on, na.rm=TRUE)) %>% 
  group_by(day_of_week, time_of_day, route_name) %>% 
  summarise(avg_daily_on = mean(daily_on, na.rm=TRUE)) %>% 
  ungroup() #%>% 
  # mutate(join_type = case_when(day_of_week=="Sunday" ~"Sunday",
  #                              day_of_week=="Saturday" ~"Saturday",
  #                              TRUE ~"Weekday")) %>% 
  # left_join(ridership_calibration, by=c("route_name","join_type"="service_type")) %>% 
  # mutate(avg_daily_on = avg_daily_on*(1+rdc_factor)) %>% 
  # select(-rdc_factor, -join_type)

ridership_day_of_week <- bind_rows(ridership_day_of_week_daily,ridership_day_of_week_timecat) %>% 
  mutate(day_of_week = factor(day_of_week,
                              levels=c("Monday","Tuesday","Wednesday","Thursday",
                                       "Friday","Saturday","Sunday")),
         time_of_day = factor(time_of_day,
                              levels=c("Early","AM Peak","Midday","PM Peak",
                                       "Evening","Late","Owl","Daily"))) %>% 
  arrange(route_name, day_of_week, time_of_day) %>% 
  select(route_name, day_of_week, time_of_day, avg_daily_on)


# Spreadsheet Export ===========================================================
## RidershipStats
ridership_df %>% 
  # left_join(ridership_rdc_count, by=c("day_of_week"="service_type","route_id"="route_name")) %>% 
  # mutate(rdc_count = case_when(year==2019 ~as.numeric(NA),
  #                              variable %in% c("Daily Boardings per Revenue Hour",
  #                                              "Daily Boardings per Revenue Mile") ~as.numeric(NA),
  #                              TRUE ~rdc_count)) %>% 
  # filter(year==2023, variable=="Daily Boardings") %>% 
  clipr::write_clip()

# RidershipMap
ridership_apr2023_stop %>% 
  pivot_longer(cols=c("avg_daily_on_adjusted","avg_daily_off_adjusted"),
               names_to = "on_off_type") %>%
  write.csv(file.path(data_path,"nn_processed/ridership_apr2023_stop.csv"),
            row.names=FALSE)

# RidershipDayofWeek
ridership_day_of_week %>% 
  clipr::write_clip()

# AvgDailyTrips ===========================================================
xl_path <- file.path(get_sharepoint_dir(),
                     "NexTransit Network Redesign 921196.002 - Documents/General/T3 Existing Conditions/Dashboard",
                     "master_db_v2.xlsx")
a <- read_excel(xl_path, sheet="AvgDailyTrips") %>% 
  clean_names()
b <- read_excel(xl_path, sheet="RidershipStats") %>% 
  clean_names() 
b <- b %>% 
  filter(variable=="Daily Boardings") %>% 
  select(-month, -variable) %>% 
  pivot_wider(names_from = year, values_from = value) %>% 
  rename(daily_boardings_2019 = `2019`,
         daily_boardings_2023 = `2023`)

a %>% 
  mutate(x2019 = as.numeric(x2019),
         x2022 = as.numeric(x2022)) %>% 
  left_join(b) %>% 
  mutate(daily_boardings_trip_2019 = daily_boardings_2019/x2019,
         daily_boardings_trip_2023 = daily_boardings_2023/x2022) %>% 
  rename(x_2023 = x2022, x_2019 = x2019) %>% 
  pivot_longer(!c(route_id, route_name, day_of_week),
               names_to = "variable") %>% 
  mutate(year = sapply(str_split(variable, "_"), function(x) tail(x, 1))) %>% 
  mutate(variable = case_when(str_detect(variable,"x_") ~"Average Daily Trips",
                              variable %in% 
                                c("daily_boardings_2019",
                                  "daily_boardings_2023") ~"Average Daily Boardings",
                              variable %in% 
                                c("daily_boardings_trip_2019",
                                  "daily_boardings_trip_2023") ~"Average Daily Boardings per Trip")) %>% 
  pivot_wider(names_from = year,
              values_from = value) %>% 
  filter(!variable == "Average Daily Trips") %>% 
  clipr::write_clip()
  


a %>% pivot_longer(!c("route_id","route_name"),
                   names_to = "name",
                   values_to = "value") %>% 
  mutate(year = str_split_fixed(name,"_",3)[,1],
         day_of_week = str_split_fixed(name,"_",3)[,3]) %>% 
  mutate(year = as.numeric(year),
         day_of_week = case_when(day_of_week == "wkdy" ~"Weekday",
                                 day_of_week == "sat" ~"Saturday",
                                 day_of_week == "sun" ~"Sunday")) %>% 
  select(route_id, route_name, day_of_week, year, value) %>% 
  pivot_wider(names_from = "year",
              values_from = "value") %>% 
  # clean_names() %>% 
  select(route_id, route_name, day_of_week, `2019`, `2022`) %>% 
  clipr::write_clip()


