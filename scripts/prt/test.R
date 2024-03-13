# Max load by line

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

load_by_hour <- ridership_apr2023 %>% 
  mutate(time_actual_arrive = as.POSIXct(str_remove(str_split_fixed(time_actual_arrive,"T",2)[,2],"Z"), format="%H:%M:%S"),
         arrive_hour = hour(time_actual_arrive)) %>% 
  group_by(survey_date, trip_key) %>% 
  mutate(arrive_hour = ifelse(is.na(arrive_hour), lag(arrive_hour, n=1L), arrive_hour)) %>% 
  mutate(arrive_hour = ifelse(arrive_hour < 4, arrive_hour+24, arrive_hour)) %>% 
  fill(arrive_hour, .direction="updown") %>% 
  ungroup() %>% 
  filter(!is.na(arrive_hour))

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
  

  

# filtered_route_patterns %>% 
#   filter(service_period=="Weekday" & route_name=="83" & direction_name=="INBOUND") %>% 
#   group_by(survey_date, route_name, direction_name, trip_key, trip_start_time, service_period, vehicle_number) %>% 
#   mutate(stop_sequence = 1:n()) %>% 
#   ungroup() %>% 
#   distinct(pattern_id, stop_sequence, clever_id) %>% 
#   arrange(stop_sequence) %>% view()

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
  
route_line_load %>% 
  filter(route_name==1 & direction_name=="OUTBOUND" & service_period=="Weekday") %>% 
  filter(arrive_hour==16) %>% view()


route_line_load %>% 
  mutate(service_period = factor(service_period, levels=c("Weekday","Saturday","Sunday")),
         clever_id = as.character(clever_id)) %>% 
  arrange(service_period, route_name, direction_name, arrive_hour, stop_sequence) %>% 
  left_join(gtfs_stop_seq, by=c("route_name"="route_short_name","clever_id"="stop_code")) %>% 
  select(service_period:clever_id, stop_name, stop_sequence, X, Y, median_load:max_trip_max_load) %>% 
  clipr::write_clip()
  
  
  ungroup() %>% 
  filter(service_period=="Weekday" & route_name=="83" & direction_name=="INBOUND" & clever_id==8674) %>% view()
  
  ungroup() %>% 
  distinct(service_period, route_name, direction_name, stop_sequence, clever_id) %>% 
  count(service_period, route_name, direction_name, clever_id) %>% 
  arrange(desc(n))












load_by_hour %>% 
  filter(survey_date %in% c("2023-04-03","2023-04-10","2023-04-18")) %>%
  filter(service_period=="Weekday" & route_name=="1" & direction_name=="OUTBOUND" & clever_id%in%c(12616,12613) & trip_key==13073484) %>%
  mutate(direction_id = case_when(direction_name=="INBOUND" ~1,
                                  direction_name=="OUTBOUND" ~0,
                                  TRUE ~NA)) %>% 
  mutate(clever_id = as.character(clever_id)) %>% 
    
  group_by(service_period, route_name, direction_id, arrive_hour, clever_id) %>% 
  summarise(median_load = median(load),
            average_load = mean(load),
            max_trip_max_load = max(trip_max_load)) %>% 
  ungroup()

load_by_hour_seq <- load_by_hour %>% 
  mutate(direction_id = case_when(direction_name=="INBOUND" ~1,
                                  direction_name=="OUTBOUND" ~0,
                                  TRUE ~NA)) %>% 
  mutate(clever_id = as.character(clever_id)) %>% 
  group_by(service_period, route_name, direction_id, arrive_hour, clever_id) %>% 
  summarise(median_load = median(load),
            average_load = mean(load),
            max_trip_max_load = max(trip_max_load)) %>% 
  ungroup() %>% 
  left_join(gtfs_stop_seq, by=c("route_name"="route_short_name",
                                "clever_id"="stop_code"))

load_by_hour_seq %>% 
  mutate(service_period = factor(service_period, levels=c("Weekday","Saturday","Sunday"))) %>% 
  arrange(service_period, route_name, direction_id, arrive_hour) %>% 
  write.csv("data/prt/line_load.csv", row.names=FALSE)

  
  
  # filter(trip_key==13059590 & survey_date=="2023-04-03")

load_by_hour %>% 
  filter(clever_id==1410) %>% 
  distinct(route_name)
  
load_by_hour %>% 
  filter(route_name=="71C" & clever_id==20776)

gtfs_stops_geom %>% 
  st_drop_geometry() %>% 
  
  group_by(service_day, route_id, direction_id, shape_id) %>% 
  mutate(n_trips = n_distinct(trip_id)) %>% 
  
  filter(service_day=="Weekday" & route_id=="65" & stop_code=="20769") %>% view()
  distinct(service_day, route_id, route_short_name, direction_id, stop_code, stop_name, stop_sequence) %>% 
  count(service_day, route_id, stop_code) %>% arrange(desc(n))
  filter(route_id=="67" & stop_code=="17629")
  
  
  group_by(service_day, route_id, route_short_name, direction_id) %>% 
  select(n_trips) %>% distinct(service_day, route_id, direction_id) %>% count(service_day, route_id, direction_id) %>% arrange(desc(n))
  

gtfs_stops_geom %>% 
  st_drop_geometry() %>% 
  filter(service_day=="Weekday" & route_id=="71C") %>% 
  distinct(shape_id)

  
  group_by(service_day, route_id, route_short_name, stop_code) %>% 
  
  filter(stop_id=="10910" & route_short_name=="57") %>% distinct(stop_sequence)



# PLOT ====

route_type %>% 
  left_join(gtfs_routes %>% select(route_id,route_long_name), by=c("route"="route_id")) %>% 
  arrange(service_cat, route) %>% view()


route_type <- read_excel(file.path(get_sharepoint_dir(),
                                   "NexTransit Network Redesign 921196.002 - Documents/General/T3 Existing Conditions",
                                   "PRT Operstat.xlsx"),
                         sheet="Dashboard Stats",
                         skip=5) %>% 
  clean_names() %>% 
  select(route, service_cat) %>% 
  filter(!is.na(route)) %>% 
  mutate(service_cat = factor(service_cat,
                              levels=c("Local","Coverage","Commuter","Rapid"))) %>% 
  arrange(desc(service_cat), desc(route)) %>% 
  mutate(axis_order = 1:n())

# reorganize routes
# recategorize times (less than 15 minutes)

weekday_freq_plot <- gtfs_stops_geom %>% 
  # filter(route_short_name=="51") %>% 
  st_drop_geometry() %>% #filter(route_id=="7-202") %>% 
  group_by(route_id, direction_id, trip_id) %>% 
  filter(stop_sequence==1 | stop_sequence==max(stop_sequence)) %>% #filter(route_id=="O12-202") %>% arrange(service_day,direction_id,departure_time) %>% view()
  mutate(start_hour = lubridate::hour(min(as.POSIXlt(departure_time)))) %>% 
  ungroup() %>% 
  mutate(start_hour = ifelse(start_hour<=2,start_hour+24,start_hour)) %>% 
  mutate(service_day = factor(service_day, levels=c("Weekday","Saturday","Sunday"))) %>% 
  arrange(service_day,route_id,direction_id,start_hour,departure_time) %>% 
  mutate(time_cat = case_when(start_hour>=4 & start_hour<6 ~"Early",
                              start_hour>=6 & start_hour<9 ~"AM",
                              start_hour>=9 & start_hour<15 ~"Mid",
                              start_hour>=15 & start_hour<18 ~"PM",
                              start_hour>=18 & start_hour<20 ~"Eve",
                              start_hour>=20 & start_hour<23 ~"Late",
                              start_hour>=23 | start_hour<4 ~"Owl")) %>% 
  mutate(time_cat = factor(time_cat,
                           levels=c("Early","AM","Mid","PM","Eve","Late","Owl"))) %>% 
  group_by(service_day,route_id,direction_id,trip_id) %>% 
  mutate(trip_time_mins = as.numeric(arrival_time - lag(departure_time, n=1L, default=NA))/60) %>% 
  fill(trip_time_mins, .direction = "up") %>% 
  filter(!stop_sequence>1) %>% 
  group_by(service_day,route_id,direction_id) %>% 
  mutate(headway_mins = ifelse(start_hour-lag(start_hour)<=3, 
                               as.numeric(departure_time - lag(departure_time, n=1L, default=NA))/60,
                               NA)) %>% 
  group_by(service_day,route_id,time_cat,start_hour) %>%
  summarise(headway_mins = mean(headway_mins,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(headway_mins = round(headway_mins/5)*5,
         headway_group = case_when(headway_mins < 15 ~"Less than 15 minutes",
                                   headway_mins >= 15 & headway_mins < 30 ~"15 to 30 minutes",
                                   headway_mins >= 30 & headway_mins < 45 ~"30 to 45 minutes",
                                   headway_mins >= 45 & headway_mins < 60 ~"45 to 60 minutes",
                                   headway_mins >= 60 ~"More than 60 minutes")) %>% 
  filter(service_day=="Weekday") %>% 
  left_join(route_type, by=c("route_id"="route"))
  
# weekday_freq_plot %>% filter(route_id=="G31")

plot <- ggplot(weekday_freq_plot, aes(x=start_hour,
                                      y=reorder(route_id,axis_order),
                                      # y=route_id,
                                      fill=headway_group))+
  # add white border to geom_tile
  geom_tile(color = "white", size=0.25) +
  # remove x and y axis labels
  labs(x="", y="") +
  # remove extra space
  scale_y_discrete(expand=c(0,0)) +
  # scale_x_discrete(position="top") +
  scale_x_continuous(breaks = round(seq(3, 24, by = 1), 1), position = "top") +
  # scale_fill_manual(values=c("Less than 10 minutes"="#54278f",
  #                            "10 to 20 minutes"="#756bb1",
  #                            "20 to 30 minutes"="#9e9ac8",
  #                            "30 to 60 minutes" = "#bcbddc",
  #                            "More than 60 minutes"="#dadaeb"),
  #                   na.value = "grey90") +
  scale_fill_manual(values=c("Less than 15 minutes"="#67BE64",
                             "15 to 30 minutes"="#D9E58D",
                             "30 to 45 minutes"="#FDDF8A",
                             "45 to 60 minutes" = "#F36D44",
                             "More than 60 minutes"="#A51D2A"),
                    na.value = "grey90") +
  theme(axis.text.x = element_text(angle = 60, hjust = 0),
        legend.position = "bottom")

ggsave("output/prt/headways_weekday.eps",
       plot,
       width = 8.5,
       height = 11,
       units = "in")
