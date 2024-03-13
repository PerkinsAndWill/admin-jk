library(tidyverse)
library(sf)
library(tidytransit)
library(lubridate)
library(janitor)
library(nntools)


mbi_sharepoint_dir  = function(){
  renv = Sys.getenv()
  user_dir = stringr::str_replace_all(renv['USERPROFILE'],stringr::fixed('\\'),'/')
  sharepoint_dir = paste0(user_dir,'/Michael Baker International')
  return(sharepoint_dir)
}

coord_local <- 6565
coord_global <- 4326

# FEED INFO ----
# feed_publisher_name:  Pittsburgh Regional Transit
# feed_start_date:      2023-02-19
# feed_end_date:        2023-06-17
# feed_version:         2302

# LOAD DATA ----

data_dir <- 'NexTransit Network Redesign 921196.002 - Documents/General/03 Background and Data'

# gtfs_feed <- read_gtfs(file.path(mbi_sharepoint_dir(),
#                                  data_dir,
#                                  'gtfs/GTFSCleverBusOnly2302.zip'))

gtfs_feed <- read_gtfs(file.path(get_sharepoint_dir(),
                                 data_dir,
                                 'gtfs/gtfs.zip'))

# GTFS - Calendar ----
gtfs_calendar <- 
  gtfs_feed$calendar %>% 
  pivot_longer(cols = c(monday:sunday), 
               names_to = "service_day",
               values_to = "in_service") %>% 
  filter(in_service == 1) %>% 
  mutate(service_day = case_when(service_day == "sunday" ~"Sunday",
                                 service_day == "saturday" ~"Saturday",
                                 TRUE ~"Weekday")) %>% 
  distinct(service_id, service_day, start_date, end_date) %>% 
  select(service_id, service_day)


# GTFS - Trips ----
gtfs_trips <- 
  gtfs_feed$trips %>% 
  filter(service_id %in% gtfs_calendar$service_id) %>%
  select(shape_id, trip_id, route_id, service_id, direction_id, trip_headsign, block_id) %>% 
  left_join(gtfs_calendar, by="service_id")

# GTFS - Routes ----
gtfs_routes <-
  gtfs_feed$routes %>%
  select(route_id, route_short_name, route_long_name, route_type, route_color)

write_rds(gtfs_routes, "data/prt/gtfs_routes.rds")

# gtfs_trips %>% 
#   left_join(gtfs_routes, by="route_id") %>% 
#   distinct(service_day, route_id, route_short_name, route_long_name) %>% 
#   mutate(service_day = factor(service_day,
#                               levels = c("Weekday","Saturday","Sunday"))) %>% 
#   arrange(service_day, route_id) %>% 
#   select(service_day,route_id, route_short_name, route_long_name) %>% write_clip()

gtfs_routes_geom <- 
  gtfs_feed$shapes %>% 
  distinct(shape_id, shape_pt_lon, shape_pt_lat, shape_pt_sequence) %>% 
  arrange(shape_id, shape_pt_sequence) %>% 
  st_as_sf(coords = c("shape_pt_lon","shape_pt_lat"), crs=4326) %>% 
  group_by(shape_id) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") %>% 
  left_join(gtfs_trips %>% distinct(route_id,direction_id,shape_id), by="shape_id") %>% 
  left_join(gtfs_routes, by="route_id") %>% 
  st_transform(coord_local) %>% 
  mutate(length_mi = as.numeric(st_length(geometry))/5280) %>% 
  st_transform(coord_global) %>% 
  select(route_id,route_short_name,route_long_name,
         shape_id,direction_id,length_mi,
         route_type,route_color,
         geometry)

# X <- gtfs_routes_geom %>% 
#   filter(route_id=="290")
# 
# mapview(X,zcol="shape_id",alpha=0.4)

# gtfs_routes_geom %>% 
#   filter(route_id=="299") %>% mapview()

# GTFS - Stops ----
gtfs_stops_geom <- 
  gtfs_feed$stop_times %>% 
  filter(trip_id %in% gtfs_trips$trip_id) %>% 
  left_join(gtfs_trips %>% select(route_id,service_id,direction_id,trip_id,shape_id), by="trip_id") %>% 
  left_join(gtfs_calendar, by="service_id") %>% 
  left_join(gtfs_routes %>% select(route_id,route_short_name,route_long_name), by="route_id") %>% 
  left_join(gtfs_feed$stops %>% select(stop_id,stop_code,stop_name,stop_lat,stop_lon), by="stop_id") %>% 
  st_as_sf(coords = c("stop_lon","stop_lat"), crs=4326)

gtfs_stops_geom %>% st_drop_geometry() %>% distinct(trip_id)

write_rds(gtfs_stops_geom, "data/prt/gtfs_stops_geom.rds")

# headway
gtfs_stops_geom %>% 
  st_drop_geometry() %>% #filter(route_id=="7-202") %>% 
  group_by(route_id, direction_id, trip_id) %>% 
  filter(stop_sequence==1 | stop_sequence==max(stop_sequence)) %>% #filter(route_id=="O12-202") %>% arrange(service_day,direction_id,departure_time) %>% view()
  mutate(start_hour = lubridate::hour(min(as.POSIXlt(departure_time)))) %>% 
  ungroup() %>% 
  mutate(start_hour = ifelse(start_hour<=2,start_hour+24,start_hour)) %>% 
  mutate(service_day = factor(service_day, levels=c("Weekday","Saturday","Sunday"))) %>% 
  arrange(service_day,route_id,direction_id,start_hour) %>% 
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
  group_by(service_day,route_id,time_cat) %>% 
  summarise(service_hours = length(unique(start_hour)),
            headway_mins = median(headway_mins,na.rm=TRUE),
            trip_time_mins = median(trip_time_mins,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(headway_mins = ifelse(service_hours>0 | is.na(service_hours),round(headway_mins/5)*5,0)) %>% 
  arrange(service_day,route_id,time_cat) %>% 
  left_join(gtfs_routes %>% select(1:3), by="route_id") %>% 
  select(service_day, route=route_id, route_short_name, route_long_name,
         time_cat, service_hours, headway_mins, trip_time_mins) %>% 
  pivot_wider(names_from = time_cat,
              values_from = c(service_hours, headway_mins, trip_time_mins)) %>% 
  clipr::write_clip()


gtfs_routes_geom %>% filter(route_id=="DQI-202")

gtfs_stops_geom %>% 
  st_drop_geometry() %>% filter(route_id=="DQI-202")
  group_by(service_day, route_id, direction_id, shape_id) %>% 
  summarise(n_trips = length(unique(trip_id))) %>% 
  ungroup() %>% 
  mutate(service_day = factor(service_day, levels=c("Weekday","Saturday","Sunday"))) %>% 
  arrange(service_day,route_id, direction_id, -n_trips) %>% 
  group_by(service_day, route_id, direction_id) %>% 
  filter(n_trips == max(n_trips)) %>% 
  ungroup() %>% 
  left_join(gtfs_routes_geom %>% 
              st_drop_geometry() %>% 
              select(route_id, route = route_short_name, route_long_name, shape_id, length_mi),
            by = c("route_id","shape_id")) %>% 
  group_by(service_day, route_id, direction_id) %>% 
  filter(length_mi == max(length_mi)) %>% 
  ungroup() %>% 
  group_by(service_day,route_id,route,route_long_name) %>% 
  summarise(dir_count = n(),
            length_mi = sum(length_mi)/dir_count) %>% 
  ungroup() %>% 
  clipr::write_clip()


c("21-202","38-202","51-202","51L-202","52L-202","53L-202","54-202","65-202",
  "77-202","79-202","87-202","91-202","G3-202","O12-202","P12-202","P16-202",
  "P16-202","P69-202","P71-202","P76-202")


gtfs_stops_geom %>% 
  st_drop_geometry() %>% 
  distinct(route_id, direction_id, service_id) %>% 
  arrange(route_id) %>% filter(str_detect(service_id,"Weekday")) %>% view()

# service span
trips_primary <- gtfs_trips %>% 
  group_by(route_id, direction_id, service_day) %>% 
  count(shape_id) %>% 
  # ungroup() %>% 
  # filter(service_day=="Weekday") %>% 
  # arrange(route_id,direction_id,desc(n)) %>% 
  left_join(gtfs_routes_geom %>% st_drop_geometry() %>% select(shape_id,length_mi),
            by="shape_id") %>% 
  filter(n==max(n)) %>% 
  filter(length_mi==max(length_mi)) %>% 
  ungroup()

route_trip_times <- gtfs_stops_geom %>% 
  st_drop_geometry() %>% 
  filter(shape_id %in% unique(trips_primary$shape_id)) %>% 
  group_by(service_day,route_id,route_short_name,route_long_name,direction_id,trip_id) %>% 
  filter(stop_sequence==min(stop_sequence) | stop_sequence==max(stop_sequence)) %>% 
  mutate(trip_start_time = lubridate::hour(lag(departure_time, n=1L, default=NA)),
         trip_time_mins = as.numeric(arrival_time - lag(departure_time, n=1L, default=NA))/60) %>% 
  ungroup() %>% 
  filter(!is.na(trip_time_mins)) %>% 
  select(service_day,route_id,route_short_name,route_long_name,direction_id,trip_id,
         trip_start_time,trip_time_mins) %>% 
  mutate(service_day = factor(service_day, levels=c("Weekday","Saturday","Sunday")),
         trip_start_time = ifelse(trip_start_time<=2,trip_start_time+24,trip_start_time)) %>% 
  arrange(service_day,route_id,direction_id,trip_start_time)
  

# GTFS Stop Trips ===============
gtfs_stops_geom %>% slice(1:10) %>% view()

gtfs_stops <- gtfs_feed$stops %>% 
  select(stop_id, stop_code, stop_name, stop_lat, stop_lon) %>% 
  st_as_sf(coords = c("stop_lon","stop_lat"), crs=4326)

gtfs_stops %>% mapview::mapview()

route_service_hours <- gtfs_stops_geom %>% 
  st_drop_geometry() %>% 
  mutate(departure_hour = hour(departure_time)) %>% 
  distinct(service_day, route_short_name, route_long_name, departure_hour) %>% 
  arrange(service_day, route_short_name, departure_hour)

route_service_hours %>% 
  filter(route_short_name=="2" & service_day=="Saturday") %>%
  right_join(X, by=c("service_day","route_short_name")) %>% 
  distinct(departure_hour)
  filter(is.na(departure_hour.x))
  
  
gtfs_stop_trips <- gtfs_stops_geom %>%
  st_drop_geometry() %>% 
  # filter(route_short_name=="2" & service_day=="Saturday") %>%
  # filter(service_day=="Saturday" & stop_id=="10246") %>%
  mutate(departure_hour = hour(departure_time)) %>% 
  group_by(service_day, route_short_name, stop_id, stop_code, stop_name, departure_hour) %>% 
  summarise(n_trips = n()) %>% 
  group_by(service_day, route_short_name, stop_id, stop_code, stop_name) %>% 
  arrange(departure_hour, .by_group = TRUE) %>% 
  complete(departure_hour = route_service_hours$departure_hour[
    route_service_hours$service_day==.$service_day &
      route_service_hours$route_short_name==.$route_short_name],
           fill = list(n_trips=0)) %>% 
  ungroup() %>% 
  mutate(time_cat = case_when(departure_hour >= 4 & departure_hour < 6 ~"Early",
                              departure_hour >= 6 & departure_hour < 9 ~"AM Peak",
                              departure_hour >= 9 & departure_hour < 15 ~"Midday",
                              departure_hour >= 15 & departure_hour < 18 ~"PM Peak",
                              departure_hour >= 18 & departure_hour < 20 ~"Evening",
                              departure_hour >= 20 & departure_hour < 23 ~"Late",
                              departure_hour >= 23 | departure_hour < 4 ~"Owl",
                              TRUE ~"-")) %>% 
  group_by(service_day, stop_id, stop_code, stop_name, departure_hour, time_cat) %>% 
  summarise(n_trips = sum(n_trips),
            routes = str_flatten(sort(unique(route_short_name)), collapse=",")) %>% 
  
  group_by(service_day, stop_id, stop_code, stop_name, time_cat, routes) %>%
  summarise(n_trips = mean(n_trips,na.rm=TRUE)) %>% 
  ungroup() %>% 
  filter(!n_trips==0) %>% 
  mutate(headway = round(60/n_trips,1),
         headway_cat = case_when(headway <= 10 ~"10 minutes or less",
                                 headway > 10 & headway <= 20 ~"11 to 20 minutes",
                                 headway > 20 & headway <= 30 ~"21 to 30 minutes",
                                 headway > 30 & headway <= 60 ~"31 to 60 minutes",
                                 headway > 60 ~"more than 60 minutes"))

# gtfs_stop_trips %>% filter(headway_cat=="10 minutes or less")

gtfs_stop_trips_geom <- gtfs_stop_trips %>% 
  left_join(gtfs_stops, by=c("stop_id","stop_code","stop_name")) %>% 
  st_as_sf() %>% 
  st_transform(coord_local)

# gtfs_stop_trips_geom %>% filter(service_day=="Weekday" & time_cat=="Early" & stop_id=="1358")

library(arcgisbinding)
arc.check_product()

gdb_path <- "G:/Current/Pittsburgh_NexTransit_Network_Redesign_20230817/Analysis/GDB/Base.gdb"
gdb_meta <- arc.open(gdb_path)

arc.write(file.path(gdb_path, "GTFS/stop_trips"),
          gtfs_stop_trips_geom,
          overwrite = TRUE,
          validate = TRUE)
    
library(leaflet)
library(htmlwidgets)

bbox <- st_bbox(gtfs_stop_trips_geom %>% st_transform(coord_global)) %>% 
  as.vector()

prt_stop_trips <- 
  leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  # addProviderTiles("Esri.WorldImagery", group="Satellite Basemap") %>% 
  addMapPane("230", zIndex = 230) %>%
  addMapPane("240", zIndex = 240) %>%
  fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>% 
  
  addCircleMarkers(data = gtfs_stop_trips_geom %>% 
                     st_transform(coord_global) %>% 
                     filter(service_day=="Weekday" & time_cat=="AM Peak"),
                   fillColor = ~case_when(headway_cat=="10 minutes or less" ~"#2b83ba",
                                          headway_cat=="11 to 20 minutes" ~"#abdda4",
                                          headway_cat=="21 to 30 minutes" ~"#ffffbf",
                                          headway_cat=="31 to 60 minutes" ~"#fdae61",
                                          headway_cat=="more than 60 minutes" ~"#d7191c"),
                   fillOpacity = 1,
                   radius = 4,
                   color = "#000000", weight = 1, opacity = 1, 
                   label = ~paste0("Stop: ",stop_name,"(",stop_id,"); Headway: ",headway),
                   group = "Weekday (AM Peak)",
                   options = pathOptions(pane = "230")) %>% 
  addCircleMarkers(data = gtfs_stop_trips_geom %>% 
                     st_transform(coord_global) %>% 
                     filter(service_day=="Weekday" & time_cat=="Midday"),
                   fillColor = ~case_when(headway_cat=="10 minutes or less" ~"#2b83ba",
                                          headway_cat=="11 to 20 minutes" ~"#abdda4",
                                          headway_cat=="21 to 30 minutes" ~"#ffffbf",
                                          headway_cat=="31 to 60 minutes" ~"#fdae61",
                                          headway_cat=="more than 60 minutes" ~"#d7191c"),
                   fillOpacity = 1,
                   radius = 4,
                   color = "#000000", weight = 1, opacity = 1, 
                   label = ~paste0("Stop: ",stop_name,"(",stop_id,"); Headway: ",headway),
                   group = "Weekday (Midday)",
                   options = pathOptions(pane = "230")) %>% 
  addCircleMarkers(data = gtfs_stop_trips_geom %>% 
                     st_transform(coord_global) %>% 
                     filter(service_day=="Weekday" & time_cat=="PM Peak"),
                   fillColor = ~case_when(headway_cat=="10 minutes or less" ~"#2b83ba",
                                          headway_cat=="11 to 20 minutes" ~"#abdda4",
                                          headway_cat=="21 to 30 minutes" ~"#ffffbf",
                                          headway_cat=="31 to 60 minutes" ~"#fdae61",
                                          headway_cat=="more than 60 minutes" ~"#d7191c"),
                   fillOpacity = 1,
                   radius = 4,
                   color = "#000000", weight = 1, opacity = 1, 
                   label = ~paste0("Stop: ",stop_name,"(",stop_id,"); Headway: ",headway),
                   group = "Weekday (PM Peak)",
                   options = pathOptions(pane = "230")) %>% 
  addCircleMarkers(data = gtfs_stop_trips_geom %>% 
                     st_transform(coord_global) %>% 
                     filter(service_day=="Saturday" & time_cat=="Midday"),
                   fillColor = ~case_when(headway_cat=="10 minutes or less" ~"#2b83ba",
                                          headway_cat=="11 to 20 minutes" ~"#abdda4",
                                          headway_cat=="21 to 30 minutes" ~"#ffffbf",
                                          headway_cat=="31 to 60 minutes" ~"#fdae61",
                                          headway_cat=="more than 60 minutes" ~"#d7191c"),
                   fillOpacity = 1,
                   radius = 4,
                   color = "#000000", weight = 1, opacity = 1, 
                   label = ~paste0("Stop: ",stop_name,"(",stop_id,"); Headway: ",headway),
                   group = "Saturday (Midday)",
                   options = pathOptions(pane = "230")) %>% 
  addCircleMarkers(data = gtfs_stop_trips_geom %>% 
                     st_transform(coord_global) %>% 
                     filter(service_day=="Sunday" & time_cat=="Midday"),
                   fillColor = ~case_when(headway_cat=="10 minutes or less" ~"#2b83ba",
                                          headway_cat=="11 to 20 minutes" ~"#abdda4",
                                          headway_cat=="21 to 30 minutes" ~"#ffffbf",
                                          headway_cat=="31 to 60 minutes" ~"#fdae61",
                                          headway_cat=="more than 60 minutes" ~"#d7191c"),
                   fillOpacity = 1,
                   radius = 4,
                   color = "#000000", weight = 1, opacity = 1, 
                   label = ~paste0("Stop: ",stop_name,"(",stop_id,"); Headway: ",headway),
                   group = "Sunday (Midday)",
                   options = pathOptions(pane = "230")) %>% 
  
  # Legends
  addLegend(position = "bottomright",
            colors = c("#2b83ba","#abdda4","#ffffbf","#fdae61","#d7191c"),
            labels = c("10 minutes or less","11 to 20 minutes","21 to 30 minutes",
                       "31 to 60 minutes","more than 60 minutes"),
            title = "Stop-Level Headway",
            opacity = 1) %>% 
  # Control Groups
  addLayersControl(baseGroups = c("Weekday (AM Peak)",
                                  "Weekday (Midday)",
                                  "Weekday (PM Peak)",
                                  "Saturday (Midday)",
                                  "Sunday (Midday)"),
                   overlayGroups = c(),
                   options = layersControlOptions(collapsed = FALSE)) #%>% 
  # hideGroup(c())
# prt_stop_trips

saveWidget(prt_stop_trips,
           file=file.path(
             "output/prt/leaflet_maps",
             "prt_stop_trips.html"))


# GTFS Span ===============
# service_span <- 
gtfs_stops_geom %>% 
  # filter(route_short_name=="51") %>% 
  st_drop_geometry() %>% 
  # select(-c(stop_headsign, pickup_type, drop_off_type, shape_dist_traveled)) %>% 
  group_by(route_short_name, route_long_name, service_day, trip_id) %>% 
  filter(stop_sequence == min(stop_sequence)) %>% 
  mutate(service_day = factor(service_day, levels=c("Weekday","Saturday","Sunday"))) %>% 
  group_by(route_short_name, route_long_name, service_day) %>% 
  arrange(route_short_name, service_day) %>% 
  arrange(departure_time, .by_group = TRUE) %>% #view()
  filter(departure_time == min(departure_time, na.rm=TRUE) | departure_time == max(departure_time)) %>% 
  select(route_id = route_short_name, route_name = route_long_name, departure_time, service_day) %>% 
  group_by(route_id, route_name, service_day) %>% 
  mutate(timepoint = ifelse(departure_time==min(departure_time),"start","end")) %>% 
  ungroup() %>% 
  mutate(service_day = case_when(service_day == "Weekday" ~"_wkdy",
                                 service_day == "Saturday" ~"_sat",
                                 service_day == "Sunday" ~"_sun")) %>% 
  mutate(col_name = paste0(timepoint,service_day)) %>% 
  select(-c(service_day,timepoint)) %>% 
  pivot_wider(names_from = col_name, values_from = departure_time) %>% 
  write.csv("output/prt/service_span.csv", row.names=FALSE)
  
gtfs_stops_geom %>% 
  filter(route_short_name=="51") %>%
  st_drop_geometry() %>% 
  filter(stop_sequence==1, direction_id==0, service_day=="Weekday") %>% 
  arrange(departure_time) %>%
  
  
  
  # group_by(trip_id) %>% 
  arrange(min(departure_time), .by_group = TRUE) %>% view()
  
# 2019 Trip Statistics ===============
trips_2019_path <- file.path(get_sharepoint_dir(),
                             "NexTransit Network Redesign 921196.002 - Documents",
                             "General/03 Background and Data")
  
trips_2019 <- read.csv(file.path(trips_2019_path,
                                 "western_pennsylvania_rdc",
                                 "d42a9392-f95c-45f7-840f-3829ed21e03a.csv")) %>% 
  as_tibble() %>% 
  clean_names() %>% 
  filter(str_detect(date,"2019-04"))


trips_2019 %>% 
  group_by(route, day_type) %>% 
  summarise(avg_daily_trips = mean(trip_count)) %>% 
  ungroup() %>% 
  filter(day_type=="Sunday") %>% 
  clipr::write_clip()


  
  

