library(tidyverse)
library(sf)
library(readxl)
library(janitor)
# library(nntools)

# Load Data ===
# gtfs_routes <- read_rds("data/prt/gtfs_routes.rds")
# gtfs_stops_geom <- read_rds("data/prt/gtfs_stops_geom.rds")
# 
# route_type <- read_excel(file.path(get_sharepoint_dir(),
#                                    "NexTransit Network Redesign 921196.002 - Documents/General/T3 Existing Conditions",
#                                    "PRT Operstat.xlsx"),
#                          sheet="Dashboard Stats",
#                          skip=5) %>% 
#   clean_names() %>% 
#   select(route, service_cat) %>% 
#   filter(!is.na(route)) %>% 
#   mutate(service_cat = factor(service_cat,
#                               levels=c("Local","Coverage","Commuter","Rapid"))) %>% 
#   arrange(desc(service_cat), desc(route)) %>% 
#   mutate(axis_order = 1:n()) %>% 
#   left_join(gtfs_routes %>% select(route_id,route_long_name), by=c("route"="route_id")) %>% 
#   select(service_cat, route, route_long_name, axis_order)
# 
# # Define Variables ===
# gtfs_headways <- gtfs_stops_geom %>% 
#   
#   # filter(route_short_name=="54", service_day=="Weekday") %>%
#   
#   st_drop_geometry() %>% 
#   select(trip_id, arrival_time, departure_time, stop_sequence, timepoint,
#          route_id, service_day, direction_id, route_long_name) %>% 
#   
#   
#   group_by(route_id, direction_id, trip_id) %>% 
#   filter(stop_sequence==1 | stop_sequence==max(stop_sequence)) %>% #filter(route_id=="O12-202") %>% arrange(service_day,direction_id,departure_time) %>% view()
#   mutate(start_hour = lubridate::hour(min(as.POSIXlt(departure_time))),
#          start_time = as.numeric(departure_time)/(60*60)) %>% 
#   ungroup() %>% 
#   mutate(start_hour = ifelse(start_hour<=3,start_hour+24,start_hour),
#          start_time = ifelse(start_time<4,start_time+24,start_time),
#          service_day = factor(service_day, levels=c("Weekday","Saturday","Sunday")),
#          time_cat = case_when(start_hour>=4 & start_hour<6 ~"Early",
#                               start_hour>=6 & start_hour<9 ~"AM",
#                               start_hour>=9 & start_hour<15 ~"Mid",
#                               start_hour>=15 & start_hour<18 ~"PM",
#                               start_hour>=18 & start_hour<20 ~"Eve",
#                               start_hour>=20 & start_hour<23 ~"Late",
#                               start_hour>=23 ~"Owl")) %>%
#   mutate(time_cat = factor(time_cat,
#                            levels=c("Early","AM","Mid","PM","Eve","Late","Owl"))) %>% 
#   filter(!stop_sequence>1) %>% 
#   # calculate headways
#   arrange(service_day,route_id,direction_id,start_hour,departure_time) %>%
#   group_by(service_day,route_id,direction_id) %>% 
#   mutate(headway_mins = ifelse(start_hour-lag(start_hour)<=3, 
#                                as.numeric(start_time - lag(start_time,n=1L,default=NA))*60,
#                                NA)) %>% 
#   
#   group_by(service_day,route_id,time_cat,start_hour) %>%
#   summarise(headway_mins = mean(headway_mins,na.rm=TRUE)) %>% 
#   ungroup() %>% 
#   mutate(headway_mins = round(headway_mins/5)*5,
#          headway_cat = case_when(headway_mins <= 15 ~"15 mins or less",
#                                  headway_mins > 15 & headway_mins <= 30 ~"16 to 30 mins",
#                                  headway_mins > 30 & headway_mins <= 45 ~"31 to 45 mins",
#                                  headway_mins > 45 & headway_mins <= 60 ~"46 to 60 mins",
#                                  headway_mins > 60 ~"more than 60 mins")) %>% 
#   left_join(route_type, by=c("route_id"="route"))
# 
# gtfs_headways %>% write.csv("output/prt/gtfs_headways.csv", row.names=FALSE)

gtfs_headways <- read.csv("output/prt/gtfs_headways.csv") %>% as_tibble()

# PLOT ====
# c("Weekday","Saturday","Sunday")

i = "Weekday"

df <- gtfs_headways %>% filter(service_day==i)

n_row <- df %>% distinct(route_id) %>% nrow()

plot <- ggplot(df,
               aes(x=start_hour,
                   y=reorder(route_id,axis_order),
                   fill=headway_cat)) +
  # add white border to geom_tile
  geom_tile(color = "white", size=0.25) +
  # remove x and y axis labels
  labs(x="", y="") +
  # remove extra space
  scale_y_discrete(expand=c(0,0)) +
  # scale_x_discrete(position="top") +
  scale_x_continuous(breaks = round(seq(4, 27, by = 1), 1), position = "top") +
  scale_fill_manual(values=c("15 mins or less"="#67BE64",
                             "16 to 30 mins"="#D9E58D",
                             "31 to 45 mins"="#FDDF8A",
                             "46 to 60 mins" = "#F36D44",
                             "more than 60 mins"="#A51D2A"),
                    na.value = "grey90") +
  theme(axis.text.x = element_text(angle = 60, hjust = 0),
        legend.position = "bottom")

ggsave(paste0("output/prt/headways_",i,".eps"),
       plot,
       width = 8.5,
       height = n_row/3.5,
       units = "in")
