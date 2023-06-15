library(tidyverse)
library(sf)

# LOAD DATA ====================================================================
# gtfs_routes <- 
#   read.delim("G:/Current/SEPTA_Bus_Network_Redesign_2019_0467/Data/z.Original/Remix/Proposed_20230601/routes.txt", header = TRUE, sep = ",") %>% 
#   as_tibble()
# 
# gtfs_trips <- 
#   read.delim("G:/Current/SEPTA_Bus_Network_Redesign_2019_0467/Data/z.Original/Remix/Proposed_20230601/trips.txt", header = TRUE, sep = ",") %>% 
#   as_tibble() %>% 
#   left_join(gtfs_routes %>% select(route_id, route_short_name, route_long_name, route_color), by="route_id")
# 
# route_shape_list <- 
#   gtfs_trips %>% 
#   filter(route_id=="26q65") %>% 
#   distinct(direction_id,shape_id)
# 
# gtfs_route_geom <- 
#   read.delim("G:/Current/SEPTA_Bus_Network_Redesign_2019_0467/Data/z.Original/Remix/Proposed_20230601/shapes.txt", header = TRUE, sep = ",") %>% 
#   as_tibble() %>% 
#   distinct(shape_id, shape_pt_lon, shape_pt_lat, shape_pt_sequence) %>% 
#   arrange(shape_id, shape_pt_sequence) %>% 
#   st_as_sf(coords = c("shape_pt_lon","shape_pt_lat")) %>% 
#   st_set_crs(4326) %>% 
#   group_by(shape_id) %>% 
#   summarise(do_union = FALSE) %>% 
#   st_cast("LINESTRING")
# 
# # "1193890403" INBOUND, "1193890404" OUTBOUND
# 
# route_geom <- 
#   gtfs_route_geom %>% 
#   filter(shape_id == "1193890403")
# 
# library(mapview)
# route_geom %>% mapview()
# 
# write_rds(route_geom, "data/septa/route_geom_sample.rds")

route_geom <- read_rds("data/septa/route_geom_sample.rds")

# MAP MATCHING =================================================================
library(valhallr)
library(leaflet)
library(osmdata)
library(nntools)

nn_valhalla_hostname <- "128.199.8.29"

rt_shp_coords <- 
  route_geom %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(lon = X, lat = Y)

#Map matching API call
match_res_obj <- trace_attributes(shape = rt_shp_coords,
                                  costing = "bus",
                                  hostname = nn_valhalla_hostname)

#Transforming matched trace into an sf linestring
matched_rt_shape <- match_res_obj$shape %>% 
  decode() %>% #Decode Valhalla polyline string
  select(lng,lat) %>%
  as.matrix() %>%
  st_linestring() %>%
  st_sfc(crs = 4326)

# mapview(matched_rt_shape) + mapview(route_geom, color="red")

#This will return a record of the current edge each time the trace passes through an OSM node
#There will be some repeat edges as OSM ways do not neccessarily break at each OSM node.
edge_tibble <- tibble(
  way_id = as.character(match_res_obj$edges$way_id)
)

#Below I am creating an edge sequence -- 
#I do it using a loop just in case an edge happens to be used twice, which is the case on this route at Barbur transit center
extracted_edges_pre <- edge_tibble %>%
  select(way_id) %>%
  mutate(edge_sequence =NA)

extracted_edges_pre$edge_sequence[1] =1

for(j in 2:nrow(extracted_edges_pre)){
  if(extracted_edges_pre$way_id[j] == extracted_edges_pre$way_id[j-1]){
    extracted_edges_pre$edge_sequence[j] = extracted_edges_pre$edge_sequence[j-1]
  }else{
    extracted_edges_pre$edge_sequence[j] = extracted_edges_pre$edge_sequence[j-1] + 1
  }
}

#Finally I can extract the ordered sequence of edges used
extracted_edges <- extracted_edges_pre %>%
  distinct(way_id,edge_sequence)

#Now I can fetch the relevant OSM geometry for each edge using the way ID
osm_query_res = osmdata::opq_osm_id(id = str_flatten(extracted_edges$way_id, collapse = ", "),
                                    type = "way") %>%
  osmdata_sf()

#Create an OSM link geometry for mapping
osm_link_geoms <- osm_query_res$osm_lines %>%
  select(osm_id, name, highway, geometry) %>%
  rename(osm_link_id = osm_id) %>%
  left_join(extracted_edges %>%
              rename(osm_link_id = way_id)) %>%
  arrange(edge_sequence) %>%
  st_as_sf()

output_tbl <- 
  osm_link_geoms %>% 
  st_transform(2272) %>% 
  mutate(length_ft = as.numeric(st_length(geometry)),
         length_mi = length_ft/5280,
         azimuth = map_dbl(geometry, ~st_avg_line_azimuth(.x,2272))) %>% 
  st_drop_geometry() %>% 
  
  # turn-by-turn directions
  # group_by(name) %>%
  # mutate(sequence_point = case_when(row_number()==1 ~"start",
  #                                   row_number()==n() ~"end")) %>%
  # ungroup() %>% 
  # 
  # mutate(turn_angle = case_when(sequence_point=="start" ~azimuth-lag(azimuth, n=1L, default=NA),
  #                               TRUE ~as.numeric(NA))) %>% 
  # mutate(turn_angle = case_when(turn_angle < 0 ~turn_angle+360,
  #                               TRUE ~turn_angle)) %>% 
  # mutate(turn_direction = case_when(turn_angle > 180 ~"left",
  #                                   turn_angle < 180 ~"right",
  #                                   TRUE ~as.character(NA))) %>% 

  group_by(name) %>% 
  summarise(length_mi = sum(length_mi,na.rm=TRUE),
            sequence = min(edge_sequence)) %>% 
  ungroup() %>% 
  arrange(sequence) %>% 
  select(-sequence) #%>% 
  # clipr::write_clip()
