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

# ID VARIABLES =================================================================
gdb_path <- "G:/Current/SEPTA_Bus_Network_Redesign_2019_0467/Data/GDBs"
# "bg_with_philly_neighborhoods"
neighborhood_geom <- read_sf(file.path(gdb_path,
                                       "SEPTA_CBNR_base.gdb"),
                             layer="bg_with_philly_neighborhoods")

region_geom <- 
  read_sf(file.path(gdb_path,
                    "SEPTA_CBNR_base.gdb"),
          layer="Communities_Region") %>% 
  select(location=Location, geometry=Shape) %>% 
  mutate(location = str_remove_all(location, "County - |Philadelphia - "))

route_start <- 
  route_geom %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  select(X,Y) %>% 
  filter(row_number()==1) %>% 
  st_as_sf(coords = c("X","Y"), crs=4326)

route_end <- 
  route_geom %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  select(X,Y) %>% 
  filter(row_number()==nrow(.)) %>% 
  st_as_sf(coords = c("X","Y"), crs=4326)

route_start_region <- 
  route_start %>% 
  st_transform(2272) %>% 
  st_intersection(region_geom) %>% 
  st_drop_geometry() %>% 
  pull(location)

route_end_region <- 
  route_end %>% 
  st_transform(2272) %>% 
  st_intersection(region_geom) %>% 
  st_drop_geometry() %>% 
  pull(location)


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


matched_rt_shape

osm_link_reorder_list <- list()

for(i in 1:nrow(osm_link_geoms)){
  osm_link <- osm_link_geoms[i,] %>% 
    mutate(azimuth = map_dbl(geometry, ~st_avg_line_azimuth(.,4326)))

  osm_link_buffer <- osm_link %>% st_buffer(2)
  
  rt_shape_intersect <- matched_rt_shape %>% 
    st_intersection(osm_link_buffer) %>% 
    st_as_sf() %>% 
    mutate(azimuth = map_dbl(., ~st_avg_line_azimuth(.,4326)))
  
  if(osm_link$azimuth+15 >= rt_shape_intersect$azimuth & 
     rt_shape_intersect$azimuth >= osm_link$azimuth-15){
    print(paste0(i,"/",nrow(osm_link_geoms)," - ",osm_link$osm_link_id,": MATCH"))
    osm_link_fix <- osm_link
  } else{
    print(paste0(i,"/",nrow(osm_link_geoms)," - ",osm_link$osm_link_id,": REVERSE"))
    osm_link_fix <- 
      osm_link %>% 
      st_coordinates() %>% 
      as_tibble() %>% 
      select(X,Y) %>% 
      mutate(temp_sequence = seq_along(X)) %>% 
      arrange(desc(temp_sequence)) %>% 
      select(X,Y) %>% 
      as.matrix() %>% 
      st_linestring() %>% 
      st_sfc(crs=4326) %>% 
      st_as_sf() %>% 
      rename(geometry = x) %>% 
      mutate(osm_link_id = osm_link$osm_link_id,
             name = osm_link$name,
             highway = osm_link$highway,
             edge_sequence = osm_link$edge_sequence,
             azimuth = map_dbl(., ~st_avg_line_azimuth(.,4326)))
  }
  
  osm_link_reorder_list[[i]] <- osm_link_fix
}

osm_link_reorder <- do.call(rbind, osm_link_reorder_list)


output_tbl <- 
  osm_link_reorder %>% 
  st_transform(2272) %>% 
  mutate(length_ft = as.numeric(st_length(geometry)),
         # azimuth = map_dbl(geometry, ~st_avg_line_azimuth(.,4326)),
         length_mi = length_ft/5280) %>% 
  st_drop_geometry() %>% 
  
  # turn-by-turn directions
  group_by(name) %>%
  mutate(sequence_point = case_when(row_number()==1 ~"start",
                                    row_number()==n() ~"end")) %>%
  ungroup() %>%

  mutate(turn_angle = case_when(sequence_point=="start" ~azimuth-lag(azimuth, n=1L, default=NA),
                                TRUE ~as.numeric(NA))) %>%
  mutate(turn_angle = case_when(turn_angle < 0 ~turn_angle+360,
                                TRUE ~turn_angle)) %>%
  mutate(turn_direction = case_when(turn_angle > 180 ~"left",
                                    turn_angle < 180 ~"right",
                                    TRUE ~as.character(NA))) %>%

  group_by(name) %>% 
  summarise(length_mi = sum(length_mi,na.rm=TRUE),
            turn_direction = unique(turn_direction),
            sequence = min(edge_sequence)) %>% 
  ungroup() %>% 
  filter(!(is.na(turn_direction) & sequence > 1)) %>%
  arrange(sequence) %>% 
  select(-sequence) #%>% 
  # clipr::write_clip()


# CONCATENATE TEXT =============================================================
text_start <- paste0("Beginning at the intersection of ",output_tbl$name[1]," and X, located in ",route_start_region,", ")
text_mid_list <- list()
for(i in 2:nrow(output_tbl)-1){
  index = i+1
  if(i==1){
    text <-  paste0(" then ", output_tbl$turn_direction[index]," via ",output_tbl$name[index],", ")
  } else{
    text <- paste0(output_tbl$turn_direction[index]," via ",output_tbl$name[index],", ")
  }
  text_mid_list[[i]] <- text
  # text_mid_list = paste0(text_mid_list,text)
}

text_mid <- text_mid_list %>% unlist() %>% paste(., collapse="")

text_end <- paste0("to the intersection of ",output_tbl$name[19]," and Y, located in ",route_end_region,".")

text_compiled <- paste0(text_start, text_mid, text_end)

