library(tidyverse)
library(sf)
library(readxl)
library(nntools)

# LOAD DATA ====================================================================
route_tracker <- 
  read_xlsx(
    file.path(
      get_sharepoint_dir(),
      "SEPTA Bus Revolution - General/SEPTA CBNR-EXTERNAL/12 Tariff Sheets Summer 2023",
      "SEPTA TariffSht_MATRIX.xlsx"),
    sheet="SEPTA Tracking") %>% 
  janitor::clean_names() %>% 
  select(rt_number:status_ready_pending)

route_to_remix_lookup <- 
  read_xlsx("G:/Current/SEPTA_Bus_Network_Redesign_2019_0467/Data/z.Original/Remix/Proposed_20230517/routes.xlsx")

route_run_list <- 
  route_tracker %>% 
  filter(status_ready_pending == "READY") %>% 
  left_join(route_to_remix_lookup %>% select(route_id_clean, route_id, route_long_name), by=c("remix_number"="route_id_clean"))
  
 
## GTFS ========================================================================
gtfs_routes <- 
  read.delim(
    file.path(get_sharepoint_dir(),
              "SEPTA Bus Revolution - General/SEPTA CBNR-EXTERNAL/12 Tariff Sheets Summer 2023/data",
              "GTFS-TRIPS_20cb800d_2023-06-16-18-20-58/routes.txt"),
    header = TRUE, sep = ",") %>%
  as_tibble()

gtfs_routes_sub <- 
  gtfs_routes %>% 
  filter(route_long_name %in% route_run_list$route_long_name) %>% 
  left_join(route_run_list %>% select(rt_number, remix_number, route_long_name), by="route_long_name")

gtfs_trips <-
  read.delim(
    file.path(get_sharepoint_dir(),
              "SEPTA Bus Revolution - General/SEPTA CBNR-EXTERNAL/12 Tariff Sheets Summer 2023/data",
              "GTFS-TRIPS_20cb800d_2023-06-16-18-20-58/trips.txt"),
    header = TRUE, sep = ",") %>%
  as_tibble() 

gtfs_trips_sub <- 
  gtfs_trips %>% 
  filter(route_id %in% gtfs_routes_sub$route_id) %>% 
  filter(service_id==3) %>% 
  distinct(route_id, service_id, direction_id, shape_id)

gtfs_shapes <-
  read.delim(
    file.path(get_sharepoint_dir(),
              "SEPTA Bus Revolution - General/SEPTA CBNR-EXTERNAL/12 Tariff Sheets Summer 2023/data",
              "GTFS-TRIPS_20cb800d_2023-06-16-18-20-58/shapes.txt"),
    header = TRUE, sep = ",") %>%
  as_tibble() %>% 
  distinct(shape_id, shape_pt_lon, shape_pt_lat, shape_pt_sequence) %>% 
  arrange(shape_id, shape_pt_sequence) %>%
  st_as_sf(coords = c("shape_pt_lon","shape_pt_lat")) %>%
  st_set_crs(4326) %>%
  group_by(shape_id) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

gtfs_shapes_sub <- 
  gtfs_shapes %>% 
  filter(shape_id %in% gtfs_trips_sub$shape_id)

## Route geometry ==============================================================
route_geom <- 
  gtfs_shapes_sub %>% 
  left_join(gtfs_trips_sub %>% select(-service_id), by="shape_id") %>% 
  left_join(gtfs_routes_sub, by="route_id")
  
route_list <- 
  route_geom %>% 
  distinct(route_id) %>%
  arrange(route_id) %>% 
  pull()


# write_rds(route_geom, "data/septa/route_geom_sample.rds")
# 
# route_geom <- read_rds("data/septa/route_geom_sample.rds")

## Start/Stop Region ===========================================================
gdb_path <- "G:/Current/SEPTA_Bus_Network_Redesign_2019_0467/Data/GDBs"

region_geom <- 
  read_sf(file.path(gdb_path,
                    "SEPTA_CBNR_base.gdb"),
          layer="Communities_Region") %>% 
  select(location=Location, geometry=Shape) %>% 
  mutate(location_v2 = case_when(str_detect(location, "County - ") ~paste0(location," County"),
                                 TRUE ~location)) %>% 
  mutate(location_v2 = str_remove_all(location_v2, "County - |Philadelphia - "))

# FOR LOOP ====================================================================
library(valhallr)
library(leaflet)
library(osmdata)
library(nntools)

nn_valhalla_hostname <- "128.199.8.29"

route_list_sub <- route_list[29:101]

# 5, 21, 23, 39 and 65
for(route in route_list_sub){
  route_sf <- route_geom %>% filter(route_id==route)
  
  tbl_rt_number <- route_sf %>% distinct(rt_number) %>% pull()
  tbl_remix_number <- route_sf %>% distinct(remix_number) %>% pull()
  
  direction_list <- route_sf %>% distinct(direction_id) %>% pull()
  
  output_tbl_dir_list <- list()
  text_tbl_dir_list <- list()
  
  for(direction in direction_list){
    route_dir_sf <- route_sf %>% filter(direction_id==direction)
    
    # define start and end regions
    route_point <- 
      route_dir_sf %>% 
      st_coordinates() %>% 
      as_tibble() %>% 
      select(X,Y)
    route_point_start <- 
      route_point %>% filter(row_number()==1) %>% 
      st_as_sf(coords = c("X","Y"), crs=4326)
    route_point_end <- 
      route_point %>% filter(row_number()==nrow(.)) %>% 
      st_as_sf(coords = c("X","Y"), crs=4326)
    route_start_region <- 
      route_point_start %>% 
      st_transform(2272) %>% 
      st_intersection(region_geom) %>% 
      st_drop_geometry() %>% 
      pull(location_v2)
    route_end_region <- 
      route_point_end %>% 
      st_transform(2272) %>% 
      st_intersection(region_geom) %>% 
      st_drop_geometry() %>% 
      pull(location_v2)
    
    ## Map matching ============================================================
    route_dir_coords <- 
      route_dir_sf %>% 
      st_coordinates() %>% 
      as_tibble() %>% 
      rename(lon = X, lat = Y)
    
    #Map matching API call
    match_res_obj <- trace_attributes(shape = route_dir_coords,
                                      costing = "bus",
                                      hostname = nn_valhalla_hostname)
    
    #Transforming matched trace into an sf linestring
    matched_rt_shape <- match_res_obj$shape %>% 
      decode() %>% #Decode Valhalla polyline string
      select(lng,lat) %>%
      as.matrix() %>%
      st_linestring() %>%
      st_sfc(crs = 4326)
    
    #This will return a record of the current edge each time the trace passes through an OSM node
    #There will be some repeat edges as OSM ways do not neccessarily break at each OSM node.
    edge_tibble <- tibble(
      way_id = as.character(match_res_obj$edges$way_id))
    
    #Below I am creating an edge sequence -- 
    #I do it using a loop just in case an edge happens to be used twice, which is the case on this route at Barbur transit center
    extracted_edges_pre <- edge_tibble %>%
      select(way_id) %>%
      mutate(edge_sequence =NA)
    
    extracted_edges_pre$edge_sequence[1] =1
    
    for(j in 2:nrow(extracted_edges_pre)){
      if(extracted_edges_pre$way_id[j] == extracted_edges_pre$way_id[j-1]){
        extracted_edges_pre$edge_sequence[j] = extracted_edges_pre$edge_sequence[j-1]
      } else{
        extracted_edges_pre$edge_sequence[j] = extracted_edges_pre$edge_sequence[j-1] + 1
      }}
    
    #Finally I can extract the ordered sequence of edges used
    extracted_edges <- extracted_edges_pre %>%
      distinct(way_id,edge_sequence)
    
    #Now I can fetch the relevant OSM geometry for each edge using the way ID
    osm_query_res <- 
      osmdata::opq_osm_id(id = str_flatten(extracted_edges$way_id, collapse = ", "),
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
    
    ## Re-sequence OSM nodes  ==================================================
    osm_link_reorder_list <- list()
    
    for(i in 1:nrow(osm_link_geoms)){
      osm_link <- osm_link_geoms[i,] %>% 
        mutate(azimuth = map_dbl(geometry, ~st_avg_line_azimuth(.,4326)))
      
      osm_link_buffer <- osm_link %>% st_buffer(5)
      
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
    
    ## Summary table ===========================================================
    output_tbl <- 
      osm_link_reorder %>% 
      st_transform(2272) %>% 
      mutate(length_mi = as.numeric(st_length(geometry))/5280) %>% 
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
      select(-sequence) %>% 
      mutate(rt_number = tbl_rt_number,
             remix_number = tbl_remix_number,
             direction_id = direction,
             direction = case_when(direction_id==0 ~"Outbound",
                                   direction_id==1 ~"Inbound",
                                   TRUE ~as.character(NA)),
             start_stop_region = case_when(row_number()==1 ~route_start_region,
                                           row_number()==nrow(.) ~route_end_region,
                                           TRUE ~as.character(NA))) %>% 
      select(rt_number, remix_number, direction_id, direction, name, length_mi, turn_direction, start_stop_region)
    
    if(direction==0){
      direction_id = "OUTBOUND"
    } else if(direction==1){
      direction_id = "INBOUND"
    }
    
    output_tbl_dir_list[[direction_id]] <- output_tbl
    
    ## Summary text ==============================================================
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
    text_tbl <- 
      text_compiled %>% 
      as_tibble() %>% 
      mutate(rt_number = tbl_rt_number,
             remix_number = tbl_remix_number,
             direction_id = direction,
             direction = case_when(direction_id==0 ~"Outbound",
                                   direction_id==1 ~"Inbound",
                                   TRUE ~as.character(NA))) %>% 
      select(rt_number, remix_number, direction_id, direction, text=value)
    
    text_tbl_dir_list[[direction_id]] <- text_tbl
  }
  
  output_tbl_dir <- do.call(rbind, output_tbl_dir_list)
  output_text_dir <- do.call(rbind, text_tbl_dir_list)
  
  ## Export ====================================================================
  export_path <- 
    file.path(
      get_sharepoint_dir(),
      "SEPTA Bus Revolution - General/SEPTA CBNR-EXTERNAL/12 Tariff Sheets Summer 2023/export")
  
  export_tbl_file_name <- paste0(tbl_rt_number,"-",tbl_remix_number,".xlsx")
  export_txt_file_name <- paste0(tbl_rt_number,"-",tbl_remix_number,".xlsx")

  openxlsx::write.xlsx(output_tbl_dir,
                       file.path(export_path,
                                 "turn_by_turn_direction_table",
                                 export_tbl_file_name),
                       rowNames=FALSE)
  openxlsx::write.xlsx(output_text_dir,
                       file.path(export_path,
                                 "text_block",
                                 export_txt_file_name),
                       rowNames=FALSE)
}
