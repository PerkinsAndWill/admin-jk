library(tidyverse)
library(sf)
library(nntools)
library(readxl)
library(leaflet)
library(htmlwidgets)
library(janitor)
library(arcgisbinding)
arc.check_product()

# Documentation/Installation
# https://valhalla.github.io/valhalla/api/isochrone/api-reference/
# https://cran.r-project.org/web/packages/spNetwork/vignettes/Isochrones.html

# Define Variables =============================================================
gdb_path <- "G:/Current/Pittsburgh_NexTransit_Network_Redesign_20230817/Analysis/GDB/Base.gdb"
nn_valhalla_hostname <- "128.199.8.29"
coord_global <- 4326
coord_local <- 2272

# Load data ====================================================================
# road_geom <- read_sf(gdb_path, layer="AlleghenyCounty_Roads")
sidewalk_geom <- read_rds("data/prt/sidewalks.rds") %>% 
  clean_names() %>% 
  mutate(length_ft = as.numeric(st_length(Shape))) %>% 
  select(objectid, type_name, road_name, length_ft, geometry=Shape)
acs_pop_emp <- read_sf(gdb_path, layer="cb_popemp_allegheny")
stops_geom <- read_rds("data/prt/stops_geom.rds")

stop_type <- read_excel(file.path("G:/Current/Pittsburgh_NexTransit_Network_Redesign_20230817",
                                  "Analysis/Spreadsheets/RouteandStopData_231108.xlsx"),
                        sheet="Stop_Data") %>% 
  select(stop_code=CleverID, stop_type=Stop_type, walkshed_dist=Walkshed) %>%
  mutate(stop_code=as.character(stop_code)) %>%
  distinct()

unique_stops_geom <- bind_cols(
  stops_geom %>% st_drop_geometry(),
  stops_geom %>% 
    st_coordinates() %>%
    as_tibble() %>%
    rename(lon = X, lat = Y)) %>%
  distinct(stop_code, stop_name, lon, lat) %>% 
  left_join(stop_type, by="stop_code") %>% 
  mutate(stop_type = ifelse(is.na(stop_type),"Bus Stop",stop_type),
         walkshed_dist = ifelse(is.na(walkshed_dist),0.25,walkshed_dist)) %>% 
  mutate(walkshed_dist_km = walkshed_dist*1.6093,
         walkshed_dist_ft = walkshed_dist*5280)

stops_union_buffer <- stops_geom %>% 
  st_transform(coord_local) %>% 
  st_buffer(5280) %>% 
  summarize(geometry = st_union(Shape))

sidewalk_geom <- sidewalk_geom[stops_union_buffer,] %>% 
  st_cast("LINESTRING")

# Isochrones ===================================================================
stops_iso <- list()

# OSM Streets Network
# for(i in 1:nrow(unique_stops_geom)){
#   
#   stop <- unique_stops_geom %>%
#     filter(row_number()==i)
#   
#   contour_dist <- stop$walkshed_dist_km
#   
#   single_iso <- stop %>% 
#     isochrone(costing = "pedestrian",
#               contours = c(contour_dist),
#               metric = "km",
#               hostname = nn_valhalla_hostname) %>% 
#     mutate(stop_code = stop$stop_code,
#            stop_name = stop$stop_name,
#            stop_type = stop$stop_type,
#            iso_dist_mi = stop$walkshed_dist) %>% 
#     select(stop_code, stop_name, stop_type, iso_dist_mi, geometry)
#   
#   print(paste0(i,"/6648"))
#   stops_iso[[i]] <- single_iso
# }

# Sidewalk Network
for(i in 1:nrow(unique_stops_geom)){
  stop <- unique_stops_geom %>%
    # filter(stop_code=="10023")
    filter(row_number()==i)
  
  stop_code <- stop$stop_code
  contour_dist <- stop$walkshed_dist_ft
  
  stop_sf <- stop %>% 
    st_as_sf(coords = c("lon","lat"), crs=4326) %>% 
    st_transform(coord_local)
  
  stop_sf_buffer <- stop_sf %>% st_buffer(5280/2)
  
  network_sub <- sidewalk_geom[stop_sf %>% st_buffer(5280/2),]
  
  tryCatch({
    iso_results <- calc_isochrones(lines = network_sub,
                                   start_points = stop_sf,
                                   dists = contour_dist,
                                   weight = "length_ft")
    # lines to polygons
    # identifying each isochrone
    iso_results$iso_oid <- paste(stop_code,
                                 iso_results$distance,
                                 sep = "_")
    
    # creating the polygons for each isochrone
    polygons <- lapply(unique(iso_results$iso_oid), function(oid){
      
      # subseting the required lines
      lines <- subset(iso_results, iso_results$iso_oid == oid)
      
      # extracting the coordinates of the lines
      coords <- st_coordinates(lines)
      poly_coords <- concaveman(points = coords, concavity = 3)
      poly <- st_polygon(list(poly_coords[,1:2]))
      return(poly)
    })
    
    # creating a SpatialPolygonsDataFrame
    iso_sp <- st_sf(
      iso_oid = unique(iso_results$iso_oid),
      distance = unique(iso_results$distance),
      geometry = polygons,
      crs = st_crs(iso_results)
    )
    
    stops_iso[[i]] <- iso_sp
    print(paste0(i,"/",nrow(unique_stops_geom)))
    
  }, error=function(e){skip_to_next <<- TRUE})
  
  if(skip_to_next){next}
  
  # tm_shape(network_sub) +
  #   tm_lines(col = "grey") +
  #   tm_shape(iso_results) + 
  #   tm_lines(col = "#9b2226") +
  #   # tm_lines(col = "fac_dist",title.col = "distance (m)",
  #   #          palette = c("500"="#005f73", "1000"="#ca6702", "2000"="#9b2226"))+
  #   tm_layout(legend.outside = TRUE) + 
    # tm_shape(stop_sf) +
    # tm_dots(col = "black", size = 0.1)
  
}

stops_iso_bind <- do.call(rbind, stops_iso)

write_rds(stops_iso_bind, "output/prt/prt_stops_sidewalk_isochrones.rds")


# Analysis ===================================================================
# gdb_meta <- arc.open(gdb_path)

stops_iso_bind <- read_rds("output/prt/prt_stops_sidewalk_isochrones.rds") %>% 
  mutate(stop_code = str_split_fixed(iso_oid,"_",2)[,1]) %>% 
  select(stop_code, distance, geometry)

frequency_cat_list <- c("Frequent","Semi-Frequent","Coverage")
day_of_week_list <- c("Weekday","Saturday","Sunday")
time_cat_list <- c("Midday","PM Peak")

df <- data.frame(frequency_cat  = rep(frequency_cat_list, each=6),
                 day_of_week = rep(day_of_week_list,each=2),
                 time_cat = rep(time_cat_list))
df_stops <- stops_geom %>% 
  st_drop_geometry() %>%
  mutate(headway_cat = case_when(headway <= 15 ~"15 minutes or less",
                                 headway > 15 & headway <= 30 ~"16 to 30 minutes",
                                 headway > 30 ~"31 minutes or more"))

ridership_apr2023_stop <- read_rds("output/prt/prt_ridership_stops_apr2023.rds") %>% 
  mutate(clever_id=as.character(clever_id))

access_analysis_list <- list()

for(i in frequency_cat_list){
  
  if(i=="Frequent"){
    headway_filter = c("15 minutes or less")
  } else if(i=="Semi-Frequent"){
    headway_filter = c("15 minutes or less","16 to 30 minutes")
  } else if(i=="Coverage"){
    headway_filter = c("15 minutes or less","16 to 30 minutes","31 minutes or more")
  }
  
  for(j in day_of_week_list){
    
    for(k in time_cat_list){
      
      dat <- df %>% 
        filter(frequency_cat==i &
                 day_of_week==j &
                 time_cat==k)
      
      sel_stops_iso <- df_stops %>% 
        filter(service_day==j &
                 time_cat==k) %>% 
        filter(headway_cat %in% headway_filter)
      
      avg_daily_boardings <- ridership_apr2023_stop %>% 
        filter(day_of_week==j & time_of_day==k) %>% 
        filter(clever_id %in% unique(sel_stops_iso$stop_code)) %>% 
        group_by(day_of_week, time_of_day) %>% 
        summarise(avg_daily_on = sum(avg_daily_on_adjusted,na.rm=TRUE),
                  n_stops = length(unique(clever_id))) %>% 
        ungroup()
      
      sel_stops_iso_union <- sel_stops_iso %>% 
        left_join(stops_iso_bind, by="stop_code") %>% 
        st_as_sf() %>% 
        st_transform(coord_local) %>% 
        summarise(geometry = st_union(geometry)) %>% 
        mutate(frequency_cat = i,
               day_of_week = j,
               time_cat = k) %>% 
        select(frequency_cat, day_of_week, time_cat, geometry)
      
      arc.write(file.path(gdb_path,
                          paste0("Analysis/sidewalk_walkshed_",
                                 str_to_lower(sub("-","_",i)),
                                 "_",str_to_lower(j),
                                 "_",sub(" ","_",str_to_lower(k)))),
                sel_stops_iso_union,
                overwrite = TRUE,
                validate = TRUE)
      
      # b <- st_intersection(sel_stops_iso_union, acs_pop_emp) %>% 
      #   mutate(area_prop = as.numeric(st_area(geometry))/Shape_Area) %>% 
      #   st_drop_geometry() %>% 
      #   mutate(area_prop = ifelse(area_prop>1,1,area_prop)) %>% 
      #   mutate(pop_20_prop = Pop_20 * area_prop,
      #          jobs_21_prop = Jobs_21 * area_prop) %>% 
      #   summarise(area_sqmi = sum(Area_sqmi*area_prop),
      #             population_2020 = sum(pop_20_prop),
      #             jobs_2021 = sum(jobs_21_prop),
      #             ridership_apr2023 = avg_daily_boardings$avg_daily_on,
      #             n_stops = avg_daily_boardings$n_stops)
      # 
      # dat <- dat %>% 
      #   mutate(area_sqmi = b$area_sqmi,
      #          population_2020 = b$population_2020,
      #          jobs_2021 = b$jobs_2021,
      #          ridership_apr2023 = b$ridership_apr2023,
      #          n_stops = b$n_stops)
      
      # access_analysis_list[[paste0(i,j,k)]] <- dat
      
      print(paste0(i,"-",j,"-",k))
    }
  }
}

access_analysis <- do.call(rbind, access_analysis_list) %>% 
  as_tibble()

access_analysis %>% clipr::write_clip()

 
# Leaflet map comparison =======================================================
road_geom <- read_sf(gdb_path, layer="AlleghenyCounty_Roads")
sidewalk_geom <- read_rds("data/prt/sidewalks.rds") %>% 
  clean_names() %>% 
  mutate(length_ft = as.numeric(st_length(Shape))) %>% 
  select(objectid, type_name, road_name, length_ft, geometry=Shape)

sample_stops <- stops_geom %>% 
  filter(service_day == 'Weekday' & time_cat == 'Midday') %>% 
  mutate(headway_cat = case_when(headway <= 15 ~"Frequent",
                                 headway > 15 & headway <= 30 ~"Semi-Frequent",
                                 headway > 30 ~"Coverage")) %>% 
  select(service_day, stop_code, stop_name, time_cat, headway_cat, Shape)


type = c('frequent','semi_frequent','coverage')
sidewalk_walksheds <- list()
for(type_id in type){
  a <- read_sf(gdb_path,
               layer=paste0('sidewalk_walkshed_', type_id, '_weekday_midday'))
  sidewalk_walksheds[[type_id]] <- a
  print(type_id)
}
sidewalk_walksheds <- do.call(rbind, sidewalk_walksheds) %>% 
  st_transform(4326)

road_walksheds <- list()
for(type_id in type){
  a <- read_sf(gdb_path,
               layer=paste0('walkshed_', type_id, '_weekday_midday'))
  road_walksheds[[type_id]] <- a
  print(type_id)
}
road_walksheds <- do.call(rbind, road_walksheds) %>% 
  st_transform(4326)

write_rds(sidewalk_walksheds, "output/prt/walksheds/isochrones_sidewalk.rds")
write_rds(road_walksheds, "output/prt/walksheds/isochrones_road.rds")

# network intersections
sw_freq <- st_intersection(sidewalk_geom,
                           sidewalk_walksheds %>% 
                             filter(frequency_cat=='Frequent') %>% 
                             st_transform(coord_local)) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  st_transform(coord_global)

sw_semi_freq <- st_intersection(sidewalk_geom,
                                sidewalk_walksheds %>% 
                                  filter(frequency_cat=='Semi-Frequent') %>% 
                                  st_transform(coord_local)) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  st_transform(coord_global)

sw_cov <- st_intersection(sidewalk_geom,
                          sidewalk_walksheds %>% 
                            filter(frequency_cat=='Coverage') %>% 
                            st_transform(coord_local)) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  st_transform(coord_global)

rd_freq <- st_intersection(road_geom,
                           road_walksheds %>% 
                             filter(frequency_cat=='Frequent') %>% 
                             st_transform(coord_local)) %>% 
  summarise(Shape = st_union(Shape)) %>%
  st_transform(coord_global)

rd_semi_freq <- st_intersection(road_geom,
                                road_walksheds %>% 
                                  filter(frequency_cat=='Semi-Frequent') %>% 
                                  st_transform(coord_local)) %>% 
  summarise(Shape = st_union(Shape)) %>%
  st_transform(coord_global)

rd_cov <- st_intersection(road_geom,
                          road_walksheds %>% 
                            filter(frequency_cat=='Coverage') %>% 
                            st_transform(coord_local)) %>% 
  summarise(Shape = st_union(Shape)) %>%
  st_transform(coord_global)

write_rds(sw_freq, "output/prt/walksheds/sidewalk_frequent.rds")
write_rds(sw_semi_freq, "output/prt/walksheds/sidewalk_semi_frequent.rds")
write_rds(sw_cov, "output/prt/walksheds/sidewalk_coverage.rds")
write_rds(rd_freq, "output/prt/walksheds/road_frequent.rds")
write_rds(rd_semi_freq, "output/prt/walksheds/road_semi_frequent.rds")
write_rds(rd_cov, "output/prt/walksheds/road_coverage.rds")

overlay_groups_lst <- c("Frequent Weekday Midday (Road)",
                        "Frequent Weekday Midday (Sidewalk)",
                        "Semi-Frequent Weekday Midday (Road)",
                        "Semi-Frequent Weekday Midday (Sidewalk)",
                        "Coverage Weekday Midday (Road)",
                        "Coverage Weekday Midday (Sidewalk)")
isochrones_map <-
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMapPane("230", zIndex = 230) %>%
  addMapPane("240", zIndex = 240) %>%
  addMapPane("250", zIndex = 250) %>%
  # Frequent Weekday Midday (Road)
  addPolylines(data = rd_freq,
               color = "#e7298a", opacity = 1, weight = 3,
               group = "Frequent Weekday Midday (Road)",
               options = pathOptions(pane="230")) %>%
  addPolygons(data = road_walksheds %>% filter(frequency_cat=="Frequent"),
              stroke = FALSE,
              fillColor = "#e7298a", fillOpacity = 0.4,
              group = "Frequent Weekday Midday (Road)",
              options = pathOptions(pane="240")) %>% 
  addCircleMarkers(data = sample_stops %>% filter(headway_cat=="Frequent"),
                   fillColor = "#b5b5b5", fillOpacity = 1, radius = 5,
                   color = "#060606", weight = 1, opacity = 1,
                   label = ~paste0(stop_code,": ",stop_name),
                   group = "Frequent Weekday Midday (Road)",
                   options = pathOptions(pane="250")) %>% 
  # Frequent Weekday Midday (Sidewalk)
  addPolylines(data = sw_freq,
               color = "#66a61e", opacity = 1, weight = 3,
               group = "Frequent Weekday Midday (Sidewalk)",
               options = pathOptions(pane="230")) %>%
  addPolygons(data = sidewalk_walksheds %>% filter(frequency_cat=="Frequent"),
              stroke = FALSE,
              fillColor = "#66a61e", fillOpacity = 0.4,
              group = "Frequent Weekday Midday (Sidewalk)",
              options = pathOptions(pane="240")) %>% 
  addCircleMarkers(data = sample_stops %>% filter(headway_cat=="Frequent"),
                   fillColor = "#b5b5b5", fillOpacity = 1, radius = 5,
                   color = "#060606", weight = 1, opacity = 1,
                   label = ~paste0(stop_code,": ",stop_name),
                   group = "Frequent Weekday Midday (Sidewalk)",
                   options = pathOptions(pane="250")) %>% 
  # Semi-Frequent Weekday Midday (Road)
  addPolylines(data = rd_semi_freq,
               color = "#e7298a", opacity = 1, weight = 3,
               group = "Semi-Frequent Weekday Midday (Road)",
               options = pathOptions(pane="230")) %>%
  addPolygons(data = road_walksheds %>% filter(frequency_cat=="Semi-Frequent"),
              stroke = FALSE,
              fillColor = "#e7298a", fillOpacity = 0.4,
              group = "Semi-Frequent Weekday Midday (Road)",
              options = pathOptions(pane="240")) %>% 
  addCircleMarkers(data = sample_stops %>% filter(headway_cat %in% c("Frequent","Semi-Frequent")),
                   fillColor = "#b5b5b5", fillOpacity = 1, radius = 5,
                   color = "#060606", weight = 1, opacity = 1,
                   label = ~paste0(stop_code,": ",stop_name),
                   group = "Semi-Frequent Weekday Midday (Road)",
                   options = pathOptions(pane="250")) %>% 
  # Semi-Frequent Weekday Midday (Sidewalk)
  addPolylines(data = sw_semi_freq,
               color = "#66a61e", opacity = 1, weight = 3,
               group = "Semi-Frequent Weekday Midday (Sidewalk)",
               options = pathOptions(pane="230")) %>%
  addPolygons(data = sidewalk_walksheds %>% filter(frequency_cat=="Semi-Frequent"),
              stroke = FALSE,
              fillColor = "#66a61e", fillOpacity = 0.4,
              group = "Semi-Frequent Weekday Midday (Sidewalk)",
              options = pathOptions(pane="240")) %>% 
  addCircleMarkers(data = sample_stops %>% filter(headway_cat %in% c("Frequent","Semi-Frequent")),
                   fillColor = "#b5b5b5", fillOpacity = 1, radius = 5,
                   color = "#060606", weight = 1, opacity = 1,
                   label = ~paste0(stop_code,": ",stop_name),
                   group = "Semi-Frequent Weekday Midday (Sidewalk)",
                   options = pathOptions(pane="250")) %>% 
  # Coverage Weekday Midday (Road)
  addPolylines(data = rd_cov,
               color = "#e7298a", opacity = 1, weight = 3,
               group = "Coverage Weekday Midday (Road)",
               options = pathOptions(pane="230")) %>%
  addPolygons(data = road_walksheds %>% filter(frequency_cat=="Coverage"),
              stroke = FALSE,
              fillColor = "#e7298a", fillOpacity = 0.4,
              group = "Coverage Weekday Midday (Road)",
              options = pathOptions(pane="240")) %>% 
  addCircleMarkers(data = sample_stops %>% filter(headway_cat %in% c("Frequent","Semi-Frequent","Coverage")),
                   fillColor = "#b5b5b5", fillOpacity = 1, radius = 5,
                   color = "#060606", weight = 1, opacity = 1,
                   label = ~paste0(stop_code,": ",stop_name),
                   group = "Coverage Weekday Midday (Road)",
                   options = pathOptions(pane="250")) %>% 
  # Coverage Weekday Midday (Sidewalk)
  addPolylines(data = sw_cov,
               color = "#66a61e", opacity = 1, weight = 3,
               group = "Coverage Weekday Midday (Sidewalk)",
               options = pathOptions(pane="230")) %>%
  addPolygons(data = sidewalk_walksheds %>% filter(frequency_cat=="Coverage"),
              stroke = FALSE,
              fillColor = "#66a61e", fillOpacity = 0.4,
              group = "Coverage Weekday Midday (Sidewalk)",
              options = pathOptions(pane="240")) %>% 
  addCircleMarkers(data = sample_stops %>% filter(headway_cat %in% c("Frequent","Semi-Frequent","Coverage")),
                   fillColor = "#b5b5b5", fillOpacity = 1, radius = 5,
                   color = "#060606", weight = 1, opacity = 1,
                   label = ~paste0(stop_code,": ",stop_name),
                   group = "Coverage Weekday Midday (Sidewalk)",
                   options = pathOptions(pane="250")) %>% 
  
  addLegend(position = "bottomright",
            colors = c("#e7298a","#66a61e"),
            labels = c("Road","Sidewalk"),
            title = "Mide",
            opacity = 1) %>%
  addLayersControl(overlayGroups = overlay_groups_lst,
                   options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(overlay_groups_lst)


saveWidget(isochrones_map,
           file=file.path(
             "output/prt/walksheds",
             "isochrones_leaflet.html"))
# # Export to S3
# put_object(file = "output/leaflet_maps/priority_sequence_leaflet.html",
#            object = "priority_sequence_leaflet.html",
#            bucket = "translink-s-r-public")

