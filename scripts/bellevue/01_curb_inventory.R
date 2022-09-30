library(sf)
library(tidyverse)
library(rgdal)
library(mapview)
library(arcgisbinding)
library(leaflet)
library(htmltools)
arc.check_product()

proj_path <- "G:/Current/BELLEVUE_Curb_Management_Plan_20210567/Analysis"
gdb_path <- "GIS/GDB/NN_bellevue_curb_management.gdb"

ogrListLayers(file.path(proj_path, gdb_path))

study_area <- st_read(file.path(proj_path, gdb_path), layer="study_area_boundary")
street_centerlines <- st_read(file.path(proj_path, gdb_path), layer="street_centerlines") %>% 
  filter(!ArterialClassification %in% c("Highway", "Highway Ramp"))# %>% 
  # st_intersection(study_area)

# subset streets
a <- street_centerlines[study_area,]
# offset streets
b <- a %>% st_buffer(30, endCapStyle = "FLAT") %>% st_cast("LINESTRING")
# intersection points
start_points <- a %>% 
  st_cast("LINESTRING") %>% 
  st_line_sample(sample = 0) %>% 
  st_cast("POINT") %>% 
  st_buffer(32)
end_points <- a %>% 
  st_cast("LINESTRING") %>% 
  st_line_sample(sample = 1) %>% 
  st_cast("POINT") %>% 
  st_buffer(32)
# library(maptools)
# points <- rbind(start_points, end_points)
# c <- st_difference(b, start_points)
# mapview(a, zcol = "ArterialClassification") +
start_points_2 <- start_points %>% 
  st_union() %>% 
  st_simplify()
end_points_2 <- end_points %>% 
  st_union() %>% 
  st_simplify()
c <- b %>% st_difference(start_points_2)
c <- c %>% st_difference(end_points_2)

mapview(c) +
  # mapview(start_points_2) +
  # mapview(end_points) +
  mapview(study_area, lwd = 5, alpha.regions = 0)

# arc.write(file.path(proj_path, gdb_path, "temp_curb2"), c, overwrite = FALSE)

# EDIT DATA =============
library(janitor)
blockfaces <- st_read(file.path(proj_path, gdb_path), layer="street_centerline_offset_30ft_filter") %>% 
  clean_names()

# library(tigris)
# be_blocks <- blocks(state = "Washington",
#                        county = "King",
#                        year = 2020)
# be_blocks_sub <- be_blocks %>% st_transform(coord_global) %>% st_intersection(study_area %>% st_transform(coord_global))
# mapview(be_blocks_sub)
# arc.write(file.path(proj_path, gdb_path, "acs_blocks"), be_blocks_sub, overwrite = FALSE)

mapview(a) + mapview(blockfaces, color = "red")
mapview(be_blocks) + mapview(study_area)

mapview(blockfaces)

blockfaces_edit <- blockfaces %>% 
  mutate(segment_id = 1:nrow(.),
         block_id = as.numeric(NA),
         sub_block_id = as.numeric(NA),
         main_street = as.character(NA),
         street_start = as.character(NA),
         street_end = as.character(NA),
         side_of_street = as.character(NA),
         feature_type = "No Parking",
         length_ft = round(as.numeric(st_length(.)),1)) %>% 
  select(segment_id, block_id, sub_block_id, main_street, street_start, street_end, 
         side_of_street, feature_type, length_ft, arterial_classification, Shape)
arc.write(file.path(proj_path, gdb_path, "curb_template"), blockfaces_edit, overwrite = FALSE)

# Leaflet map ====
base_boundary <- st_read(file.path(proj_path, gdb_path), layer="study_area_boundary") %>% 
  st_transform(coord_global)
# base_streets <- st_read(file.path(proj_path, gdb_path), layer="street_centerlines") %>% 
#   st_transform(coord_global) %>% 
#   st_union()
  # st_transform(2285) %>% 
  # st_intersection(base_boundary %>% st_transform(2285)) %>% 
  # st_transform(coord_global)
  # st_union() %>% 
  # st_as_sf()

base_blocks <- st_read(file.path(proj_path, gdb_path), layer="acs_blocks") %>% 
  st_transform(coord_global)

curb_downtown <- st_read(file.path(proj_path, "GIS/GDB/Bellevue_curb_inventory_may_2020.gdb"), layer="all_levels_inventory_tmp") %>% 
  st_transform(coord_global) 
curb_data <- st_read(file.path(proj_path, gdb_path), layer="curb_template") %>% 
  st_transform(coord_global) %>% filter(!is.na(main_street))
null_curb <- st_read(file.path(proj_path, gdb_path), layer="curb_template") %>% 
  st_transform(coord_global) %>% filter(is.na(main_street))


curb_map <- leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addMapPane("440", zIndex = 440) %>% 
  addMapPane("450", zIndex = 450) %>% 
  addMapPane("460", zIndex = 460) %>% 
  
  addPolygons(data = base_boundary,
              fillOpacity = 0,
              weight = 5, color = "black", opacity = 1,
              options = c(pathOptions(pane = "440")),
              group = "Study Area Boundary") %>% 
  addPolygons(data = base_blocks,
              fillOpacity = 0.2, fillColor = "blue",
              weight = 1, fill = "blue", opacity = 1,
              label = ~GEOID20,
              options = c(pathOptions(pane = "440")),
              group = "ACS Blocks") %>% 
  
  addPolylines(data = curb_downtown,
               weight = 3, opacity = 1,
               color = ~case_when(feature_type == "No Parking" ~"#e41a1c",
                                  feature_type == "Driveway" ~"#377eb8",
                                  feature_type == "Other (Notes)" ~"#a65628",
                                  feature_type == "Regulated Parking (assign time limit)" ~"#4daf4a",
                                  feature_type == "Plant Strip" ~"#984ea3",
                                  feature_type == "Bikeshare Zone" ~"#ff7f00",
                                  feature_type == "Bike Lane" ~"#ffff33"),
               label = ~lapply(as.list(paste0(
                 "<div><table>",
                 "<tr><th colspan='4' style=\"background-color:#666666; color: #ffffff;\">",
                 street_segment,
                 "</th></tr>",
                 "<tr><td>Block ID: </td>","<td>",block_id,"</td>","</tr>",
                 "<tr><td>Start Street: </td>","<td>",starting,"</td>","</tr>",
                 "<tr><td>End Street: </td>","<td>",ending_street,"</td>","</tr>",
                 "<tr><td>Side of Street: </td>","<td>",side_of_street,"</td>","</tr>",
                 "<tr><td>Feature Type: </td>","<td>",feature_type,"</td>","</tr>",
                 "</div></table>"
               )), HTML),
               options = c(lineCap = "butt", pathOptions(pane = "450")),
               group = "Downtown Curbs") %>% 

  addPolylines(data = curb_data,
               weight = 3, opacity = 1,
               color = ~case_when(feature_type == "No Parking" ~"#e41a1c"),
               label = ~lapply(as.list(paste0(
                 "<div><table>",
                 "<tr><th colspan='4' style=\"background-color:#666666; color: #ffffff;\">",
                 main_street,
                 "</th></tr>",
                 "<tr><td>Block ID: </td>","<td>",block_id,"</td>","</tr>",
                 "<tr><td>Start Street: </td>","<td>",street_start,"</td>","</tr>",
                 "<tr><td>End Street: </td>","<td>",street_end,"</td>","</tr>",
                 "<tr><td>Side of Street: </td>","<td>",side_of_street,"</td>","</tr>",
                 "<tr><td>Feature Type: </td>","<td>",feature_type,"</td>","</tr>",
                 "</div></table>"
               )), HTML),
               options = c(lineCap = "butt", pathOptions(pane = "450")),
               group = "Curb Data") %>% 
  
  addPolylines(data = null_curb,
               weight = 3, opacity = 1,
               color = "black",
               options = c(lineCap = "butt", pathOptions(pane = "450")),
               group = "Curb Data") %>% 
  
  addLegend("bottomright",
            colors = c("#e41a1c", "#377eb8", "#a65628", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33"),
            labels = c("No Parking", "Driveway", "Other (Notes)", "Regulated Parking (assign time limit)",
                       "Plant Strip", "Bikeshare Zone", "Bike Lane"),
            title = "Feature Types",
            opacity = 1) %>% 
  
  hideGroup(c("ACS Blocks", "Downtown Curbs")) %>% 
  addLayersControl(
    baseGroups = c("Curb Data"),
    overlayGroups = c("Study Area Boundary", "ACS Blocks", "Downtown Curbs"),
    options = layersControlOptions(collapsed = FALSE),
    position = "topright")



# POPULUS DATA =============
shared_proj_path <- file.path(get_sharepoint_dir(), "Bellevue Curb Management Plan - General/Shared")
shared_data_path <- "03 Background and Data/CLT data from Populus"
populus <- st_read(file.path(shared_proj_path, shared_data_path, "Loading Trends Map Data from 3_1_2019 to 10_2_2019.geojson"))
mapview(populus)
