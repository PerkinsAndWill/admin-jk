library(tidycensus)
library(data.table)
library(janitor)
library(sf)
library(mapview)

coord_global <- 4326 

proj_dir <- "G:/Current/NASHVILLE_Downtown_Neighborhood_Traffic_Project_2020.1164/Analysis"
aadt_dir <- "z_Original/Replica/Davidson_county_Tennessee_aadt_2021/hex"

a <- st_read(file.path(proj_dir, aadt_dir, "Davidson_county_Tennessee_aadt_2021.shp"))

mapview(a, color="red", width=20)
