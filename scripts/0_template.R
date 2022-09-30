library(tidyverse)
library(sf)
library(tidytransit)
library(lubridate)
library(readxl)

proj_dir <- "P:/M-R/PAAC General Planning Services 2021.0196"
proj_data_dir <- "06 Analysis/PAAC Origin-Destination Data"

## READ CSV ====
# read_csv()
# read_excel()
data_df <- read_csv(
  file.path(proj_dir,
            proj_data_dir,
            "paac-OD-data-dump.csv"))


## READ GTFS ====
gtfs <- read_gtfs("data/SunMetro/gtfs_v2.zip")
# Set servicepatern_id
gtfs <- set_servicepattern(gtfs)


## ACS ====
library(tigris)

state <- c("Tennessee")
county <- c("Davidson")
city <- c("Nashville")

## Block Groups ===============================================
blk_group_lst <- list()

for(i in 1:length(city)){
  lst_name <- city[i]
  
  a <- block_groups(state = state[i],
                    county = county[i],
                    year = 2020)# %>% 
    # st_transform(coord_local[i]) %>% 
    # mutate(area_acre = as.numeric(st_area(.)/43560)) %>% 
    # st_transform(coord_global) %>% 
    # select(GEOID, area_acre, geometry)
  # b <- a %>% st_centroid(.)
  # c <- b %>% st_intersection(., msa_lst[i][[1]])
  # d <- a %>% filter(GEOID %in% c$GEOID)
  
  tmp <- list(a)
  blk_group_lst[lst_name] <- tmp
}

# ACS CENSUS ===================================================================
census_variables <- load_variables(year = 2020, dataset = "acs5", cache = TRUE)

## B01003 - TOTAL POPULATION ===================================================
lst_pop_code <- c("B01001_001")
lst_pop_name <- c("total_population")

acs_tot_pop_lst <- list()

for(i in 1:length(city)){
  lst_name <- city[i]
  
  a <- get_acs(geography = "tract",
               variables = lst_pop_code,
               state = state[i],
               year = 2020) %>% 
    filter(GEOID %in% tracts_lst[[city[i]]]$GEOID) %>% 
    select(-moe) %>% 
    spread(., key = variable, value = estimate, sep = NULL) %>% 
    setnames(., old = lst_pop_code, new = lst_pop_name) %>% 
    left_join(tracts_lst[[city[i]]], by = "GEOID") %>% 
    st_as_sf() %>% 
    mutate(total_population_acre = total_population / area_acre)
  
  tmp <- list(a)
  acs_tot_pop_lst[lst_name] <- tmp
}


