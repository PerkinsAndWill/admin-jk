library(tidyverse)
library(tigris)
library(sf)
library(rgdal)
library(mapview)

block_groups <- block_groups(state = "Hawaii", year=2019)
soi_block_groups <- block_groups %>% 
  filter(GEOID %in% c("150070405002", "150070405004",
                      "150070405005", "150070405003"))
mapview(soi_block_groups)

# ACS CENSUS ===================================================================
census_variables <- load_variables(year = 2019, dataset = "acs5", cache = TRUE)
census_variables %>% filter(str_detect(concept, "EMPLOYMENT"),
                            geography == "block group") %>% view()
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
