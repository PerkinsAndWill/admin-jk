library(sf)
library(tidyverse)
library(leaflet)

coord_global <- 4236


clean_dc_merged_od_data <- function(streetlight_result_data, target_zones, year){
  cleaned_table <- streetlight_result_data %>%
    mutate(year = year,
           study_area = TRUE) %>% 
    rename(origin_zone_id = Origin.Zone.ID,
           destination_zone_id = Destination.Zone.ID,
           day_type =  Day.Type,
           day_part = Day.Part,
           avg_daily_od_volume = Average.Daily.O.D.Traffic..StL.Volume.) %>% 
    select(year,
           origin_zone_id,
           destination_zone_id,
           day_type,
           day_part,
           avg_daily_od_volume,
           study_area) %>% 
    filter(day_type == "3: Wednesday (W-W)",
           day_part == "0: All Day (12am-12am)") %>% 
    mutate(type = case_when(origin_zone_id %in% target_zones & !destination_zone_id %in% target_zones ~'exit',
                            !origin_zone_id %in% target_zones & destination_zone_id %in% target_zones ~'enter',
                            origin_zone_id %in% target_zones & destination_zone_id %in% target_zones ~'internal',
                            TRUE ~'external')) %>% 
    filter(type != 'external')
}


clean_dc_merged_za_data <- function(streetlight_result_data, target_zones, year){
  cleaned_table <- streetlight_result_data %>%
    mutate(year = year,
           study_area = FALSE) %>% 
    rename(zone_id = Zone.ID,
           zone_type = Zone.Type,
           day_type =  Day.Type,
           day_part = Day.Part,
           avg_daily_zone_volumne = Average.Daily.Zone.Traffic..StL.Volume.) %>% 
    select(year,
           zone_id,
           zone_type,
           day_type,
           day_part,
           avg_daily_zone_volumne,
           study_area) %>% 
    filter(zone_id %in% target_zones,
           day_type == "3: Wednesday (W-W)",
           day_part == "0: All Day (12am-12am)")
}



clean_dc_merged_od_data_pattern <- function(streetlight_result_data){
  cleaned_data <- streetlight_result_data %>% 
    mutate(study_area = TRUE) %>% 
    rename(origin_zone_id = Origin.Zone.ID,
           destination_zone_id = Destination.Zone.ID,
           day_type =  Day.Type,
           day_part = Day.Part,
           avg_daily_od_volume = Average.Daily.O.D.Traffic..StL.Volume.) %>% 
    select(origin_zone_id,
           destination_zone_id,
           day_type,
           day_part,
           avg_daily_od_volume) %>% 
    filter(origin_zone_id == 3 | destination_zone_id == 3) %>% 
    mutate(type = case_when(origin_zone_id == destination_zone_id ~'internal',
                            TRUE ~'external'))
  
  return(cleaned_data)
}


clean_dc_focused_od_data <- function(streetlight_result_data){
  cleaned_data <- streetlight_result_data %>% 
    rename(origin_zone_id = Origin.Zone.ID,
           destination_zone_id = Destination.Zone.ID,
           day_type =  Day.Type,
           day_part = Day.Part,
           avg_daily_od_volume = Average.Daily.O.D.Traffic..StL.Volume.) %>% 
    select(origin_zone_id,
           destination_zone_id,
           day_type,
           day_part,
           avg_daily_od_volume) %>% 
    filter(day_type == "3: Wednesday (W-W)",
           day_part == "0: All Day (12am-12am)",
           origin_zone_id %in% dc_zones | destination_zone_id %in% dc_zones) %>% 
    mutate(type = case_when(origin_zone_id %in% dc_zones & !destination_zone_id %in% dc_zones ~'exit',
                            !origin_zone_id %in% dc_zones & destination_zone_id %in% dc_zones ~'enter',
                            origin_zone_id == destination_zone_id ~'internal',
                            TRUE ~'external'))
  
}

clean_dc_downtown_od_data <- function(streetlight_result_data){
  cleaned_data <- streetlight_result_data %>% 
    rename(origin_zone_id = Origin.Zone.ID,
           destination_zone_id = Destination.Zone.ID,
           day_type =  Day.Type,
           day_part = Day.Part,
           avg_daily_od_volume = Average.Daily.O.D.Traffic..StL.Volume.) %>% 
    select(origin_zone_id,
           destination_zone_id,
           day_type,
           day_part,
           avg_daily_od_volume) %>% 
    filter(day_type == "3: Wednesday (W-W)",
           day_part == "0: All Day (12am-12am)",
           origin_zone_id %in% downtown_zone | destination_zone_id %in% downtown_zone) %>% 
    mutate(type = case_when(origin_zone_id %in% downtown_zone & !destination_zone_id %in% downtown_zone ~'exit',
                            !origin_zone_id %in% downtown_zone & destination_zone_id %in% downtown_zone ~'enter',
                            origin_zone_id == destination_zone_id ~'internal',
                            TRUE ~'external'))
  
}
