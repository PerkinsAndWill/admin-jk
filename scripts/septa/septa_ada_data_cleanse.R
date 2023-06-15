library(tidyverse)
library(readxl)
library(clipr)

sheet_list <- c("SEP2022","OCT2022","NOV2022")

for(item in sheet_list){
  dat <- 
    read_xlsx("data/septa/SEPTA ADA Pickups Fall 2022 SEP_NOV.xlsx", sheet=item)
  
  dat %>% slice(1:3813) %>% view()
  dat %>% slice(3813:n()) %>% view()
  
  dat_processed <- 
    dat %>% 
    mutate(rider_id = case_when(str_detect(trip_id, ".\\(|\\).") ~as.character(trip_id),
                                TRUE ~as.character(rider_id))) %>% 
    fill(rider_id, .direction="down") %>% 
    filter(!is.na(trip_id), !str_detect(trip_id, ".\\(|\\).")) %>% #view()
    mutate(rider_name = str_split_fixed(rider_id,".\\(", 2)[,1],
           rider_id = str_split_fixed(rider_id,".\\(", 2)[,2],
           rider_id = str_remove(rider_id, ".\\)")) %>% 
    select(rider_id, rider_name, trip_id: type)
  
  write_clip(dat_processed)
}

dat <- 
  read_xlsx("data/septa/nov1.xlsx")

dat_processed <- 
  dat %>% 
  mutate(rider_id = case_when(str_detect(trip_id, ".\\(|\\).") ~as.character(trip_id),
                              TRUE ~as.character(rider_id))) %>% 
  fill(rider_id, .direction="down") %>% 
  filter(!trip_id=="", !str_detect(trip_id, ".\\(|\\).")) %>% 
  mutate(rider_name = str_split_fixed(rider_id,".\\(", 2)[,1],
         rider_id = str_split_fixed(rider_id,".\\(", 2)[,2],
         rider_id = str_remove(rider_id, ".\\)")) %>% 
  select(rider_id, rider_name, trip_id: type)

write_clip(dat_processed)

dat