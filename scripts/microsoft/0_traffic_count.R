library(tidyverse)
library(sf)
library(tidytransit)
library(lubridate)
library(readxl)
library(nntools)
library(stringr)

# PURPOSE
# To identify how many trips (in/out) are occuring during the AM/PM Peak Hour

proj_dir <- "Confidential TDM ROI Analysis - General"
trip_cap_dir <- "Trip Cap Reporting/Count Data"


## READ CSV ====
count_dr1 <- read_excel(file.path(get_sharepoint_dir(),proj_dir,trip_cap_dir, "Driveway 1, East of Macon Ave_Volume.xlsx"), sheet = "Vol_One Day_15min")
count_dr2 <- read_excel(file.path(get_sharepoint_dir(),proj_dir,trip_cap_dir, "Driveway 2, South of Macon Ave_Volume.xlsx"), sheet = "Vol_One Day_15min")

count_dr1_15min <- count_dr1 %>% 
  mutate(total = EB + WB) %>% 
  pivot_wider(names_from = Date, values_from = c(EB, WB, total)) %>% 
  select(1,
         2,11,20,
         3,12,21,
         4,13,22) %>% 
  mutate(`EB_midweekAvg` = round((`EB_2022-09-13`+ `EB_2022-09-14` + `EB_2022-09-15`)/3, 2),
         `WB_midweekAvg` = round((`WB_2022-09-13`+ `WB_2022-09-14` + `WB_2022-09-15`)/3, 2),
         `total_midweekAvg` = round((`total_2022-09-13`+ `total_2022-09-14` + `total_2022-09-15`)/3, 2)) %>% 
  mutate(minus_15 = lag(total_midweekAvg, n = 1L, default = 0),
         minus_30 = lag(total_midweekAvg, n = 2L, default = 0),
         minus_45 = lag(total_midweekAvg, n = 3L, default = 0),
         lag_hour_total = total_midweekAvg + minus_15 + minus_30 + minus_45) %>% 
  select(-c(minus_15, minus_30, minus_45)) %>% 
  mutate(Time = str_split_fixed(as.character(Time), " ", 2)[,2])
write_csv(count_dr1_15min, "output/microsoft/Driveway 1, East of Macon Ave_Volume.csv")

count_dr2_15min <- count_dr2 %>% 
  mutate(total = NB + SB) %>% 
  pivot_wider(names_from = Date, values_from = c(NB, SB, total)) %>% 
  select(1,
         2,11,20,
         3,12,21,
         4,13,22) %>% 
  mutate(`NB_midweekAvg` = round((`NB_2022-09-13` + `NB_2022-09-14` + `NB_2022-09-15`)/3, 2),
         `SB_midweekAvg` = round((`SB_2022-09-13` + `SB_2022-09-14` + `SB_2022-09-15`)/3, 2),
         `total_midweekAvg` = round((`total_2022-09-13` + `total_2022-09-14` + `total_2022-09-15`)/3, 2)) %>% 
  mutate(minus_15 = lag(total_midweekAvg, n = 1L, default = 0),
         minus_30 = lag(total_midweekAvg, n = 2L, default = 0),
         minus_45 = lag(total_midweekAvg, n = 3L, default = 0),
         lag_hour_total = total_midweekAvg + minus_15 + minus_30 + minus_45) %>% 
  select(-c(minus_15, minus_30, minus_45)) %>% 
  mutate(Time = str_split_fixed(as.character(Time), " ", 2)[,2])
write_csv(count_dr2_15min, "output/microsoft/Driveway 2, South of Macon Ave_Volume.csv")




