library(tidyverse)
library(sf)
library(tidytransit)
library(lubridate)
library(readxl)
library(nntools)
library(janitor)
library(ggplot)

# proj_dir <- file.path(get_sharepoint_dir(), "Līhue Civic Center Mobility Plan - General/LCCMP")
# parking_data_dir <- file.path(proj_dir, "04 Meetings/Charrette Week/Parking Materials")
# 
# read_excel(file.path(parking_data_dir, "LCMMP_ParkingSheet_20220628.xlsx"), sheet="Summary", encoding = "UTF-8")

data <- read_excel("data/Kauai/LCMMP_ParkingSheet_20220628.xlsx", sheet="Summary", skip=2) %>% 
  clean_names() %>% 
  slice(2:(nrow(.)-4)) %>%
  rename(street_type = off_on_street,
         lot_no = number,
         lot_owner = owner,
         lot_name = name_description,
         regulation_note = notes_on_regulations,
         count_summer_wed_am = wed_midday_10_am_june_29_2022_9,
         count_summer_wed_pm = wed_evening_5_pm_june_29_2022_10,
         count_summer_thu_am = thur_midday_10_am_june_30_2022_11,
         count_summer_thu_pm = thur_evening_5_pm_june_30_2022_12,
         count_school_wed_am = wed_midday_10_am_august_31_2022_13,
         count_school_wed_pm = wed_evening_5_pm_august_31_2022_14,
         count_school_thu_am = thur_midday_10_am_september_1_2022_15,
         count_school_thu_pm = thur_evening_5_pm_september_2_2022_16,
         util_summer_wed_am = wed_midday_10_am_june_29_2022_18,
         util_summer_wed_pm = wed_evening_5_pm_june_29_2022_19,
         util_summer_thu_am = thur_midday_10_am_june_30_2022_20,
         util_summer_thu_pm = thur_evening_5_pm_june_30_2022_21,
         util_school_wed_am = wed_midday_10_am_august_31_2022_22,
         util_school_wed_pm = wed_evening_5_pm_august_31_2022_23,
         util_school_thu_am = thur_midday_10_am_september_1_2022_24,
         util_school_thu_pm = thur_evening_5_pm_september_2_2022_25) %>% 
  select(street_type:ada_spaces, is_regulated,
         count_summer_wed_am:count_school_thu_pm,
         util_summer_wed_am:util_school_thu_pm)

util_by_owner <- data %>% 
  select(lot_no, lot_owner, is_regulated, util_summer_wed_am:util_school_thu_pm) %>% 
  mutate(wed_am = rowMeans(.[ , c("util_summer_wed_am","util_school_wed_am")], na.rm=TRUE),
         wed_pm = rowMeans(.[ , c("util_summer_wed_pm","util_school_wed_pm")], na.rm=TRUE),
         thu_am = rowMeans(.[ , c("util_summer_thu_am","util_school_thu_am")], na.rm=TRUE),
         thu_pm = rowMeans(.[ , c("util_summer_thu_pm","util_school_thu_pm")], na.rm=TRUE)) %>% 
  select(lot_no, lot_owner, is_regulated, wed_am:thu_pm) %>% 
  group_by(lot_owner) %>% 
  summarise(wed_am_min = min(wed_am), wed_am_avg = mean(wed_am), wed_am_max = max(wed_am),
            wed_pm_min = min(wed_pm), wed_pm_avg = mean(wed_pm), wed_pm_max = max(wed_pm),
            thu_am_min = min(thu_am), thu_am_avg = mean(thu_am), thu_am_max = max(thu_am),
            thu_pm_min = min(thu_pm), thu_pm_avg = mean(thu_pm), thu_pm_max = max(thu_pm))

util_by_reg <- data %>% 
  select(lot_no, lot_owner, is_regulated, util_summer_wed_am:util_school_thu_pm) %>% 
  mutate(wed_am = rowMeans(.[ , c("util_summer_wed_am","util_school_wed_am")], na.rm=TRUE),
         wed_pm = rowMeans(.[ , c("util_summer_wed_pm","util_school_wed_pm")], na.rm=TRUE),
         thu_am = rowMeans(.[ , c("util_summer_thu_am","util_school_thu_am")], na.rm=TRUE),
         thu_pm = rowMeans(.[ , c("util_summer_thu_pm","util_school_thu_pm")], na.rm=TRUE)) %>% 
  select(lot_no, lot_owner, is_regulated, wed_am:thu_pm) %>% 
  group_by(is_regulated) %>% 
  summarise(wed_am_min = min(wed_am), wed_am_avg = mean(wed_am), wed_am_max = max(wed_am),
            wed_pm_min = min(wed_pm), wed_pm_avg = mean(wed_pm), wed_pm_max = max(wed_pm),
            thu_am_min = min(thu_am), thu_am_avg = mean(thu_am), thu_am_max = max(thu_am),
            thu_pm_min = min(thu_pm), thu_pm_avg = mean(thu_pm), thu_pm_max = max(thu_pm))

util_by_reg %>% write.csv("temp.csv", row.names = FALSE)

data_pivot <- read.csv("temp.csv")

library(wesanderson)
col <- as.character(wes_palette("Zissou1", n = 3, type = c("continuous")))
col2 <- as.character(wes_palette("GrandBudapest2", n = 3, type = c("continuous")))
owner_type <- c("County", "State", "Other")
regulation_type <- c("Free", "Limited", "Reserved")

for (i in 1:length(regulation_type)){
  owner_id <- regulation_type[i]
  color_id <- col2[i]
  
  ggplot() +
    geom_ribbon(data = data_pivot %>% filter(X == owner_id),
                aes(time, ymin=min, ymax=max),
                fill = color_id, alpha=0.3) +
    geom_line(data = data_pivot %>% filter(X == owner_id),
              aes(time, avg),
              col = color_id, size = 1.5) +
    scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
    scale_x_continuous(labels = c("1"="Wednesday AM", "2"="Wednesday PM", "3"="Thursday AM", "4"="Thursday PM")) +
    labs(x = "", y = "") +
    ggtitle(paste0(owner_id, " Lots Utilization"))
  
  ggsave(paste0("output/kauai/regulation_", owner_id, "_plot.png"), width=(800)*3, height=(500)*3, units="px")
}


