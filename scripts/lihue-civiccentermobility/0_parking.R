library(tidyverse)
library(sf)
library(tidytransit)
library(lubridate)
library(readxl)
library(nntools)
library(janitor)
library(ggplot2)
library(leaflet)
library(mapview)
# proj_dir <- file.path(get_sharepoint_dir(), "Līhue Civic Center Mobility Plan - General/LCCMP")
# parking_data_dir <- file.path(proj_dir, "04 Meetings/Charrette Week/Parking Materials")
# 
# read_excel(file.path(parking_data_dir, "LCMMP_ParkingSheet_20220628.xlsx"), sheet="Summary", encoding = "UTF-8")


data <- read_excel("data/lihue-civiccentermobility/LCMMP_ParkingSheet_20220628.xlsx", sheet="Summary", skip=2) %>% 
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

parking_name <- data %>% 
  select(lot_no,lot_name)

parking_inventory <- data %>% 
  select(street_type:is_regulated)

parking_count <- data %>% 
  select(street_type:lot_owner, total_spaces, is_regulated, count_summer_wed_am:count_school_thu_pm)

parking_utilization <- data %>% 
  select(street_type:lot_owner, total_spaces, is_regulated, util_summer_wed_am:util_school_thu_pm)



## Average peak utilization rate by owner ======================================
library(wesanderson)
col <- as.character(wes_palette("GrandBudapest2", n = 4, type = c("continuous")))
owner_type <- c("County", "State", "Other")
regulation_type <- c("Free", "Limited", "Reserved")
street_type <- c("Off", "On")

util_owner_sample1 <- parking_count %>% 
  select(street_type:is_regulated, count_summer_wed_am:count_summer_thu_pm) %>% 
  pivot_longer(cols = !c("street_type", "lot_no", "lot_owner", "total_spaces", "is_regulated"),
               names_to = "time_period", values_to = "count") %>% 
  mutate(count = as.numeric(count),
         time_period = str_split_fixed(time_period, "_", 3)[,3],
         time_order = case_when(time_period == "wed_am" ~1,
                                time_period == "wed_pm" ~2,
                                time_period == "thu_am" ~3,
                                time_period == "thu_pm" ~4)) %>% 
  group_by(lot_owner, time_period, time_order) %>% 
  summarise(total_spaces = sum(total_spaces),
            occupied_count = sum(count),
            util_rate = occupied_count/total_spaces) %>% 
  ungroup() %>% 
  arrange(lot_owner, time_order)

util_owner_sample2 <- parking_count %>% 
  select(street_type:is_regulated, count_school_wed_am:count_school_thu_pm) %>% 
  pivot_longer(cols = !c("street_type", "lot_no", "lot_owner", "total_spaces", "is_regulated"),
               names_to = "time_period", values_to = "count") %>% 
  mutate(count = as.numeric(count),
         time_period = str_split_fixed(time_period, "_", 3)[,3],
         time_order = case_when(time_period == "wed_am" ~1,
                                time_period == "wed_pm" ~2,
                                time_period == "thu_am" ~3,
                                time_period == "thu_pm" ~4)) %>% 
  group_by(lot_owner, time_period, time_order) %>% 
  summarise(total_spaces = sum(total_spaces),
            occupied_count = sum(count),
            util_rate = occupied_count/total_spaces) %>% 
  ungroup() %>% 
  arrange(lot_owner, time_order)

util_owner <- util_owner_sample1 %>% 
  mutate(sample_time = "Summer Weekday") %>% 
  bind_rows(., util_owner_sample2 %>% mutate(sample_time = "School Weekday")) %>% 
  mutate(sample_time = factor(sample_time, levels = c("Summer Weekday", "School Weekday")),
         time_period = factor(time_period, levels = c("wed_am", "wed_pm", "thu_am", "thu_pm")))

for (i in 1:length(owner_type)){
  owner_id <- owner_type[i]
  # color_id <- col[i]
  
  data <- util_owner %>% 
    filter(lot_owner == owner_id)
  
  ggplot(data, aes(fill=sample_time, y=util_rate, x=time_period)) +
    geom_col(position="dodge") +
    geom_hline(yintercept=0.85, linetype=2, color="#006d9d", size=1.2) +
    scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
    scale_x_discrete(labels = c("wed_am"="Wednesday AM", "wed_pm"="Wednesday PM",
                                "thu_am"="Thursday AM", "thu_pm"="Thursday PM")) +
    labs(x="", y="") +
    scale_fill_manual(values = c("#00A08A", "#F98400")) +
    geom_text(aes(x=time_period, y=util_rate, label=scales::percent(round(util_rate,2)), group=sample_time),
              position=position_dodge(width=0.9), vjust=-0.2, fontface="bold") +
    guides(fill=guide_legend(title="Sample Time")) +
    ggtitle(paste0(owner_id, "-Owned Lots Utilization"))
  
  ggsave(paste0("output/lihue-civiccentermobility/owner_", owner_id, "_plot.png"), width=(800)*3, height=(500)*3, units="px")
}

## Average peak utilization rate by regulation =================================
util_reg_sample1 <- parking_count %>% 
  select(street_type:is_regulated, count_summer_wed_am:count_summer_thu_pm) %>% 
  pivot_longer(cols = !c("street_type", "lot_no", "lot_owner", "total_spaces", "is_regulated"),
               names_to = "time_period", values_to = "count") %>% 
  mutate(count = as.numeric(count),
         time_period = str_split_fixed(time_period, "_", 3)[,3],
         time_order = case_when(time_period == "wed_am" ~1,
                                time_period == "wed_pm" ~2,
                                time_period == "thu_am" ~3,
                                time_period == "thu_pm" ~4)) %>% 
  group_by(is_regulated, time_period, time_order) %>% 
  summarise(total_spaces = sum(total_spaces),
            occupied_count = sum(count),
            util_rate = occupied_count/total_spaces) %>% 
  ungroup() %>% 
  arrange(is_regulated, time_order)

util_reg_sample2 <- parking_count %>% 
  select(street_type:is_regulated, count_school_wed_am:count_school_thu_pm) %>% 
  pivot_longer(cols = !c("street_type", "lot_no", "lot_owner", "total_spaces", "is_regulated"),
               names_to = "time_period", values_to = "count") %>% 
  mutate(count = as.numeric(count),
         time_period = str_split_fixed(time_period, "_", 3)[,3],
         time_order = case_when(time_period == "wed_am" ~1,
                                time_period == "wed_pm" ~2,
                                time_period == "thu_am" ~3,
                                time_period == "thu_pm" ~4)) %>% 
  group_by(is_regulated, time_period, time_order) %>% 
  summarise(total_spaces = sum(total_spaces),
            occupied_count = sum(count),
            util_rate = occupied_count/total_spaces) %>% 
  ungroup() %>% 
  arrange(is_regulated, time_order) 

util_reg <- util_reg_sample1 %>% 
  mutate(sample_time = "Summer Weekday") %>% 
  bind_rows(., util_reg_sample2 %>% mutate(sample_time = "School Weekday")) %>% 
  mutate(sample_time = factor(sample_time, levels = c("Summer Weekday", "School Weekday")),
         time_period = factor(time_period, levels = c("wed_am", "wed_pm", "thu_am", "thu_pm")))

for (i in 1:length(regulation_type)){
  regulation_id <- regulation_type[i]
  # color_id <- col[i]
  
  data <- util_reg %>% 
    filter(is_regulated == regulation_id)
  
  ggplot(data, aes(fill=sample_time, y=util_rate, x=time_period)) +
    geom_col(position="dodge") +
    geom_hline(yintercept=0.85, linetype=2, color="#006d9d", size=1.2) +
    scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
    scale_x_discrete(labels = c("wed_am"="Wednesday AM", "wed_pm"="Wednesday PM",
                                "thu_am"="Thursday AM", "thu_pm"="Thursday PM")) +
    labs(x="", y="") +
    scale_fill_manual(values = c("#00A08A", "#F98400")) +
    geom_text(aes(x=time_period, y=util_rate, label=scales::percent(round(util_rate,2)), group=sample_time),
              position=position_dodge(width=0.9), vjust=-0.2, fontface="bold") +
    guides(fill=guide_legend(title="Sample Time")) +
    ggtitle(paste0(regulation_id, " Lots Utilization"))
  
  ggsave(paste0("output/lihue-civiccentermobility/regulation_", regulation_id, "_plot.png"), width=(800)*3, height=(500)*3, units="px")
}

## Average peak utilization rate by street type ================================
util_type_sample1 <- parking_count %>% 
  select(street_type:is_regulated, count_summer_wed_am:count_summer_thu_pm) %>% 
  pivot_longer(cols = !c("street_type", "lot_no", "lot_owner", "total_spaces", "is_regulated"),
               names_to = "time_period", values_to = "count") %>% 
  mutate(count = as.numeric(count),
         time_period = str_split_fixed(time_period, "_", 3)[,3],
         time_order = case_when(time_period == "wed_am" ~1,
                                time_period == "wed_pm" ~2,
                                time_period == "thu_am" ~3,
                                time_period == "thu_pm" ~4)) %>% 
  group_by(street_type, time_period, time_order) %>% 
  summarise(total_spaces = sum(total_spaces),
            occupied_count = sum(count),
            util_rate = occupied_count/total_spaces) %>% 
  ungroup() %>% 
  arrange(street_type, time_order)

util_type_sample2 <- parking_count %>% 
  select(street_type:is_regulated, count_school_wed_am:count_school_thu_pm) %>% 
  pivot_longer(cols = !c("street_type", "lot_no", "lot_owner", "total_spaces", "is_regulated"),
               names_to = "time_period", values_to = "count") %>% 
  mutate(count = as.numeric(count),
         time_period = str_split_fixed(time_period, "_", 3)[,3],
         time_order = case_when(time_period == "wed_am" ~1,
                                time_period == "wed_pm" ~2,
                                time_period == "thu_am" ~3,
                                time_period == "thu_pm" ~4)) %>% 
  group_by(street_type, time_period, time_order) %>% 
  summarise(total_spaces = sum(total_spaces),
            occupied_count = sum(count),
            util_rate = occupied_count/total_spaces) %>% 
  ungroup() %>% 
  arrange(street_type, time_order) 

util_type <- util_type_sample1 %>% 
  mutate(sample_time = "Summer Weekday") %>% 
  bind_rows(., util_type_sample2 %>% mutate(sample_time = "School Weekday")) %>% 
  mutate(sample_time = factor(sample_time, levels = c("Summer Weekday", "School Weekday")),
         time_period = factor(time_period, levels = c("wed_am", "wed_pm", "thu_am", "thu_pm")))

for (i in 1:length(street_type)){
  type_id <- street_type[i]
  # color_id <- col[i]
  
  data <- util_type %>% 
    filter(street_type == type_id)
  
  ggplot(data, aes(fill=sample_time, y=util_rate, x=time_period)) +
    geom_col(position="dodge") +
    geom_hline(yintercept=0.85, linetype=2, color="#006d9d", size=1.2) +
    scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
    scale_x_discrete(labels = c("wed_am"="Wednesday AM", "wed_pm"="Wednesday PM",
                                "thu_am"="Thursday AM", "thu_pm"="Thursday PM")) +
    labs(x="", y="") +
    scale_fill_manual(values = c("#00A08A", "#F98400")) +
    geom_text(aes(x=time_period, y=util_rate, label=scales::percent(round(util_rate,2)), group=sample_time),
              position=position_dodge(width=0.9), vjust=-0.2, fontface="bold") +
    guides(fill=guide_legend(title="Sample Time")) +
    ggtitle(paste0(type_id, "-Street Parking Utilization"))
  
  ggsave(paste0("output/lihue-civiccentermobility/type_", type_id, "_plot.png"), width=(800)*3, height=(500)*3, units="px")
}

## On-Street average peak-utilization ==========================================
on_street_sample1 <- parking_count %>% 
  filter(street_type == "On") %>% 
  left_join(parking_name, by="lot_no") %>% 
  select(street_type, lot_no, lot_name, total_spaces, count_summer_wed_am:count_summer_thu_pm) %>% 
  pivot_longer(cols = !c("street_type", "lot_no", "lot_name", "total_spaces"),
               names_to = "time_period", values_to = "count") %>% 
  mutate(count = as.numeric(count),
         time_period = str_split_fixed(time_period, "_", 4)[,4],
         time_order = case_when(time_period == "am" ~1,
                                time_period == "pm" ~2)) %>% 
  group_by(street_type, lot_no, lot_name, time_period, time_order) %>% 
  summarise(total_spaces = mean(total_spaces),
            occupied_count = mean(count),
            util_rate = occupied_count/total_spaces) %>% 
  ungroup() %>% 
  arrange(lot_no, time_order)

on_street_sample2 <- parking_count %>% 
  filter(street_type == "On") %>% 
  left_join(parking_name, by="lot_no") %>% 
  select(street_type, lot_no, lot_name, total_spaces, count_school_wed_am:count_school_thu_pm) %>% 
  pivot_longer(cols = !c("street_type", "lot_no", "lot_name", "total_spaces"),
               names_to = "time_period", values_to = "count") %>% 
  mutate(count = as.numeric(count),
         time_period = str_split_fixed(time_period, "_", 4)[,4],
         time_order = case_when(time_period == "am" ~1,
                                time_period == "pm" ~2)) %>% 
  group_by(street_type, lot_no, lot_name, time_period, time_order) %>% 
  summarise(total_spaces = mean(total_spaces),
            occupied_count = mean(count),
            util_rate = occupied_count/total_spaces) %>% 
  ungroup() %>% 
  arrange(lot_no, time_order)

on_street <- on_street_sample1 %>% 
  mutate(sample_time = "Summer Weekday") %>% 
  bind_rows(., on_street_sample2 %>% mutate(sample_time = "School Weekday")) %>% 
  mutate(sample_time = factor(sample_time, levels = c("Summer Weekday", "School Weekday")),
         time_period = factor(time_period, levels = c("am", "pm")),
         time = paste(sample_time, toupper(time_period))) %>% 
  mutate(time = factor(time, levels = c("Summer Weekday AM", "Summer Weekday PM", 
                                        "School Weekday AM", "School Weekday PM")))

ggplot(on_street, aes(x=lot_name, y=util_rate, fill=time)) +
  geom_col(position="dodge") +
  geom_hline(yintercept=0.85, linetype=2, color="#006d9d", size=1.2) +
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
  labs(x="", y="") +
  scale_fill_manual(values = c("#3B9AB2", "#78B7C5", "#E1AF00", "#EBCC2A")) +
  guides(fill=guide_legend(title="Sample Time")) +
  ggtitle("On-Street Parking Utilization by Lot and Time of Day") +
  theme(axis.text.x = element_text(size=5, angle=90, vjust=0.5, hjust=1))

# ggsave(paste0("output/lihue-civiccentermobility/on_street_by_lot_plot.png"), width=(800)*3, height=(500)*3, units="px")

## Mapping =====
coord_local <- 26964
coord_global <- 4326
gdb_path <- "G:/Current/KAUAI_Lihue_Civic_Center_Mobility_Plan_2022.0969/Analysis/GDB/LihueMobility_GDB.gdb"
ogrListLayers(gdb_path)

on_street_sf <- st_read(gdb_path, layer = "on_street") %>% 
  st_zm(drop=TRUE) %>% 
  mutate(lot_no = str_split_fixed(Name, " ", 2)[,2]) %>% 
  select(lot_no, Shape)

off_street_sf <- st_read(gdb_path, layer = "off_street") %>% 
  st_zm(drop=TRUE) %>% 
  mutate(lot_no = str_split_fixed(Name, " ", 2)[,2]) %>% 
  mutate(lot_no = toupper(lot_no)) %>% 
  select(lot_no, Shape)

parking_attr <- data %>% 
  select(street_type:is_regulated)
parking_util <- data %>% 
  select(count_summer_wed_am:util_school_thu_pm)

a <- data %>% select(util_school_wed_am)
quantile(a$util_school_wed_am)

c("1-30%", "30-50%", "50-70%", "70-85%", "85-90%", "90-95%", "95+%")
util_pal <- c("#50c5ff","#00a4b9","#006d9d", "#4ea652", "#ffcb05", "#f47d20", "#d63f3e")

on_street <- data %>% 
  inner_join(on_street_sf, by="lot_no") %>% 
  st_as_sf() %>% 
  st_transform(coord_global)
off_street <- data %>% 
  inner_join(off_street_sf, by="lot_no") %>% 
  st_as_sf() %>% 
  st_transform(coord_global)

on_street_util <- on_street %>% 
  select(street_type:is_regulated, util_summer_wed_am:util_school_thu_pm) %>% 
  pivot_longer(cols = starts_with("util_"),
               names_to = "sample_time",
               values_to = "utilization")
off_street_util <- off_street %>% 
  select(street_type:is_regulated, util_summer_wed_am:util_school_thu_pm) %>% 
  pivot_longer(cols = starts_with("util_"),
               names_to = "sample_time",
               values_to = "utilization")



on_street_util <- on_street_util %>% 
  mutate(html_label = paste0(
    "<div>",
    "<table>",
    "<tr>","<th colspan='4' style=\"background-color:#666666; color: #ffffff;\">",
    lot_no, ": ", lot_name, 
    "</th>","</tr>",
    "<tr>","<td>Lot Owner: </td>","<td>",lot_owner,"</td>","</tr>",
    "<tr>","<td>Total Spaces: </td>","<td>",total_spaces,"</td>","</tr>",
    "</table>",
    "</div>"
  ))
labs_on_street <- as.list(on_street_util$html_label)

off_street_util <- off_street_util %>% 
  mutate(html_label = paste0(
    "<div>",
    "<table>",
    "<tr>","<th colspan='4' style=\"background-color:#666666; color: #ffffff;\">",
    lot_no, ": ", lot_name, 
    "</th>","</tr>",
    "<tr>","<td>Lot Owner: </td>","<td>",lot_owner,"</td>","</tr>",
    "<tr>","<td>Total Spaces: </td>","<td>",total_spaces,"</td>","</tr>",
    "</table>",
    "</div>"
  ))
labs_off_street <- as.list(off_street_util$html_label)

write_rds(on_street_util, "data/lihue-civiccentermobility/on_street.rds")
write_rds(off_street_util, "data/lihue-civiccentermobility/off_street.rds")

library(htmltools)
library(htmlwidgets)

leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  
  addPolylines(data = on_street_util %>% filter(sample_time == "util_school_wed_am"),
               weight = 2,
               color = ~case_when(utilization >= .01 & utilization < .3 ~"#bdd7e7",
                                  utilization >= .3 & utilization < .5 ~"#6baed6",
                                  utilization >= .5 & utilization < .7 ~"#2171b5",
                                  utilization >= .7 & utilization < .85 ~"#4ea652",
                                  utilization >= .85 & utilization < .9 ~"#ffcb05",
                                  utilization >= .9 & utilization < .95 ~"#f47d20",
                                  utilization >= .95 ~"#d63f3e",
                                  TRUE ~"#cccccc"),
               opacity = 1,
               label = ~lapply(as.list(paste0(
                 "<div>",
                 "<table>",
                 "<tr>","<th colspan='4' style=\"background-color:#666666; color: #ffffff;\">",
                 lot_no, ": ", lot_name, 
                 "</th>","</tr>",
                 "<tr>","<td>Lot Owner: </td>","<td>",lot_owner,"</td>","</tr>",
                 "<tr>","<td>Total Spaces: </td>","<td>",total_spaces,"</td>","</tr>",
                 "</table>",
                 "</div>"
               )), HTML),
               # label = lapply(labs_on_street, HTML),
               group = "School Wednesday AM") %>% 
  addPolygons(data = off_street_util %>% filter(sample_time == "util_school_wed_am"),
              stroke = FALSE,
              fillColor = ~case_when(utilization >= .01 & utilization < .3 ~"#bdd7e7",
                                     utilization >= .3 & utilization < .5 ~"#6baed6",
                                     utilization >= .5 & utilization < .7 ~"#2171b5",
                                     utilization >= .7 & utilization < .85 ~"#4ea652",
                                     utilization >= .85 & utilization < .9 ~"#ffcb05",
                                     utilization >= .9 & utilization < .95 ~"#f47d20",
                                     utilization >= .95 ~"#d63f3e",
                                     TRUE ~"#cccccc"),
              fillOpacity = 1,
              label = ~lapply(as.list(paste0(
                "<div>",
                "<table>",
                "<tr>","<th colspan='4' style=\"background-color:#666666; color: #ffffff;\">",
                lot_no, ": ", lot_name, 
                "</th>","</tr>",
                "<tr>","<td>Lot Owner: </td>","<td>",lot_owner,"</td>","</tr>",
                "<tr>","<td>Total Spaces: </td>","<td>",total_spaces,"</td>","</tr>",
                "</table>",
                "</div>"
              )), HTML),
              group = "School Wednesday AM") %>% 
  
  addPolylines(data = on_street_util %>% filter(sample_time == "util_school_wed_pm"),
               weight = 2,
               color = ~case_when(utilization >= .01 & utilization < .3 ~"#bdd7e7",
                                  utilization >= .3 & utilization < .5 ~"#6baed6",
                                  utilization >= .5 & utilization < .7 ~"#2171b5",
                                  utilization >= .7 & utilization < .85 ~"#4ea652",
                                  utilization >= .85 & utilization < .9 ~"#ffcb05",
                                  utilization >= .9 & utilization < .95 ~"#f47d20",
                                  utilization >= .95 ~"#d63f3e",
                                  TRUE ~"#cccccc"),
               opacity = 1,
               label = lapply(labs_on_street, HTML),
               group = "School Wednesday PM") %>% 
  addPolygons(data = off_street_util %>% filter(sample_time == "util_school_wed_pm"),
              stroke = FALSE,
              fillColor = ~case_when(utilization >= .01 & utilization < .3 ~"#bdd7e7",
                                     utilization >= .3 & utilization < .5 ~"#6baed6",
                                     utilization >= .5 & utilization < .7 ~"#2171b5",
                                     utilization >= .7 & utilization < .85 ~"#4ea652",
                                     utilization >= .85 & utilization < .9 ~"#ffcb05",
                                     utilization >= .9 & utilization < .95 ~"#f47d20",
                                     utilization >= .95 ~"#d63f3e",
                                     TRUE ~"#cccccc"),
              fillOpacity = 1,
              label = lapply(labs_off_street, HTML),
              group = "School Wednesday PM") %>% 
  
  addLegend(position = "bottomleft",
            colors = c("#d63f3e", "#f47d20", "#ffcb05", "#4ea652", "#2171b5", "#6baed6", "#bdd7e7", "#cccccc"),
            labels = c("95%+", "90-95%", "85-90%", "70-85%", "50-70%", "30-50%", "1-30%", "NA"),
            title = "Parking Utilization Rate",
            opacity = 1) %>% 
  addLayersControl(
    baseGroups = c("School Wednesday AM",
                   "School Wednesday PM"),
    options = layersControlOptions(collapsed=FALSE)
  )
  
