library(tidyverse)
library(sf)
library(tidytransit)
library(lubridate)
library(readxl)
library(nntools)
library(wesanderson)
library(ggplot2)

proj_dir <- "P:/S-Z/UC SANTA CRUZ LRDP 2022.0063"
proj_data_dir <- file.path(proj_dir, "05 Background/Surveys")

# 2018 SURVEY ==================================================================
# LOAD DATA ====================================================================
data_df <- read_xlsx(
  file.path(proj_data_dir,
            "S18 Travel Survey Raw.xlsx"), skip=9)

colnames <- paste(names(data_df), data_df[1,], sep="!!")
survey_clean <- data_df[0:-1,]
names(survey_clean) <- colnames

modeshare_color <- c("#0d37ff", "#00b1ff", "#a9dbff", "#997500", "#c77942","#7e354d", 
                     "#a24463", "#bb5d7d", "#ca819a", "#d9a5b7", "#d61f3d", 
                     "#c0354d", "#ffc60b", "#ff850a", "#ea0b8c", "#59ff59", 
                     "#31c431", "#cfd0d1") #"#31c47a",
#  "#0D37FF","#00b1ff", "#a9dbff",

# PROCESS ======================================================================
## Rename columns ==============================================================
survey_id <- survey_clean[,1:25]
names(survey_id) <- str_split_fixed(names(survey_id), "!!", 2)[,2]
survey_id <- survey_id %>% 
  select(-c(Count, Assigned:Status, Faculty:Student, `Full time`:`Part time`,
            `On-Campus`:`Off-campus`)) %>% 
  mutate(`Response ID` = as.numeric(`Response ID`))

## 1. Travel to/from campus ====================================================
a <- survey_clean %>% 
  select(2, 26:115) %>% 
  # clean_names() %>% colnames()
  pivot_longer(cols = !`...2!!Response ID`,
               names_to = "pivot_column",
               values_to = "response") %>% 
  rename(`Response ID` = 1) %>% 
  mutate(mode = str_split_fixed(pivot_column, "!!", n=2)[,1],
         time = str_split_fixed(pivot_column, "!!", n=2)[,2]) %>% 
  mutate(mode = str_split_fixed(str_replace(mode, "[.]", "!!"), "!!", n=2)[,1]) %>% 
  mutate(mode = case_when(mode == "" ~as.character(NA),
                          TRUE ~mode)) %>% 
  mutate(time_order = case_when(time == "12-7am" ~1,
                                time == "7-9am" ~2,
                                time == "9-4pm" ~3,
                                time == "4-6pm" ~4,
                                time == "6-12" ~5)) %>% 
  mutate(response = case_when(response == "0" ~as.character(NA),
                              TRUE ~response)) %>% 
  select(1, mode, time, time_order, response) %>% 
  fill(mode) %>% 
  mutate(`Response ID` = as.numeric(`Response ID`),
         response = as.numeric(response))

### 1.1 Total Modeshare ========================================================
modeshare_total <- a %>%
  group_by(mode) %>% 
  summarise(n = sum(response, na.rm=TRUE)) %>% 
  # select(n) %>% sum()
  mutate(mode_pct = n/sum(n)) %>% 
  mutate(mode = factor(mode, levels = c("Metro", "Campus Transit", "Bike Shuttle",
                                        "Night Owl", "Motorcycle", "SOV", "2-MOV", "3-MOV", 
                                        "4+MOV", "Vanpool", "Zipcar SOV", "Zipcar MOV",
                                        "EV SOV", "EV MOV", "Lyft/Uber", "Walk", 
                                        "Bicycle", "Telecommute/teleconference")))

ggplot(modeshare_total, aes(fill=mode, x=1, y=mode_pct)) +
  geom_col() +
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
  labs(x="", y="") +
  geom_text(aes(x=1, y=mode_pct, label=paste0(mode, ": ", scales::percent(round(mode_pct,3))), group=mode),
            position=position_stack(vjust=0.5), angle=90, size = 3) +
  # geom_text(aes(x=time_period, y=util_rate, label=scales::percent(round(util_rate,2)), group=sample_time),
  #           position=position_dodge(width=0.9), vjust=-0.2, fontface="bold") +
  scale_fill_manual(values = modeshare_color) +
  coord_flip() +
  guides(fill=guide_legend(title="Travel Mode")) +
  ggtitle("To/From Campus: Overall Modeshare") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # legend.position="bottom",
        legend.title=element_text(size=8),
        legend.text=element_text(size=6))
ggsave(paste0("output/ucsc/2018_modeshare_1.png"), width=(800)*3, height=(500)*3, units="px")

### 1.2 Total Modeshare by Affiliate Group =====================================
affiliation_lst <- c("Staff", "Faculty", "Retiree", "Student")

modeshare_total_affiliation <- a %>% 
  filter(!is.na(response)) %>% 
  left_join(survey_id %>% select("Response ID", "Affiliation"), by = "Response ID") %>% 
  filter(!Affiliation == "Retiree") %>% 
  group_by(mode, Affiliation) %>%
  summarise(n = sum(response, na.rm=TRUE)) %>% 
  group_by(Affiliation) %>% 
  mutate(mode_pct = n/sum(n)) %>% 
  mutate(mode = factor(mode, levels = c("Metro", "Campus Transit", "Bike Shuttle",
                                        "Night Owl", "Motorcycle", "SOV", "2-MOV", "3-MOV", 
                                        "4+MOV", "Vanpool", "Zipcar SOV", "Zipcar MOV",
                                        "EV SOV", "EV MOV", "Lyft/Uber", "Walk", 
                                        "Bicycle", "Telecommute/teleconference")))

# modeshare_total_affiliation %>% 
#   filter(!Affiliation == "Retiree") %>% 
#   write.csv("output/ucsc/total_modeshare_afilliate_group.csv", row.names=FALSE)

ggplot(modeshare_total_affiliation, aes(fill=mode, x=Affiliation, y=mode_pct)) +
  geom_col() +
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
  labs(x="Affiliate Group", y="% Modeshare") +
  geom_text(aes(x=Affiliation, y=mode_pct,
                # label=scales::percent(round(mode_pct,2)),
                label=paste0(round(mode_pct*100,1), "%"),
                group=mode),
            position=position_stack(vjust=0.5), size = 3) +
  scale_fill_manual(values = modeshare_color) +
  guides(fill=guide_legend(title="Travel Mode")) +
  ggtitle("To/From Campus: Modeshare by Affiliate Group") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title=element_text(size=8),
        legend.text=element_text(size=6))
ggsave(paste0("output/ucsc/2018_modeshare_1-All.png"),
       width=(800)*3, height=(500)*3, units="px")


for(i in 1:length(affiliation_lst)){
  group_id <- affiliation_lst[i]
  
  dat <- modeshare_total_affiliation %>% filter(Affiliation == group_id)
  
  ggplot(dat, aes(fill=mode, x=1, y=mode_pct)) +
    geom_col() +
    scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
    labs(x="", y="") +
    geom_text(aes(x=1, y=mode_pct, label=paste0(mode, ": ", scales::percent(round(mode_pct,3))), group=mode),
              position=position_stack(vjust=0.5), angle=90, size = 3) +
    scale_fill_manual(values = modeshare_color) +
    coord_flip() +
    guides(fill=guide_legend(title="Travel Mode")) +
    ggtitle(paste0("To/From Campus: Modeshare by ", group_id)) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.title=element_text(size=8),
          legend.text=element_text(size=6))
  ggsave(paste0("output/ucsc/2018_modeshare_1-" ,group_id, ".png"),
         width=(800)*3, height=(500)*3, units="px")
}

### 1.3 Modeshare by Number of Modes ===========================================
modeshare_nummodes <- a %>% 
    filter(!is.na(response)) %>% 
    distinct(`Response ID`, mode) %>% 
    group_by(`Response ID`) %>% 
    mutate(num_mode = n()) %>% 
    group_by(mode, num_mode) %>% 
    summarise(count = n()) %>% 
    ungroup() %>%
    arrange(num_mode) %>% 
    mutate(mode = factor(mode, levels = c("Metro", "Campus Transit", "Bike Shuttle",
                                          "Night Owl", "SOV", "2-MOV", "3-MOV", "4+MOV",
                                          "Vanpool", "Zipcar SOV", "Zipcar MOV",
                                          "EV SOV", "EV MOV", "Lyft/Uber", 
                                          "Walk", "Bicycle", "Motorcycle",
                                          "Telecommute/teleconference")))
  
ggplot(modeshare_nummodes, aes(fill=mode, y=count, x=num_mode)) +
    geom_bar(position = "stack", stat = "identity") +
    # geom_col() +
    labs(x="Number of modes per response", y="") +
    scale_x_continuous(breaks = seq(1,7,by=1)) +
    scale_fill_manual(values = modeshare_color) +
    guides(fill=guide_legend(title="Travel Mode")) +
    ggtitle("To/From Campus: Modeshare by Number of Modes") +
    theme(legend.title=element_text(size=8),
          legend.text=element_text(size=6))
ggsave("output/ucsc/2018_modeshare_2.png", width=(800)*3, height=(500)*3, units="px")

### 1.4 Overall frequency, weekly ==============================================
# col_mode <- as.character(wes_palette("Darjeeling1", n = 18, type = c("continuous")))

mode_freq_time <- a %>% 
  filter(!is.na(response)) %>% 
  left_join(survey_id %>% select("Response ID", "Affiliation"), by = "Response ID") %>% 
  # count(Affiliation, mode, time, time_order) %>% 
  group_by(Affiliation, mode, time, time_order) %>% 
  summarise(n = sum(response)) %>% 
  arrange(mode, time_order) %>% 
  mutate(mode = factor(mode, levels = c("Metro", "Campus Transit", "Bike Shuttle",
                                        "Night Owl", "SOV", "2-MOV", "3-MOV", "4+MOV",
                                        "Vanpool", "Zipcar SOV", "Zipcar MOV",
                                        "EV SOV", "EV MOV", "Lyft/Uber", 
                                        "Walk", "Bicycle", "Motorcycle",
                                        "Telecommute/teleconference")))
mode_freq_time %>% filter(Affiliation == "Faculty", mode == "SOV")
mode_freq_time %>% write.csv("output/ucsc/frequency_modeshare_affiliate_group.csv", row.names = FALSE)

for(i in 1:length(affiliation_lst)){
  group_id <- affiliation_lst[i]
  
  dat <- mode_freq_time %>% filter(Affiliation == group_id)
  
  ggplot(dat, aes(color=mode, y=n, x=time_order)) +
    geom_line(size=1.4) +
    scale_x_continuous(labels = c("1"="12AM-7AM", "2"="7AM-9AM", "3"="9AM-4PM", "4"="4PM-6PM", "5"="6PM-12AM")) +
    scale_color_manual(values = modeshare_color) +
    labs(x="", y="") +
    guides(color=guide_legend(title="Travel Mode")) +
    ggtitle(paste0("To/From Campus: Weekly Mode Frequency by ", group_id)) +
    theme(legend.title=element_text(size=8),
          legend.text=element_text(size=6))

  ggsave(paste0("output/ucsc/2018_modefrequency_3-" ,group_id, ".png"),
         width=(800)*3, height=(500)*3, units="px")
}


## 2. Travel on campus ============================================================
b <- survey_clean %>% 
  select(2, 116:200) %>% 
  pivot_longer(cols = !`...2!!Response ID`,
               names_to = "pivot_column",
               values_to = "response") %>% 
  rename(`Response ID` = 1) %>% 
  mutate(mode = str_split_fixed(pivot_column, "!!", n=2)[,1],
         time = str_split_fixed(pivot_column, "!!", n=2)[,2]) %>% 
  mutate(mode = str_split_fixed(str_replace(mode, "[.]", "!!"), "!!", n=2)[,1]) %>% 
  mutate(mode = case_when(mode == "" ~as.character(NA),
                          TRUE ~mode)) %>% 
  mutate(time_order = case_when(time == "12-7am" ~1,
                                time == "7-9am" ~2,
                                time == "9-4pm" ~3,
                                time == "4-6pm" ~4,
                                time == "6-12" ~5)) %>% 
  mutate(response = case_when(response == "0" ~as.character(NA),
                              TRUE ~response)) %>% 
  select(1, mode, time, time_order, response) %>% 
  fill(mode) %>% 
  mutate(`Response ID` = as.numeric(`Response ID`),
         response = as.numeric(response))

### Modeshare ==================================================================
modeshare_total <- b %>% 
  filter(!is.na(response)) %>% 
  distinct(`Response ID`, mode) %>% 
  group_by(`Response ID`) %>% 
  mutate(num_mode = n()) %>% 
  group_by(mode, num_mode) %>% 
  summarise(count = n()) %>% 
  ungroup()

ggplot(modeshare_total, aes(fill=mode, y=count, x=num_mode)) +
  geom_col() +
  labs(x="Number of modes per response", y="") +
  scale_fill_manual(values = col_mode) +
  guides(fill=guide_legend(title="Travel Mode")) +
  ggtitle("")

ggsave("output/ucsc/0_on_campus_mode_share.png", width=(800)*3, height=(500)*3, units="px")


### Overall frequency, weekly ==================================================
col_mode <- as.character(wes_palette("Darjeeling1", n = 18, type = c("continuous")))

mode_freq_time <- b %>% 
  filter(!is.na(response)) %>% 
  count(mode, time, time_order) %>% 
  arrange(mode, time_order)

ggplot(mode_freq_time, aes(color=mode, y=n, x=time_order)) +
  geom_line(size=1.4) +
  scale_x_continuous(labels = c("1"="12AM-7AM", "2"="7AM-9AM", "3"="9AM-4PM", "4"="4PM-6PM", "5"="6PM-12AM")) +
  scale_color_manual(values = col_mode) +
  labs(x="", y="") +
  guides(fill=guide_legend(title="Travel Mode")) +
  ggtitle("Total Mode Frequency")

ggsave("output/ucsc/1_on_campus_frequency_all.png", width=(800)*3, height=(500)*3, units="px")


# 2019 SURVEY ==================================================================
# LOAD DATA ====================================================================
data_2019 <- read_xlsx(
  file.path(proj_data_dir,
            "S19 Travel Survey Raw.xlsx"), skip=7)

colnames <- paste(names(data_2019), data_2019[1,], sep="!!")
survey_clean <- data_2019[0:-1,]
names(survey_clean) <- colnames

modeshare_color <- c("#ffc60b", "#ff850a", "#ff470a", "#997500", "#7e354d", 
                     "#a24463", "#bb5d7d", "#ca819a", "#d9a5b7", "#d61f3d", 
                     "#c0354d", "#00b1ff", "#a9dbff", "#ea0b8c", "#7ac231", 
                     "#31c431", "#31c47a", "#cfd0d1")

# PROCESS ======================================================================
## Rename columns ==============================================================
survey_id <- survey_clean[,1:25]
names(survey_id) <- str_split_fixed(names(survey_id), "!!", 2)[,2]
# survey_id <- survey_id %>% 
#   select(-c(Count, Assigned:Status, Faculty:Student, `Full time`:`Part time`,
#             `On-Campus`:`Off-campus`)) %>% 
#   mutate(`Response ID` = as.numeric(`Response ID`))

## 1. Travel to/from campus ====================================================
a <- survey_clean %>% 
  select(1, 23:112) %>% 
  # clean_names() %>% colnames()
  pivot_longer(cols = !`...1!!Response ID`,
               names_to = "pivot_column",
               values_to = "response") %>% 
  rename(`Response ID` = 1) %>% 
  mutate(mode = str_split_fixed(pivot_column, "!!", n=2)[,1],
         time = str_split_fixed(pivot_column, "!!", n=2)[,2]) %>% 
  mutate(mode = str_split_fixed(str_replace(mode, "[.]", "!!"), "!!", n=2)[,1]) %>% 
  mutate(mode = case_when(mode == "" ~as.character(NA),
                          TRUE ~mode)) %>% 
  fill(mode) %>% 
  group_by(`Response ID`, mode) %>% 
  mutate(time_order = 1:n()) %>% 
  ungroup() %>% 
  mutate(time = case_when(time_order == 1 ~"12AM-7AM",
                          time_order == 2 ~"7AM-9AM",
                          time_order == 3 ~"9AM-4PM",
                          time_order == 4 ~"4PM-6PM",
                          time_order == 5 ~"6PM-12AM")) %>% 
  select(1, mode, time, time_order, response) %>% 
  mutate(`Response ID` = as.numeric(`Response ID`),
         response = as.numeric(response))
  
### 1.1 Total Modeshare ========================================================
mode_2019 <- a %>% distinct(mode)

modeshare_total <- a %>% 
  group_by(mode) %>% 
  summarise(n = sum(response, na.rm=TRUE)) %>% 
  # select(n) %>% sum()
  mutate(mode_pct = n/sum(n)) %>% 
  mutate(mode = factor(mode, levels = c("Metro", "Shuttles", "Bike Shuttle", "Jump Bike",
                                        "SOV", "MOV 2", "MOV 3", "MOV 4+",
                                        "Vanpool", "Zipcar SOV", "Zipcar 2+",
                                        "EV SOV", "EV 2+", "Rideshare", 
                                        "Walk", "Bicycle", "Motorcycle",
                                        "Telecommuting")))

ggplot(modeshare_total, aes(fill=mode, x=1, y=mode_pct)) +
  geom_col() +
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
  labs(x="", y="") +
  geom_text(aes(x=1, y=mode_pct, label=paste0(mode, ": ", scales::percent(round(mode_pct,2))), group=mode),
            position=position_stack(vjust=0.5), angle=90, size = 3) +
  # geom_text(aes(x=time_period, y=util_rate, label=scales::percent(round(util_rate,2)), group=sample_time),
  #           position=position_dodge(width=0.9), vjust=-0.2, fontface="bold") +
  scale_fill_manual(values = modeshare_color) +
  coord_flip() +
  guides(fill=guide_legend(title="Travel Mode")) +
  ggtitle("2019 To/From Campus: Overall Modeshare") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # legend.position="bottom",
        legend.title=element_text(size=8),
        legend.text=element_text(size=6))
ggsave(paste0("output/ucsc/2019_modeshare_1.png"), width=(800)*3, height=(500)*3, units="px")

  
  