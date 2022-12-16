library(tidyverse)
library(readxl)
library(nntools)
library(clipr)
library(janitor)
library(scales)
library(ggrepel)

# LOAD =========================================================================
## Surveys ====
proj_dir <- file.path(get_sharepoint_dir(), 
                      "SFCTA - School Access Study - Documents/General/06 Analysis/Surveys")

survey_en <- read_excel(file.path(proj_dir,
                                  "English/Data_All_Responses/Excel/San Francisco School Access Plan Survey.xlsx")) %>% 
  mutate(language = "English")
survey_es <- read_excel(file.path(proj_dir,
                                  "Spanish/Data_All_Responses/Excel/San Francisco Plan de Acceso Escolar Encuesta.xlsx")) %>% 
  add_column(., `...15` = as.character(NA), .after = 14) %>% 
  add_column(., `...19` = as.character(NA), .after = 18) %>% 
  mutate(language = "Spanish")
survey_cn <- read_excel(file.path(proj_dir,
                                  "Chinese/Data_All_Responses_Chinese/Excel/Sheet1.xlsx")) %>% 
  add_column(., `...15` = as.character(NA), .after = 14) %>% 
  add_column(., `...19` = as.character(NA), .after = 18) %>% 
  mutate(language = "Chinese")

survey_es_other <- read_excel(file.path(proj_dir,
                                        "TASAP_SurveyResponses_forTranslation_11302022.xlsx"),
                              sheet="Spanish") %>% 
  mutate(language = "Spanish") %>% 
  rename(answer_text_other_es=4, value=3)
survey_cn_other <- read_excel(file.path(proj_dir,
                                        "TASAP_SurveyResponses_forTranslation_11302022.xlsx"),
                              sheet="Chinese") %>% 
  mutate(language = "Chinese") %>% 
  rename(answer_text_other_cn=4, value=3)

## Schema ====
questions <- read_excel("data/sfcta/survey_schema.xlsx", sheet="questions")
answers <- read_excel("data/sfcta/survey_schema.xlsx", sheet="answers")
answers_tbl <- bind_rows(answers %>% 
                           select(question_num, answer_num, answer_en, answer_es) %>% 
                           rename(value=answer_es, answer_text=answer_en) %>% 
                           mutate(language="Spanish"),
                         answers %>% 
                           select(question_num, answer_num, answer_en, answer_cn) %>% 
                           rename(value=answer_cn, answer_text=answer_en) %>% 
                           mutate(language="Chinese")) %>% 
  rename(answer_order = answer_num)


sub_question_text <- survey_en %>% 
  slice(1) %>% 
  mutate(across(everything(), as.character)) %>% 
  pivot_longer(everything(), names_to = "question_text", values_to = "sub_question_text") %>%
  mutate(col_num = 1:nrow(.))


# sub_question_text <- survey_en %>%
#   slice(1) %>%
#   mutate(across(everything(), as.character)) %>%
#   pivot_longer(everything(), names_to = "col_num", values_to = "sub_question_text") %>%
#   mutate(col_num = 1:nrow(.))
# 
# col_defs <- colnames(survey_en) %>%
#   as_tibble() %>%
#   mutate(col_num = 1:nrow(.)) %>%
#   left_join(sub_question_text, by="col_num") %>%
#   rename(question_text = value) %>%
#   select(col_num, question_text, sub_question_text)


# TRANSFORM ====================================================================
colnames(survey_en) = c("respondent_id", as.character(2:ncol(survey_en)))
colnames(survey_es) = c("respondent_id", as.character(2:ncol(survey_es)))
colnames(survey_cn) = c("respondent_id", as.character(2:ncol(survey_cn)))

survey_bind <- bind_rows(survey_en, survey_es, survey_cn) %>% 
  filter(!is.na(respondent_id)) %>% 
  gather(col_num, value, -respondent_id)

## Define Respondent IDs (Language) ====
respondent_language_id <- survey_bind %>% 
  filter(col_num == 69) %>% 
  rename(language = 3) %>% 
  select(-col_num)

## Define Survey Variable ====
survey_df <- survey_bind %>% 
  mutate(col_num = as.numeric(col_num)) %>% 
  arrange(respondent_id, col_num) %>% 
  filter(!col_num == 69) %>% 
  
  left_join(sub_question_text, by="col_num") %>% 
  left_join(respondent_language_id, by="respondent_id") %>% 
  left_join(questions %>% select(col_num, question_num), by="col_num") %>% 
  
  left_join(answers_tbl, by=c("language","question_num","value")) %>% 
  left_join(survey_es_other, by=c("respondent_id","language","col_num","value")) %>% 
  left_join(survey_cn_other, by=c("respondent_id","language","col_num","value")) %>% 
  
  mutate(temp_col = coalesce(answer_text, answer_text_other_es, answer_text_other_cn)) %>% 
  
  mutate(answer_text = case_when(is.na(temp_col) ~value,
                                 TRUE ~temp_col)) %>% 
  
  select(respondent_id, language, col_num, question_num, question_text,
         sub_question_text, answer_order, value, answer_text)


## Define Respondent IDs (Race) ====
respondent_race_id <- survey_df %>% 
  select(respondent_id, col_num, answer_text) %>% 
  filter(col_num %in% c(56:64)) %>% 
  group_by(respondent_id) %>% 
  mutate(count = sum(!is.na(answer_text))) %>% 
  mutate(answer_text = case_when(count > 1 & !col_num == 62 ~as.character(NA),
                                 count > 1 & col_num == 62 ~"Two or more races",
                                 count == 1 & col_num == 64 & !is.na(answer_text) ~"Other",
                                 TRUE ~answer_text)) %>% 
  ungroup() %>% 
  distinct(respondent_id, answer_text) %>% 
  filter(!is.na(answer_text)) %>% 
  rename(race_id = answer_text)

race_levels <- answers %>% 
  select(1,2,3,4) %>% 
  filter(question_num == 24) %>% 
  add_row(answer_num = max(answers$answer_num[answers$question_num==24])+1, answer_en = "Other") %>%
  arrange(desc(answer_num)) %>% 
  pull(answer_en)

## Define Respondent IDs (Income) ====
respondent_income_id <- survey_df %>% 
  select(respondent_id, col_num, answer_text) %>% 
  filter(col_num %in% c(66)) %>% 
  distinct(respondent_id, answer_text) %>% 
  filter(!is.na(answer_text)) %>% 
  rename(income_id = answer_text)

income_levels <- answers %>% 
  select(1,2,3,4) %>% 
  filter(question_num == 26) %>% 
  arrange(desc(answer_num)) %>% 
  pull(answer_en)

## Respondent List ====
# How many of your children, or the children you take care of, attend school in San Francisco?
respondent_filter_1 <- survey_df %>% 
  select(respondent_id, col_num, answer_text) %>%
  filter(col_num == 10) %>% 
  filter(answer_text %in% c("NONE","0","N/A") | is.na(answer_text)) %>% 
  pull(respondent_id)
  
respondent_filter_2 <- survey_df %>% 
  select(respondent_id, col_num, answer_text) %>% 
  filter(col_num == 10) %>% 
  filter(answer_text %in% c("NONE","0","N/A","1","Uno","1 niña") | is.na(answer_text)) %>% 
  pull(respondent_id)

respondent_filter_3 <- survey_df %>% 
  select(respondent_id, col_num, answer_text) %>% 
  filter(col_num == 12) %>% 
  filter(answer_text %in% c("没有","2-8","my son is 4.5","n/a") | is.na(answer_text)) %>% 
  pull(respondent_id)

num_respondents_total <- length(unique(survey_df$respondent_id))
num_respondents <-length(
  unique(
    survey_df$respondent_id[
      !survey_df$respondent_id %in% unique(c(respondent_filter_1, respondent_filter_3))
      ]))

# DEFINE =======================================================================
## Color Palette ====
# Primary "#006c69"
# Supplemental Yellow "#ffb81d"
# Supplemental Red "#c41d4a"
# Supplemental Blue "#1c355e"

pal_seq3 <- rev(c('#002423','#036f6c','#69bdb9'))
pal_seq4 <- rev(c('#003433','#036e6b','#56aaa6','#96eae5'))
pal_seq5 <- rev(c('#001a15','#004341','#07716e','#4a9f9b','#7cd0cc'))
pal_seq6 <- rev(c('#002322','#004846','#056f6c','#419793','#6cc0bc','#97ebe6'))

pal_div3 <- c("#ffb81d","#c41d4a","#006c69")
pal_div4 <- c("#ffb81d","#c41d4a","#006c69","#1c355e") 
pal_div5 <- c("#b9d9ec","#ffb81d","#c41d4a","#006c69","#1c355e") 

# PLOT =========================================================================
## Part 1: Who did we hear from? ====
### Question 1 ====
summ <- survey_df %>% 
  filter(question_num == 1) %>% 
  mutate(answer_text = case_when(answer_text %in% c("259","70","35") ~"5 or more",
                                 answer_text %in% c("三个","3 niños") ~"3",
                                 answer_text == "Two" ~"2",
                                 str_detect(answer_text, "please note") ~"2",
                                 answer_text %in% c("Uno","1 niña")  ~"1",
                                 answer_text %in% c("NONE","0","N/A") ~"0",
                                 # is.na(answer_text) ~"0",
                                 TRUE ~answer_text)) %>% 
  group_by(answer_text) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  filter(!is.na(answer_text)) %>% 
  mutate(prop = n/sum(n)) %>% 
  mutate(answer_text = factor(.$answer_text,
                              ordered=TRUE,
                              levels=rev(.$answer_text)))

num_respondents <- sum(summ$n)

plot_title = paste0(
  str_wrap(
    survey_df %>% filter(question_num == 1) %>% slice(1) %>% pull(question_text), 60),
  " (N=", num_respondents, ")")

plt <- ggplot(summ, aes(x="", y=prop, fill=answer_text)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position="bottom") +
  # geom_label_repel(aes(y = cumsum(prop)-prop*0.5, x = 1.5,
  #                      label= if_else(prop < 0.01, "<1%", sprintf("%1.0f%%", prop*100))),
  #                  color="black", size=6, fill="white", nudge_x = 0.5,
  #                  show.legend=FALSE) +
  ggtitle(plot_title) +
  guides(fill=guide_legend(reverse=TRUE, title=NULL, nrow=1)) +
  scale_fill_manual(values=pal_seq6) +
  theme(plot.title = element_text(face="bold"))

png(paste0("output/sfcta/plot/q1.png"), width = 8*300, height=6*300, res=300)
print(plt)
dev.off()

### Question 2 ====
ans_levels <- answers %>% 
  select(1,2,3,4) %>% 
  filter(question_num == 2) %>% 
  arrange(desc(answer_num)) %>% 
  pull(answer_en)

summ <- survey_df %>% 
  filter(!respondent_id %in% respondent_filter_2) %>% 
  filter(question_num == 2) %>% 
  group_by(answer_text) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  mutate(prop = n/sum(n)) %>% 
  mutate(answer_text = factor(str_wrap(.$answer_text,30),
                              ordered=TRUE,
                              levels=str_wrap(ans_levels,30)))

plot_title = paste0(
  str_wrap(
    survey_df %>% filter(question_num == 2) %>% slice(1) %>% pull(question_text), 60),
  " (N=", sum(summ$n), ")")

plt <- ggplot(summ, aes(x="", y=prop, fill=answer_text)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position="bottom") +
  # geom_label_repel(aes(y = cumsum(prop)-prop*0.5, x = 1.5,
  #                      label= if_else(prop < 0.01, "<1%", sprintf("%1.0f%%", prop*100))),
  #                  color="black", size=6, fill="white", nudge_x = 0.5,
  #                  # segment.size = 0.2,
  #                  show.legend=FALSE) +
  ggtitle(plot_title) +
  guides(fill=guide_legend(reverse=TRUE, title=NULL, nrow=1)) +
  scale_fill_manual(values=pal_div3) +
  theme(plot.title = element_text(face="bold"))

png(paste0("output/sfcta/plot/q2.png"), width = 8*300, height=6*300, res=300)
print(plt)
dev.off()

### Question 3 ====
summ <- survey_df %>%
  filter(!respondent_id %in% respondent_filter_1) %>%
  filter(question_num == 3) %>%
  mutate(answer_text = case_when(answer_text %in% c("没有","2-8","my son is 4.5","n/a") ~"0",
                                 # is.na(answer_text) ~"0",
                                 answer_text %in% c("是","Seis años","7 años") ~"1",
                                 answer_text %in% c("2个","6歲、9歲") ~"2",
                                 answer_text %in% c("259","70","23","8") ~"5 or more",
                                 TRUE ~answer_text)) %>%
  group_by(answer_text) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  filter(!is.na(answer_text)) %>% 
  mutate(prop = n/sum(n)) %>% 
  mutate(answer_text = factor(.$answer_text,
                              ordered=TRUE,
                              levels=rev(.$answer_text)))

plot_title = paste0(
  str_wrap(
    survey_df %>% filter(question_num == 3) %>% slice(1) %>% pull(question_text), 60),
  " (N=", sum(summ$n), ")")

plt <- ggplot(summ, aes(x="", y=prop, fill=answer_text)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position="bottom") +
  ggtitle(plot_title) +
  guides(fill=guide_legend(reverse=TRUE, title=NULL, nrow=1)) +
  scale_fill_manual(values=pal_seq6) +
  theme(plot.title = element_text(face="bold"))

png(paste0("output/sfcta/plot/q3.png"), width = 8*300, height=6*300, res=300)
print(plt)
dev.off()

### Question 24 (Race), 25 (Gender), 26 (Income)====
questions_lst <- c(4, 24, 25, 26)

for(question in 1:length(questions_lst)){
  i = questions_lst[question]
  print(i)
  
  if(i==24){
    ans_levels <- answers %>% 
      select(1,2,3,4) %>% 
      filter(question_num == i) %>% 
      add_row(answer_num = max(answers$answer_num[answers$question_num==i])+1, answer_en="Other") %>% 
      arrange(desc(answer_num)) %>% 
      pull(answer_en)
  }else{
    ans_levels <- answers %>% 
      select(1,2,3,4) %>% 
      filter(question_num == i) %>% 
      arrange(desc(answer_num)) %>% 
      pull(answer_en)
  }
  
  # num_respondents <-length(
  #   unique(
  #     survey_df$respondent_id[
  #       !survey_df$respondent_id %in% unique(c(respondent_filter_1, respondent_filter_3))
  #     ]))
  
  summ <- survey_df %>% 
    filter(!respondent_id %in% unique(c(respondent_filter_1, respondent_filter_3))) %>% 
    filter(question_num == i & !is.na(answer_text)) %>% 
    # convert "Other" race responses
    mutate(answer_text = case_when(col_num == 64 ~"Other",
                                   TRUE ~answer_text)) %>% 
    
    group_by(answer_text) %>% 
    summarise(n=n()) %>% 
    ungroup() %>% 
    mutate(prop = n/sum(n)) %>% 
    arrange(prop) %>% 
    mutate(answer_text = factor(str_wrap(.$answer_text,40),
                                ordered=TRUE,
                                levels=str_wrap(ans_levels,40)))
  
  num_respondents <- sum(summ$n)
  
  plot_title <- paste0(
    str_wrap(
      survey_df %>% filter(question_num == i) %>% slice(1) %>% pull(question_text), 60),
    " (N=", num_respondents, ")")
  
  plt <- ggplot(summ,aes(x=answer_text,y=prop)) +
    geom_bar(stat="identity", fill="#006c69", alpha=0.9) +
    geom_label(aes(label=if_else(prop < 0.01, "<1%", sprintf("%1.0f%%", prop*100))), color="white", fill="#666666") +
    coord_flip() +
    ggtitle(plot_title) +
    xlab("") +
    ylab("Share of Respondents") +
    scale_y_continuous(labels=scales::percent) +
    theme(plot.title = element_text(face="bold"),
          axis.text=element_text(size=10))
  
  png(paste0("output/sfcta/plot/q",i,".png"), width = 8*300, height=(1+length(ans_levels)*0.4)*300, res=300)
  print(plt)
  dev.off()
}

### Question 4 (Cross-tabs) ====

i=4

ans_levels = answers %>% 
  select(1,2,3,4) %>% 
  filter(question_num == i) %>% 
  arrange(desc(answer_num)) %>% 
  pull(answer_en)

summ_race <- survey_df %>% 
  filter(!respondent_id %in% unique(c(respondent_filter_1, respondent_filter_3))) %>%  
  filter(question_num == i, !is.na(answer_text)) %>% 
  left_join(race_id, by="respondent_id") %>% 
  group_by(answer_text, race_id) %>% 
  summarise(n=n()) %>% 
  ungroup() %>%
  filter(!is.na(race_id)) %>% 
  group_by(race_id) %>% 
  mutate(prop = n/sum(n),
         race_id_label = paste0(race_id, " (N=", sum(n), ")")) %>% 
  ungroup()

summ_race_a <- summ_race %>% 
  filter(answer_text == "Yes") %>% 
  add_row(answer_text = "Yes",
          race_id = "Native Hawaiian or other Pacific Islander",
          n = 0,
          prop = 0,
          race_id_label = "Native Hawaiian or other Pacific Islander (N=2)") %>% 
  arrange(prop, desc(race_id)) %>% 
  pull(race_id)
  
summ_race <- summ_race %>% 
  mutate(answer_text = factor(str_wrap(.$answer_text,40),
                              ordered=TRUE,
                              levels=rev(str_wrap(ans_levels,40))),
         race_id = factor(str_wrap(.$race_id,40),
                          ordered=TRUE,
                          levels=str_wrap(summ_race_a,40)))

summ_race_label <- summ_race %>% 
  distinct(race_id, race_id_label) %>% 
  arrange(race_id) %>% 
  pull(race_id_label)

summ_income <- survey_df %>% 
  filter(!respondent_id %in% unique(c(respondent_filter_1, respondent_filter_3))) %>% 
  filter(question_num == i, !is.na(answer_text)) %>% 
  left_join(income_id, by="respondent_id") %>% 
  group_by(answer_text, income_id) %>% 
  summarise(n=n()) %>% 
  ungroup() %>%
  filter(!is.na(income_id)) %>% 
  group_by(income_id) %>%
  mutate(prop = n/sum(n),
         income_id_label = paste0(income_id, " (N=", sum(n), ")")) %>% 
  ungroup() %>% 
  mutate(answer_text = factor(str_wrap(.$answer_text,40),
                              ordered=TRUE,
                              levels=rev(str_wrap(ans_levels,40))),
         income_id = factor(str_wrap(.$income_id,40),
                          ordered=TRUE,
                          levels=str_wrap(income_levels,40)))

summ_income_label <- summ_income %>% 
  distinct(income_id, income_id_label) %>% 
  arrange(income_id) %>% 
  pull(income_id_label)

plot_title = paste0(
  str_wrap(
    survey_df %>% filter(question_num == i) %>% slice(1) %>% pull(question_text), 60),
  " (N=", sum(summ_race$n), ")")

plt <- ggplot(summ_race, aes(x=race_id, y=prop, fill=answer_text, )) +
  geom_col(position = position_stack(reverse = TRUE)) + 
  geom_label(aes(x=race_id, y=prop,
                 label = if_else(prop > 0.01, sprintf("%1.0f%%", prop*100), NULL),
                 group = answer_text),
             lineheight = 0.9,
             position = position_stack(vjust=0.5, reverse=TRUE),
             color = "white",
             size = 3,
             fill = "#666666") +
  coord_flip() +
  ggtitle(plot_title) +
  labs(x="", y="Share of Respondents") +
  scale_x_discrete(labels = summ_race_label) +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values = rev(pal_div3)) +
  guides(fill=guide_legend(title=NULL)) +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(face="bold"),
        axis.text=element_text(size=10))

png(paste0("output/sfcta/plot/q",i,"_race.png"), width = 11*300, height=(1+length(race_levels)*0.5)*300, res=300)
print(plt)
dev.off()

plot_title = paste0(
  str_wrap(
    survey_df %>% filter(question_num == i) %>% slice(1) %>% pull(question_text), 60),
  " (N=", sum(summ_income$n), ")")

plt <- ggplot(summ_income, aes(fill=answer_text, x=income_id, y=prop)) +
  geom_col(position = position_stack(reverse = TRUE)) + 
  geom_label(aes(x=income_id, y=prop, 
                 label = if_else(prop > 0.01, sprintf("%1.0f%%", prop*100), NULL),
                 group = answer_text),
             lineheight = 0.9,
             position = position_stack(vjust=0.5, reverse=TRUE),
             color = "white",
             size = 3,
             fill = "#666666") +
  coord_flip() +
  ggtitle(plot_title) +
  labs(x="", y="Share of Respondents") +
  scale_x_discrete(labels = summ_income_label) +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values = rev(pal_div3)) +
  guides(fill=guide_legend(title=NULL)) +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(face="bold"),
        axis.text=element_text(size=10))

png(paste0("output/sfcta/plot/q",i,"_income.png"), width = 11*300, height=(4+length(ans_levels)*0.5)*300, res=300)
print(plt)
dev.off()

## Part 2: What did we hear? ====
### Question 5,7,8,15,16,21 (No Cross Tabs) ====

questions_lst <- c(5,7,8,15,16,21)

for(question in 1:length(questions_lst)){
  i = questions_lst[question]
  print(i)
  
  ans_levels = answers %>% 
    select(1,2,3,4) %>% 
    filter(question_num == i) %>% 
    mutate(answer_en = case_when(str_detect(answer_en, "Other") ~"Other",
                                 TRUE ~answer_en)) %>% 
    arrange(desc(answer_num)) %>% 
    pull(answer_en)
  
  summ <- survey_df %>% 
    filter(!respondent_id %in% unique(c(respondent_filter_1, respondent_filter_3))) %>%
    filter(question_num == i & !is.na(answer_text)) %>% 
    mutate(answer_text = case_when(str_detect(sub_question_text, "Other") ~"Other",
                                   TRUE ~answer_text)) %>% 
    group_by(answer_text) %>% 
    summarise(n=n()) %>% 
    ungroup() %>% 
    mutate(prop = n/sum(n)) %>% 
    arrange(prop) %>% 
    mutate(answer_text = factor(str_wrap(.$answer_text,40),
                                ordered=TRUE,
                                levels=str_wrap(.$answer_text,40))) #ans_levels
  
  plot_title = paste0(
    str_wrap(
      survey_df %>% filter(question_num == i) %>% slice(1) %>% pull(question_text), 60),
    " (N=", sum(summ$n), ")")
  
  plt <- ggplot(summ, aes(x=answer_text,y=prop)) +
    geom_bar(stat="identity", fill="#006c69", alpha=0.9) +
    geom_label(aes(label=sprintf("%1.0f%%", prop*100)), color="white", fill="#666666") +
    coord_flip() +
    ggtitle(plot_title) +
    xlab("") +
    ylab("Share of Respondents") +
    scale_y_continuous(labels=scales::percent) +
    theme(plot.title = element_text(face="bold"),
          axis.text=element_text(size=10))
  
  png(paste0("output/sfcta/plot/q",i,".png"), width = 9*300, height=(1+length(ans_levels)*0.5)*300, res=300)
  print(plt)
  dev.off()
}

### Question 12 (Cross Tabs) ====
ans_levels <- answers %>% 
  select(1,2,3,4) %>% 
  filter(question_num == 12) %>% 
  arrange(desc(answer_num)) %>% 
  pull(answer_en)

fill_cat <- survey_df %>% 
  filter(question_num == 10, sub_question_text == "Shuttle Program") %>% 
  filter(!is.na(answer_text)) %>% 
  mutate(score = str_split_fixed(answer_text,". ",2)[,1]) %>% 
  mutate(score_cat = case_when(score %in% c(1,2) ~"1-2",
                               score %in% c(4,5) ~"4-5",
                               TRUE ~score)) %>% 
  select(respondent_id, score_cat) %>% 
  arrange(score_cat) %>% 
  mutate(score_cat = factor(.$score_cat,
                            ordered=TRUE,
                            level=c("1-2","3","4-5")))


summ <- survey_df %>% 
  filter(!respondent_id %in% unique(c(respondent_filter_1, respondent_filter_3))) %>% 
  filter(question_num == 12 & !is.na(answer_text)) %>% 
  left_join(fill_cat, by="respondent_id") %>% 
  group_by(answer_text, score_cat) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  filter(!is.na(score_cat)) %>% 
  group_by(answer_text) %>% 
  mutate(prop = n/sum(n),
         cat_label = str_wrap(paste0(answer_text, " (N=", sum(n), ")"),40)) %>% 
  ungroup() %>% 
  # arrange(prop) %>% 
  mutate(answer_text = factor(str_wrap(.$answer_text,40),
                              ordered=TRUE,
                              levels=str_wrap(ans_levels,40)))

summ_label <- summ %>% 
  distinct(answer_text, cat_label) %>% 
  arrange(answer_text) %>% 
  pull(cat_label)

plot_title = paste0(
  str_wrap(
    survey_df %>% filter(question_num == 12) %>% slice(1) %>% pull(question_text), 60),
  " (N=", sum(summ$n), ")")

plt <- ggplot(summ, aes(x=answer_text, y=prop, fill=score_cat)) +
  geom_col(position = position_stack(reverse = TRUE)) + 
  geom_label(aes(x=answer_text, y=prop,
                 label = if_else(prop > 0.02, sprintf("%1.0f%%", prop*100), NULL),
                 group = score_cat),
             lineheight = 0.9,
             position = position_stack(vjust=0.5, reverse=TRUE),
             color = "white",
             size = 3,
             fill = "#666666") +
  coord_flip() +
  ggtitle(plot_title) +
  labs(x="", y="Share of Respondents") +
  scale_x_discrete(labels = summ_label) +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values = pal_seq3) +
  guides(fill=guide_legend(title="Level of Interest in Shuttle Program", nrow=1)) + 
  theme(legend.position="bottom") +
  theme(plot.title = element_text(face="bold"),
        axis.text=element_text(size=10))

png(paste0("output/sfcta/plot/q",12,"_xtab.png"), width = 10*300, height=(1+length(ans_levels)*0.8)*300, res=300)
print(plt)
dev.off()


### Question 8.1,9,12 (Cross Tabs) ====
# questions_lst <- c(8.1,9,10,12,13,17)
questions_lst <- c(8.1,9,12)

for(question in 1:length(questions_lst)){
  i = questions_lst[question]
  print(i)
  
  if(i %in% c(8.1,9)){
    color_palette <- pal_div5
  }
  
  # num_respondents <-length(
  #   unique(
  #     survey_df$respondent_id[
  #       !survey_df$respondent_id %in% unique(c(respondent_filter_1, respondent_filter_3))
  #     ]))
  
  ans_levels <- answers %>% 
    select(1,2,3,4) %>% 
    filter(question_num == i) %>% 
    mutate(answer_en = case_when(str_detect(answer_en, "Other") ~"Other",
                                 TRUE ~answer_en)) %>% 
    arrange(desc(answer_num)) %>% 
    pull(answer_en)
  
  # OVERVIEW
  summ <- survey_df %>% 
    filter(!respondent_id %in% unique(c(respondent_filter_1, respondent_filter_3))) %>% 
    filter(question_num == i & !is.na(answer_text)) %>% 
    # convert "Other" responses
    mutate(answer_text = case_when(col_num == 25 ~"Other",
                                   TRUE ~answer_text)) %>% 
    group_by(answer_text) %>% 
    summarise(n=n()) %>% 
    ungroup() %>% 
    mutate(prop = n/sum(n)) %>% 
    arrange(prop) %>% 
    mutate(answer_text = factor(str_wrap(.$answer_text,40),
                                ordered=TRUE,
                                levels=str_wrap(.$answer_text,40)))

  plot_title = paste0(
    str_wrap(
      survey_df %>% filter(question_num == i) %>% slice(1) %>% pull(question_text), 60),
    " (N=", sum(summ$n), ")")
  
  plt <- ggplot(summ,aes(x=answer_text,y=prop)) +
    geom_bar(stat="identity", fill="#006c69", alpha=0.9) +
    geom_label(aes(label=if_else(prop < 0.01, "<1%", sprintf("%1.0f%%", prop*100))), color="white", fill="#666666") +
    coord_flip() +
    ggtitle(plot_title) +
    xlab("") +
    ylab("Share of Respondents") +
    scale_y_continuous(labels=scales::percent) +
    theme(plot.title = element_text(face="bold"),
          axis.text=element_text(size=10))
  
  png(paste0("output/sfcta/plot/q",i,".png"), width = 8*300, height=(1+length(ans_levels)*0.4)*300, res=300)
  print(plt)
  dev.off()
  
  if(length(summ$answer_text) > 4){
    legend_row_num = 2
  } else{
    legend_row_num = 1
  }
  
  # CROSS-TAB
  summ_race <- survey_df %>% 
    filter(!respondent_id %in% unique(c(respondent_filter_1, respondent_filter_3))) %>%  
    filter(question_num == i, !is.na(answer_text)) %>% 
    # convert "Other" responses
    mutate(answer_text = case_when(col_num == 25 ~"Other",
                                   TRUE ~answer_text)) %>% 
    left_join(race_id, by="respondent_id") %>% 
    group_by(answer_text, race_id) %>% 
    summarise(n=n()) %>% 
    ungroup() %>% 
    filter(!is.na(race_id)) %>% 
    
    group_by(race_id) %>% 
    mutate(prop = n/sum(n),
           race_id_label = str_wrap(paste0(race_id, " (N=", sum(n), ")"),40)) %>% 
    ungroup() %>% 
    
    # add_row(answer_text = "Yes, I would ride Muni to school with my child more often if it cost less",
    #         race_id = "Native Hawaiian or other Pacific Islander",
    #         n = 0,
    #         prop = 0,
    #         race_id_label = "Native Hawaiian or other Pacific\nIslander (N=2)") %>% 

    mutate(answer_text = factor(str_wrap(.$answer_text,40),
                                ordered=TRUE,
                                levels=rev(str_wrap(ans_levels,40)))) %>% 
    
    group_by(answer_text) %>% 
    arrange(prop, .by_group=TRUE) %>% 
    mutate(race_id = factor(str_wrap(race_id,40), levels=str_wrap(race_id,40))) %>% 
    ungroup()
  
  summ_race_label <- summ_race %>% 
    distinct(race_id, race_id_label) %>% 
    arrange(race_id) %>% 
    pull(race_id_label)
  
  plot_title = paste0(
    str_wrap(
      survey_df %>% filter(question_num == i) %>% slice(1) %>% pull(question_text), 60),
    " (N=", sum(summ_race$n), ")")
  
  plt <- ggplot(summ_race, aes(x=race_id, y=prop, fill=answer_text)) +
    geom_col(position = position_stack(reverse = TRUE)) + 
    geom_label(aes(x=race_id, y=prop,
                   label = if_else(prop > 0.01, sprintf("%1.0f%%", prop*100), NULL),
                   group = answer_text),
               lineheight = 0.9,
               position = position_stack(vjust=0.5, reverse=TRUE),
               color = "white",
               size = 3,
               fill = "#666666") +
    coord_flip() +
    ggtitle(plot_title) +
    labs(x="", y="Share of Respondents") +
    scale_x_discrete(labels = summ_race_label) +
    scale_y_continuous(labels=scales::percent) +
    scale_fill_manual(values = color_palette) +
    guides(fill=guide_legend(title=NULL, nrow=legend_row_num)) +
    theme(legend.position="bottom") +
    theme(plot.title = element_text(face="bold"),
          axis.text=element_text(size=10))
  
  png(paste0("output/sfcta/plot/q",i,"_race.png"), width = 11*300, height=(1+length(race_levels)*0.5)*300, res=300)
  print(plt)
  dev.off()
  
  summ_income <- survey_df %>% 
    filter(!respondent_id %in% unique(c(respondent_filter_1, respondent_filter_3))) %>% 
    filter(question_num == i, !is.na(answer_text)) %>% 
    # convert "Other" responses
    mutate(answer_text = case_when(col_num == 25 ~"Other",
                                   TRUE ~answer_text)) %>% 
    left_join(income_id, by="respondent_id") %>% 
    group_by(answer_text, income_id) %>% 
    summarise(n=n()) %>% 
    ungroup() %>%
    filter(!is.na(income_id)) %>% 
    group_by(income_id) %>%
    mutate(prop = n/sum(n),
           income_id_label = str_wrap(paste0(income_id, " (N=", sum(n), ")"),40)) %>% 
    ungroup() %>%
    mutate(answer_text = factor(str_wrap(.$answer_text,40),
                                ordered=TRUE,
                                levels=rev(str_wrap(ans_levels,40))),
           income_id = factor(str_wrap(.$income_id,40),
                              ordered=TRUE,
                              levels=str_wrap(income_levels,40)))
  
  summ_income_label <- summ_income %>% 
    distinct(income_id, income_id_label) %>% 
    arrange(income_id) %>% 
    pull(income_id_label)
  
  plot_title = paste0(
    str_wrap(
      survey_df %>% filter(question_num == i) %>% slice(1) %>% pull(question_text), 60),
    " (N=", sum(summ_income$n), ")")

  plt <- ggplot(summ_income, aes(fill=answer_text, x=income_id, y=prop)) +
    geom_col(position = position_stack(reverse = TRUE)) + 
    geom_label(aes(x=income_id, y=prop, 
                   label = if_else(prop > 0.01, sprintf("%1.0f%%", prop*100), NULL),
                   group = answer_text),
               lineheight = 0.9,
               position = position_stack(vjust=0.5, reverse=TRUE),
               color = "white",
               size = 3,
               fill = "#666666") +
    coord_flip() +
    ggtitle(plot_title) +
    labs(x="", y="Share of Respondents") +
    scale_x_discrete(labels = summ_income_label) +
    scale_y_continuous(labels=scales::percent) +
    scale_fill_manual(values = rev(color_palette)) +
    guides(fill=guide_legend(title=NULL, nrow=legend_row_num)) +
    theme(legend.position="bottom") +
    theme(plot.title = element_text(face="bold"),
          axis.text=element_text(size=10))
  
  png(paste0("output/sfcta/plot/q",i,"_income.png"), width = 11*300, height=(4+length(ans_levels)*0.5)*300, res=300)
  print(plt)
  dev.off()
}

### Question 10,13,17,19 (Matrix Responses) ====
questions_lst <- c(10,13,17,19)

title_tbl <- tibble(question_num=c(10,13,17,19),
                    plot_title=c(
                      str_wrap(
                        "The SFCTA is considering several options to improve how students get to school. We want your feedback on what ideas you like and don't like.",
                        60),
                      str_wrap(
                        "What would be your level of comfort with your child using these transportation options? Please rank each on a scale from 1 to 5.",
                        60),
                      str_wrap(
                        "SFCTA is considering additional solutions that focus on safety for the school trip. We want your feedback on what ideas you like and don't like. Please rank each on a scale from 1 to 5.",
                        60),
                      str_wrap(
                        "SFCTA has heard that information about transportation to schools is sometimes hard to access. We want your feedback on what ideas you like and don't like.",
                        60)))

for(question in 1:length(questions_lst)){
  i = questions_lst[question]
  print(i)
  
  ans_levels <- answers %>% 
    select(1,2,3,4) %>% 
    filter(question_num == i) %>% 
    arrange(desc(answer_num)) %>% 
    pull(answer_en)
  
  plot_title = title_tbl %>% 
    filter(question_num == i) %>% 
    pull(plot_title)
  
  # Overview
  
  # num_respondents <- survey_df %>% 
  #   filter(!respondent_id %in% unique(c(respondent_filter_1, respondent_filter_3))) %>% 
  #   filter(question_num == i, !is.na(answer_text)) %>% 
  #   distinct(respondent_id) %>% nrow()
  
  summ <- survey_df %>% 
    filter(!respondent_id %in% unique(c(respondent_filter_1, respondent_filter_3))) %>% 
    filter(question_num == i, !is.na(answer_text)) %>% 
    group_by(sub_question_text, answer_text) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    group_by(sub_question_text) %>% 
    mutate(prop = n/sum(n),
           sub_question_text = paste0(sub_question_text, " (N=", sum(n), ")")) %>% 
    ungroup() %>% 
    mutate(sub_question_text = str_wrap(.$sub_question_text,40),
           answer_text = factor(str_wrap(.$answer_text,40),
                                ordered=TRUE,
                                levels=str_wrap(ans_levels,40)))

  plt <- ggplot(summ, aes(x=answer_text,y=prop)) +
    geom_bar(stat="identity", fill="#006c69", alpha=0.9) +
    geom_label(aes(label=if_else(prop < 0.01, "<1%", sprintf("%1.0f%%", prop*100))), color="white", fill="#666666") +
    coord_flip() +
    ggtitle(plot_title) +
    xlab("") +
    ylab("Share of Respondents") +
    scale_y_continuous(labels=scales::percent) +
    facet_wrap(~sub_question_text) +
    theme(plot.title = element_text(face="bold"),
          axis.text=element_text(size=10))
  
  if(length(unique(summ$sub_question_text)) > 3){
    png(paste0("output/sfcta/plot/q",i,"_overview.png"), width = 10*300, height=8*300, res=300)
    print(plt)
    dev.off()
  } else{
    png(paste0("output/sfcta/plot/q",i,"_overview.png"), width = 12*300, height=6*300, res=300)
    print(plt)
    dev.off()
  }
    
  # Crosstabs 
  if(length(summ$answer_text) > 4){
    legend_row_num = 2
  } else{
    legend_row_num = 1
  }
  
  unique_sub_question <- survey_df %>% 
    filter(question_num == i) %>% 
    distinct(sub_question_text) %>% 
    pull(sub_question_text)

  
  for(j in 1:length(unique_sub_question)){
    sub_question_id <- unique_sub_question[j]
    print(paste(j,sub_question_id))
    
    summ_race <- survey_df %>% 
      filter(!respondent_id %in% unique(c(respondent_filter_1, respondent_filter_3))) %>% 
      filter(question_num == i, !is.na(answer_text), sub_question_text == sub_question_id) %>% 
      left_join(race_id, by="respondent_id") %>% 
      group_by(answer_text, race_id) %>% 
      summarise(n=n()) %>% 
      ungroup() %>% 
      filter(!is.na(race_id)) %>% 
      group_by(race_id) %>% 
      mutate(prop = n/sum(n),
             race_id_label = str_wrap(paste0(race_id,  " (N=", sum(n), ")"),40)) %>% 
      ungroup() %>% 
      mutate(answer_text = factor(str_wrap(.$answer_text,40),
                                  ordered=TRUE,
                                  levels=str_wrap(rev(ans_levels),40)),
             race_id = factor(str_wrap(.$race_id,40),
                              ordered=TRUE,
                              levels=str_wrap(race_levels,40)))
    
    summ_race_label <- summ_race %>% 
      distinct(race_id, race_id_label) %>% 
      arrange(race_id) %>% 
      pull(race_id_label)
    
    plt <- ggplot(summ_race, aes(x=race_id, y=prop, fill=answer_text)) +
      geom_col(position = position_stack(reverse = TRUE)) +
      geom_label(aes(x=race_id, y=prop,
                     label = if_else(prop > 0.01, sprintf("%1.0f%%", prop*100), NULL),
                     group = answer_text),
                 lineheight = 0.9,
                 position = position_stack(vjust=0.5, reverse=TRUE),
                 color = "white",
                 size = 3,
                 fill = "#666666") +
      coord_flip() +
      labs(x="", y="Share of Respondents") +
      ggtitle(sub_question_id) +
      scale_x_discrete(labels = summ_race_label) +
      scale_y_continuous(labels=scales::percent) +
      scale_fill_manual(values = pal_seq5) +
      guides(fill=guide_legend(title=NULL, nrow=legend_row_num)) +
      theme(legend.position="bottom",
            plot.title = element_text(face="bold"),
            axis.text=element_text(size=10))
    
    png(paste0("output/sfcta/plot/q",i,"-race-",j,".png"), width = 10*300, height=(1+length(race_levels)*0.4)*300, res=300)
    print(plt)
    dev.off()
    
    summ_income <- survey_df %>% 
      filter(!respondent_id %in% unique(c(respondent_filter_1, respondent_filter_3))) %>% 
      filter(question_num == i, !is.na(answer_text)) %>% 
      left_join(income_id, by="respondent_id") %>% 
      group_by(answer_text, income_id) %>% 
      summarise(n=n()) %>% 
      ungroup() %>% 
      filter(!is.na(income_id)) %>% 
      group_by(income_id) %>% 
      mutate(prop = n/sum(n),
             income_id_label = str_wrap(paste0(income_id,  " (N=", sum(n), ")"),40)) %>% 
      ungroup() %>% 
      mutate(answer_text = factor(str_wrap(.$answer_text,40),
                                  ordered=TRUE,
                                  levels=str_wrap(rev(ans_levels),40)),
             income_id = factor(str_wrap(.$income_id,40),
                                ordered=TRUE,
                                levels=str_wrap(income_levels,40)))

    summ_income_label <- summ_income %>%
      distinct(income_id, income_id_label) %>% 
      arrange(income_id) %>% 
      pull(income_id_label)
    
    plt <- ggplot(summ_income, aes(x=income_id, y=prop, fill=answer_text)) +
      geom_col(position = position_stack(reverse = TRUE)) +
      geom_label(aes(x=income_id, y=prop,
                     label = if_else(prop > 0.01, sprintf("%1.0f%%", prop*100), NULL),
                     group = answer_text),
                 lineheight = 0.9,
                 position = position_stack(vjust=0.5, reverse=TRUE),
                 color = "white",
                 size = 3,
                 fill = "#666666") +
      coord_flip() +
      labs(x="", y="Share of Respondents") +
      ggtitle(sub_question_id) +
      scale_x_discrete(labels = summ_income_label) +
      scale_y_continuous(labels=scales::percent) +
      scale_fill_manual(values = pal_seq5) +
      guides(fill=guide_legend(title=NULL, nrow=legend_row_num)) +
      theme(legend.position="bottom",
            plot.title = element_text(face="bold"),
            axis.text=element_text(size=10))
    
    png(paste0("output/sfcta/plot/q",i,"-income-",j,".png"), width = 10*300, height=(1+length(income_levels)*0.4)*300, res=300)
    print(plt)
    dev.off()
    
  }

  summ <- survey_df %>% 
    filter(!respondent_id %in% unique(c(respondent_filter_1, respondent_filter_3))) %>% 
    filter(question_num == i, !is.na(answer_text)) %>% 
    mutate(answer_score = as.numeric(str_split_fixed(answer_text,"\\.",2)[,1])) %>% 
    left_join(race_id, by="respondent_id") %>% 
    left_join(income_id, by="respondent_id") %>% 
    group_by(sub_question_text, race_id, income_id) %>% 
    summarise(n = n(),
              avg_score = mean(answer_score, na.rm=TRUE)) %>% 
    ungroup() %>% 
    filter(!is.na(race_id), !is.na(income_id)) %>%
    mutate(race_id = factor(str_wrap(.$race_id,35),
                            ordered=TRUE,
                            levels=rev(str_wrap(race_levels,35))),
           income_id = factor(str_wrap(.$income_id,40),
                              ordered=TRUE,
                              levels=rev(str_wrap(income_levels,40))))
  
  plt <- ggplot(summ, aes(x=race_id, y=income_id, fill=avg_score)) +
    geom_raster(hjust = 0.5, vjust = 0.5) +
    geom_text(aes(label=paste0(round(avg_score,1),"\n(N=",n,")")),
              size=2,
              lineheight=1,
              color = "white") +
    scale_fill_gradient(low="#7cd0cc", high="#001a15") +
    facet_wrap(~sub_question_text) +
    labs(x="", y="") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          legend.title = element_blank())
  
  if(length(unique(summ$sub_question_text)) > 3){
    png(paste0("output/sfcta/plot/q",i,"_matrix.png"), width = 10*300, height=8*300, res=300)
    print(plt)
    dev.off()
  } else{
    png(paste0("output/sfcta/plot/q",i,"_matrix.png"), width = 12*300, height=6*300, res=300)
    print(plt)
    dev.off()
  }

}

### 9.1 pie chart ====
q12_respondent_id <- survey_df %>%
  filter(question_num == 10 &
           !is.na(answer_text) &
           sub_question_text == "Shuttle Program" &
           answer_text %in% c("4. I may use this in the future given the right circumstances",
                              "5. I would absolutely use this in the future")) %>%
  pull(respondent_id)

respondent_q9_1 <- survey_df %>% 
  filter(!respondent_id %in% unique(c(respondent_filter_1, respondent_filter_3))) %>% 
  filter(respondent_id %in% q12_respondent_id) %>%
  filter(question_num == 9 & !is.na(answer_text)) %>% 
  filter(str_detect(answer_text, "Yes,")) %>% 
  pull(respondent_id)

ans_levels <- answers %>% 
  select(1,2,3,4) %>% 
  filter(question_num == 9.1) %>% 
  arrange(desc(answer_num)) %>% 
  pull(answer_en)

summ <- survey_df %>% 
  filter(respondent_id %in% respondent_q9_1) %>% 
  filter(question_num == 9.1 & !is.na(answer_text)) %>% 
  group_by(answer_text) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  mutate(prop = n/sum(n)) %>% 
  arrange(prop) %>% 
  mutate(answer_text = factor(str_wrap(.$answer_text,40),
                              ordered=TRUE,
                              levels=str_wrap(ans_levels,40)))

plot_title = paste0(
  str_wrap(
    survey_df %>% filter(question_num == 9.1) %>% slice(1) %>% pull(question_text), 60),
  " (N=", sum(summ$n), ")")

plt <- ggplot(summ, aes(x="", y=prop, fill=answer_text)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position="bottom") +
  # geom_label_repel(aes(y = cumsum(prop)-prop*0.5, x = 1.5,
  #                      label= if_else(prop < 0.01, "<1%", sprintf("%1.0f%%", prop*100))),
  #                  color="black", size=6, fill="white", nudge_x = 0.5,
  #                  show.legend=FALSE) +
  ggtitle(plot_title) +
  guides(fill=guide_legend(reverse=TRUE, title=NULL, nrow=2)) +
  scale_fill_manual(values=rev(pal_seq4)) +
  theme(plot.title = element_text(face="bold"))

png(paste0("output/sfcta/plot/q9_1.png"), width = 8*300, height=6*300, res=300)
print(plt)
dev.off()

summ_income <- survey_df %>% 
  filter(respondent_id %in% respondent_q9_1) %>% 
  filter(question_num == 9.1  & !is.na(answer_text)) %>% 
  left_join(income_id, by="respondent_id") %>% 
  group_by(answer_text, income_id) %>% 
  summarise(n=n()) %>% 
  ungroup() %>%
  filter(!is.na(income_id)) %>% 
  group_by(income_id) %>%
  mutate(prop = n/sum(n),
         income_id_label = str_wrap(paste0(income_id, " (N=", sum(n), ")"),40)) %>% 
  ungroup() %>% 
  mutate(answer_text = factor(str_wrap(.$answer_text,40),
                              ordered=TRUE,
                              levels=rev(str_wrap(ans_levels,40))),
         income_id = factor(str_wrap(.$income_id,40),
                            ordered=TRUE,
                            levels=str_wrap(income_levels,40)))

summ_income_label <- summ_income %>% 
  distinct(income_id, income_id_label) %>% 
  arrange(income_id) %>% 
  pull(income_id_label)

plt <- ggplot(summ_income, aes(fill=answer_text, x=income_id, y=prop)) +
  geom_col(position = position_stack(reverse = TRUE)) + 
  geom_label(aes(x=income_id, y=prop, 
                 label = if_else(prop > 0.01, sprintf("%1.0f%%", prop*100), NULL),
                 group = answer_text),
             lineheight = 0.9,
             position = position_stack(vjust=0.5, reverse=TRUE),
             color = "white",
             size = 3,
             fill = "#666666") +
  coord_flip() +
  ggtitle(plot_title) +
  labs(x="", y="Share of Respondents") +
  scale_x_discrete(labels = summ_income_label) +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values = pal_seq4) +
  guides(fill=guide_legend(title=NULL, nrow=legend_row_num)) +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(face="bold"),
        axis.text=element_text(size=10))

png(paste0("output/sfcta/plot/q9_1_income.png"), width = 11*300, height=(4+length(ans_levels)*0.5)*300, res=300)
print(plt)
dev.off()  
  
  
  
  

### Question 9, 12 (Cross Tabs) ====
questions_lst <- c(8.1,9,10,12,13,17)
# questions_lst <- c(12)

for(question in 1:length(questions_lst)){
  i = questions_lst[question]
  print(i)

  q12_respondent_id <- survey_df %>%
    filter(question_num == 10 &
             !is.na(answer_text) &
             sub_question_text == "Shuttle Program" &
             answer_text %in% c("4. I may use this in the future given the right circumstances",
                                "5. I would absolutely use this in the future")) %>%
    pull(respondent_id)
  
  num_respondents <- survey_df %>% 
    filter(!respondent_id %in% unique(c(respondent_filter_1, respondent_filter_3))) %>% 
    filter(respondent_id %in% q12_respondent_id) %>% 
    filter(question_num == i & !is.na(answer_text)) %>% 
    distinct(respondent_id) %>% 
    nrow()
  
  if(i == 9){
    color_palette <- pal_div3
  } else if(i %in% c(8.1,10,13,17)){
    color_palette <- pal_seq5
  } else{
    color_palette <- rev(pal_seq4)
  }
  

  ans_levels <- answers %>% 
    select(1,2,3,4) %>% 
    filter(question_num == i) %>% 
    mutate(answer_en = case_when(str_detect(answer_en, "Other") ~"Other",
                                 TRUE ~answer_en)) %>% 
    arrange(desc(answer_num)) %>% 
    pull(answer_en)
  
  # OVERVIEW
  summ <- survey_df %>% 
    filter(!respondent_id %in% unique(c(respondent_filter_1, respondent_filter_3))) %>% 
    filter(respondent_id %in% q12_respondent_id) %>%
    filter(question_num == i & !is.na(answer_text)) %>% 
    group_by(answer_text) %>% 
    summarise(n=n()) %>% 
    ungroup() %>% 
    mutate(prop = n/sum(n)) %>% 
    arrange(prop) %>% 
    mutate(answer_text = factor(str_wrap(.$answer_text,40),
                                ordered=TRUE,
                                levels=str_wrap(ans_levels,40)))

  plot_title = paste0(
    str_wrap(
      survey_df %>% filter(question_num == i) %>% slice(1) %>% pull(question_text), 60),
    " (N=", num_respondents, ")")
  
  plt <- ggplot(summ,aes(x=answer_text,y=prop)) +
    geom_bar(stat="identity", fill="#006c69", alpha=0.9) +
    geom_label(aes(label=if_else(prop < 0.01, "<1%", sprintf("%1.0f%%", prop*100))), color="white", fill="#666666") +
    coord_flip() +
    ggtitle(plot_title) +
    xlab("") +
    ylab("Share of Respondents") +
    scale_y_continuous(labels=scales::percent) +
    theme(plot.title = element_text(face="bold"),
          axis.text=element_text(size=10))
  
  png(paste0("output/sfcta/plot/q",i,".png"), width = 8*300, height=(1+length(ans_levels)*0.4)*300, res=300)
  print(plt)
  dev.off()
  
  if(length(summ$answer_text) > 4){
    legend_row_num = 2
  } else{
    legend_row_num = 1
  }
  
  # CROSS-TAB
  summ_race <- survey_df %>% 
    filter(!respondent_id %in% unique(c(respondent_filter_1, respondent_filter_3))) %>%  
    filter(respondent_id %in% q12_respondent_id) %>%
    filter(question_num == i, !is.na(answer_text)) %>% 
    # convert "Other" responses
    mutate(answer_text = case_when(col_num == 25 ~"Other",
                                   TRUE ~answer_text)) %>% 
    left_join(race_id, by="respondent_id") %>% 
    group_by(answer_text, race_id) %>% 
    summarise(n=n()) %>% 
    ungroup() %>%
    filter(!is.na(race_id)) %>% 
    group_by(race_id) %>% 
    mutate(prop = n/sum(n),
           race_id_label = str_wrap(paste0(race_id, " (N=", sum(n), ")"),40)) %>% 
    ungroup() %>% 
    mutate(answer_text = factor(str_wrap(.$answer_text,40),
                                ordered=TRUE,
                                levels=rev(str_wrap(ans_levels,40))),
           race_id = factor(str_wrap(.$race_id,40),
                            ordered=TRUE,
                            levels=str_wrap(race_levels,40)))
  
  summ_race_label <- summ_race %>% 
    distinct(race_id, race_id_label) %>% 
    arrange(race_id) %>% 
    pull(race_id_label)
  
  plot_title <- paste0(
    str_wrap(
      survey_df %>% filter(question_num == i) %>% slice(1) %>% pull(question_text), 60),
    " (N=", sum(summ_race$n), ")")
  
  plt <- ggplot(summ_race, aes(x=race_id, y=prop, fill=answer_text)) +
    geom_col(position = position_stack(reverse = TRUE)) + 
    geom_label(aes(x=race_id, y=prop,
                   label = if_else(prop > 0.01, sprintf("%1.0f%%", prop*100), NULL),
                   group = answer_text),
               lineheight = 0.9,
               position = position_stack(vjust=0.5, reverse=TRUE),
               color = "white",
               size = 3,
               fill = "#666666") +
    coord_flip() +
    ggtitle(plot_title) +
    labs(x="", y="Share of Respondents") +
    scale_x_discrete(labels = summ_race_label) +
    scale_y_continuous(labels=scales::percent) +
    scale_fill_manual(values = rev(color_palette)) +
    guides(fill=guide_legend(title=NULL, nrow=legend_row_num)) +
    theme(legend.position="bottom") +
    theme(plot.title = element_text(face="bold"),
          axis.text=element_text(size=10))
  
  png(paste0("output/sfcta/plot/q",i,"_race.png"), width = 11*300, height=(1+length(race_levels)*0.5)*300, res=300)
  print(plt)
  dev.off()
  
  summ_income <- survey_df %>% 
    filter(!respondent_id %in% unique(c(respondent_filter_1, respondent_filter_3))) %>% 
    filter(respondent_id %in% q12_respondent_id) %>%
    filter(question_num == i, !is.na(answer_text)) %>% 
    # convert "Other" responses
    mutate(answer_text = case_when(col_num == 25 ~"Other",
                                   TRUE ~answer_text)) %>% 
    left_join(income_id, by="respondent_id") %>% 
    group_by(answer_text, income_id) %>% 
    summarise(n=n()) %>% 
    ungroup() %>%
    filter(!is.na(income_id)) %>% 
    group_by(income_id) %>%
    mutate(prop = n/sum(n),
           income_id_label = str_wrap(paste0(income_id, " (N=", sum(n), ")"),40)) %>% 
    ungroup() %>% 
    mutate(answer_text = factor(str_wrap(.$answer_text,40),
                                ordered=TRUE,
                                levels=rev(str_wrap(ans_levels,40))),
           income_id = factor(str_wrap(.$income_id,40),
                              ordered=TRUE,
                              levels=str_wrap(income_levels,40)))
  
  summ_income_label <- summ_income %>% 
    distinct(income_id, income_id_label) %>% 
    arrange(income_id) %>% 
    pull(income_id_label)
  
  plot_title <- paste0(
    str_wrap(
      survey_df %>% filter(question_num == i) %>% slice(1) %>% pull(question_text), 60),
    " (N=", sum(summ_income$n), ")")
  
  plt <- ggplot(summ_income, aes(fill=answer_text, x=income_id, y=prop)) +
    geom_col(position = position_stack(reverse = TRUE)) + 
    geom_label(aes(x=income_id, y=prop, 
                   label = if_else(prop > 0.01, sprintf("%1.0f%%", prop*100), NULL),
                   group = answer_text),
               lineheight = 0.9,
               position = position_stack(vjust=0.5, reverse=TRUE),
               color = "white",
               size = 3,
               fill = "#666666") +
    coord_flip() +
    ggtitle(plot_title) +
    labs(x="", y="Share of Respondents") +
    scale_x_discrete(labels = summ_income_label) +
    scale_y_continuous(labels=scales::percent) +
    scale_fill_manual(values = rev(color_palette)) +
    guides(fill=guide_legend(title=NULL, nrow=legend_row_num)) +
    theme(legend.position="bottom") +
    theme(plot.title = element_text(face="bold"),
          axis.text=element_text(size=10))
  
  png(paste0("output/sfcta/plot/q",i,"_income.png"), width = 11*300, height=(4+length(ans_levels)*0.5)*300, res=300)
  print(plt)
  dev.off()
}


survey_df %>% 
  filter(!respondent_id %in% unique(c(respondent_filter_1, respondent_filter_3))) %>% 
  distinct(respondent_id, language) %>% 
  count(language)



## Other Responses ====

library(tidytext)

## Define Functions ====
id_common_words <- function(data) {
  data %>%
    select(respondent_id, answer_text) %>% 
    unnest_tokens(word, answer_text) %>% 
    anti_join(stop_words) %>%
    group_by(word) %>% 
    summarise(num_respondents = n_distinct(respondent_id)) %>% 
    arrange(desc(num_respondents))
}

cat_common_words_14 <- function(data) {
  data %>% 
    mutate(category = case_when(str_detect(word, c("bike|bikes|bicycle")) ~"Bike Reliability & Infrastructure",
                                str_detect(word, c("drivers|shuttle")) ~"Driver Trust & Reliability",
                                str_detect(word, c("seat")) ~"Child Seat Availability & Proper Installation")) %>% 
    filter(!is.na(category)) %>% 
    group_by(category) %>% 
    summarise(count = sum(num_respondents))
}

cat_common_words_18 <- function(data) {
  data %>% 
    mutate(category = case_when(str_detect(word, c("ambassadors")) ~"Increased Safety Through Ambassadors",
                                str_detect(word, c("muni|transit")) ~"Transit Safety & Reliability",
                                str_detect(word, c("drop|parking")) ~"Pick-Up/Drop-Off guidelines",
                                str_detect(word, c("bike")) ~"Bicycle Infrastructure")) %>% 
    filter(!is.na(category)) %>% 
    group_by(category) %>% 
    summarise(count = sum(num_respondents))
}

cat_common_words_20 <- function(data) {
  data %>% 
    mutate(category = case_when(str_detect(word, c("transit")) ~"Transit Users are Knowledgeable",
                                str_detect(word, c("safe|safety")) ~"Safe Infrastructure")) %>% 
    filter(!is.na(category)) %>% 
    group_by(category) %>% 
    summarise(count = sum(num_respondents))
}


#c(8.1,10,14,18,20)
i= 11

other_question <- survey_raw %>% filter(question_num == i) %>% slice(1) %>% pull(question_text)

survey_raw %>% 
  filter(question_num == i, !is.na(answer_text)) %>% 
  select(respondent_id, language, question_text, answer_text) %>% 
  mutate(answer_text = str_to_lower(answer_text)) %>% 
  write_clip()


summ_other <- survey_raw %>% #distinct(col_num, question_text) %>% view()
  filter(question_num == i, !is.na(answer_text)) %>% 
  mutate(answer_text = str_to_lower(answer_text))

common_words <- id_common_words(summ_other)
common_words %>% write_clip()
other_cat <- cat_common_words_20(common_words)

plot_title = paste0(str_wrap(unique(survey_raw %>% filter(question_num == i) %>% slice(1) %>% pull(question_text)),60),
                    " (N=",
                    length(unique(summ_other$respondent_id)),
                    ")")

# plot_title = paste0(str_wrap("If you would like to tell us more about why or why you would not consider any of these options, please share.",60),
#                     " (N=",
#                     length(unique(summ_other$respondent_id)),
#                     ")")

plt <- ggplot(other_cat, aes(x=category, y=count)) +
  geom_col(fill="#006c69", alpha=0.9) +
  coord_flip() +
  ggtitle(plot_title) +
  labs(x="", y="Number of Respondents")

png(paste0("output/sfcta/plot/q",i,".png"), width = 8*300, height=(1+length(other_cat$category)*0.4)*300, res=300)
print(plt)
dev.off()




# Zipcode =====
survey_zipcode <- survey_raw %>% 
  filter(col_num == 54) %>% 
  mutate(value = case_when(str_detect(value,"94122 ") ~"94122",
                           TRUE ~value)) %>% 
  distinct(respondent_id, value) %>% 
  filter(!is.na(value))

# make map
gdb_path <- "G:/Current/SFCTA_OnCall_15_16_2016.0109/TO8_SF_Trans_Plan/Analysis/GDB/Base.gdb"
# ogrListLayers(gdb_path)
zipcode_sf <- st_read(gdb_path, layer="Zipcode")

survey_zipcode %>% count(value) %>% summarise(sum(n))

survey_zipcode_sf <- zipcode_sf %>% 
  left_join(survey_zipcode %>% count(value), by=c("zip"="value")) %>% 
  rename(num_survey_response=n)

library(arcgisbinding)
arc.check_product()

arc.write(file.path(gdb_path, "Boundaries/survey_zipcode"), survey_zipcode_sf, overwrite = FALSE)


# ID respondents
survey_id <- survey_raw %>% 
  filter(col_num %in% c(54,55,65,66)) %>% 
  bind_rows(., survey_race) %>% 
  mutate(col_num = case_when(is.na(col_num) ~56,
                             TRUE ~col_num),
         question = case_when(col_num == 56 ~"Do you identify yourself as (Check all that apply)",
                              TRUE ~question)) %>% 
  arrange(respondent_id, col_num)
  
answers_single <- survey_raw %>% 
  filter(col_num %in% c(10:53)) %>% 
  filter(question_type %in% c("Response","Open-Ended Response")) %>% view()
  
answers_other <- survey_raw %>% 
  filter(col_num %in% c(10:53)) %>% 
  filter(str_detect(question_type, "Other")) %>% 
  filter(!is.na(value))

answers_multiple <- survey_raw %>% 
  filter(col_num %in% c(10:53)) %>% 
  filter(!str_detect(question_type, "Other"),
         !question_type %in% c("Response","Open-Ended Response")) %>% view()


# "Open Ended Numeric"
# "Open Ended Text"
# "Multiple Choice - Multiple Selection"
# "Multiple Choice - Multiple Selection"


q_14 <- survey_raw %>% 
  filter(question_num == 14, !is.na(answer_text)) %>% 
  mutate(answer_text = str_to_lower(answer_text)) %>% 
  select(respondent_id, language, question_text, answer_text)
q_18 <- survey_raw %>% 
  filter(question_num == 18, !is.na(answer_text)) %>% 
  mutate(answer_text = str_to_lower(answer_text)) %>% 
  select(respondent_id, language, question_text, answer_text)
q_20 <- survey_raw %>% 
  filter(question_num == 20, !is.na(answer_text)) %>% 
  mutate(answer_text = str_to_lower(answer_text)) %>% 
  select(respondent_id, language, question_text, answer_text)

require(openxlsx)
list_of_datasets <- list("q_16"=q_14, "q_20"=q_18, "q_22"=q_20)
write.xlsx(list_of_datasets, file = "output/sfcta/survey_open_responses.xlsx")

