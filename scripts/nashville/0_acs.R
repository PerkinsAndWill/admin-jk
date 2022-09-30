library(tidycensus)
library(data.table)
library(janitor)

nashville_downtown <- blk_group_lst$Nashville %>% 
  filter(GEOID %in% c("470370194012", "470370194021", "470370194022", "470370193001",
                      "470370194011", "470370195031", "470370195032", "470370195033",
                      "470370195022", "470370195023", "470370195021"))
mapview(nashville_downtown)


# ACS CENSUS ===================================================================
census_variables <- load_variables(year = 2020, dataset = "acs5", cache = TRUE)

## B01003 - TOTAL POPULATION ===================================================
lst_pop_code <- c("B01001_001")
lst_pop_name <- c("total_population")

acs_tot_pop_lst <- list()

for(i in 1:length(city)){
  lst_name <- city[i]
  
  a <- get_acs(geography = "block group",
               variables = lst_pop_code,
               state = state[i],
               year = 2020) %>% 
    filter(GEOID %in% nashville_downtown$GEOID) %>% 
    select(-moe) %>% 
    spread(., key = variable, value = estimate, sep = NULL) %>% 
    setnames(., old = lst_pop_code, new = lst_pop_name)# %>% 
    # left_join(tracts_lst[[city[i]]], by = "GEOID") %>% 
    # st_as_sf() %>% 
    # mutate(total_population_acre = total_population / area_acre)
  
  tmp <- list(a)
  acs_tot_pop_lst[lst_name] <- tmp
}

sum(acs_tot_pop_lst$Nashville$total_population)

## B19013 - MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS ======================
med_hh_income_vars <- census_variables %>% 
  filter(str_detect(name, "B19013_001"), geography == "block group") %>% #B19049_001
  mutate(label = "med_hh_income") %>% 
  select(name, label)

acs_med_hh_income_lst <- list()

for(i in 1:length(city)){
  lst_name <- city[i]
  
  a <- get_acs(geography = "block group",
               variables = med_hh_income_vars$name,
               state = state[i],
               county = county[i],
               year = 2020) %>% 
    filter(GEOID %in% nashville_downtown$GEOID) %>% 
    select(-moe) %>% 
    spread(., key = variable, value = estimate, sep = NULL) %>% 
    setnames(., old = med_hh_income_vars$name, new = med_hh_income_vars$label) %>% 
    clean_names()
  
  tmp <- list(a)
  acs_med_hh_income_lst[lst_name] <- tmp
}

mean(acs_med_hh_income_lst$Nashville$med_hh_income, na.rm=TRUE)

## B19013 - MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS ======================
mode_vars <- census_variables %>% 
  filter(str_detect(name, "B08301"), geography == "block group")
 
acs_mode_lst <- list()

for(i in 1:length(city)){
  lst_name <- city[i]
  
  a <- get_acs(geography = "block group",
               variables = mode_vars$name,
               state = state[i],
               county = county[i],
               year = 2020) %>% 
    filter(GEOID %in% nashville_downtown$GEOID) %>% 
    select(-moe) %>% 
    spread(., key = variable, value = estimate, sep = NULL) %>% 
    setnames(., old = mode_vars$name, new = mode_vars$label) %>% 
    clean_names()
  
  tmp <- list(a)
  acs_mode_lst[lst_name] <- tmp
}

acs_mode_lst$Nashville %>% view()

