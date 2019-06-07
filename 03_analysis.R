# source of pdf files:
# https://intelligence.house.gov/social-media-content/social-media-advertisements.htm

library(pdftools)
library(tidyverse)
library(janitor)
library(lubridate)


#### run function in step 02 to handle processing of extracting from the pdf file
# source("02_solve_for_all_function.R")


# bring in the results data 
data <- readRDS("myresults_formatted_all.rds")


# check if formatted dataset came in successfully
data

# see characteristics of the data columns
glimpse(data)


### let's do some initial aggregate counts
data %>% 
  count(target_age) %>% 
  arrange(desc(n))

data %>% 
  count(target_location) %>% 
  arrange(desc(n)) 

data %>% 
  count(target_pplwhomatch) %>% 
  arrange(desc(n))

#looking at ads by date
data %>% 
  count(ad_creation_year) %>% 
  arrange(desc(n))

data %>% 
  count(ad_creation_year, ad_creation_month) %>% 
  arrange(ad_creation_year, ad_creation_month)

#bringing in sum of ad spend
data %>% 
  group_by(ad_creation_year, ad_creation_month) %>% 
  summarise(num_ads = n(), sum_spending = sum(ad_spend, na.rm = TRUE)) %>% 
  arrange(ad_creation_year, ad_creation_month)

#sum of impressions
data %>% 
  group_by(ad_creation_year, ad_creation_month) %>% 
  summarise(num_ads = n(), sum_impressions = sum(ad_impressions, na.rm = TRUE)) %>% 
  arrange(ad_creation_year, ad_creation_month)



### aiming to see if state-by-state spending can be compiled ####
names(data)

forstates <- data %>% 
  select(ad_spend,
         
         location_state1,
         location_state2,
         location_state3,
         location_state4,
         location_state5) %>% 
  mutate(numads = 1)

# 
# forstates %>% 
#   gather(key = state, value = ad_spend, 2:6, na.rm = FALSE)


tidystates <- forstates %>% 
  mutate(
    statecombo = paste0(location_state1, ",", location_state2, ",", location_state3, ",", location_state4, ",", location_state5)
  ) %>% 
  select(numads, ad_spend, statecombo) %>% 
  separate_rows(statecombo)

tidystates

#group by each state's ad count and spending
tidystates %>% 
  filter(statecombo != "NA") %>% 
  group_by(statecombo) %>% 
  summarise(ad_count = sum(numads, na.rm = TRUE), ad_spending = sum(ad_spend, na.rm = TRUE)) 


### BY STATE AND YEAR ####

names(data)

forstates_timeline <- data %>% 
  select(ad_spend,
         ad_creation_year,
         ad_creation_month,
         location_state1,
         location_state2,
         location_state3,
         location_state4,
         location_state5) %>% 
  mutate(numads = 1)

# 
# forstates %>% 
#   gather(key = state, value = ad_spend, 2:6, na.rm = FALSE)


tidystates_timeline <- forstates_timeline %>% 
  mutate(
    statecombo = paste0(location_state1, ",", location_state2, ",", location_state3, ",", location_state4, ",", location_state5)
  ) %>% 
  select(numads, ad_spend, ad_creation_year, ad_creation_month, statecombo) %>% 
  separate_rows(statecombo)

tidystates_timeline

#group by each state's ad count and spending
tidystates_timeline %>% 
  filter(statecombo != "NA") %>% 
  group_by(statecombo, ad_creation_year, ad_creation_month) %>% 
  summarise(ad_count = sum(numads, na.rm = TRUE), ad_spending = sum(ad_spend, na.rm = TRUE))

#write to file
tidystates_timeline %>% 
  filter(statecombo != "NA") %>% 
  group_by(statecombo, ad_creation_year, ad_creation_month) %>% 
  summarise(ad_count = sum(numads, na.rm = TRUE), ad_spending = sum(ad_spend, na.rm = TRUE)) %>% 
  write_csv("states_bymonth.csv")




#*********************************************************************
###### ANALYZING INTERESTS / PEOPLE WHO MATCH CHARACTERISTICS ########

forinterests_timeline <- data %>% 
  select(ad_creation_year,
         ad_creation_month,
         target_pplwhomatch) %>% 
  mutate(numads = 1)


# forinterests_timeline$ad_targeting_fulltext

# 
# forstates %>% 
#   gather(key = state, value = ad_spend, 2:6, na.rm = FALSE)


interests_working <- forinterests_timeline %>% 
  mutate(target_pplwhomatch = str_squish(target_pplwhomatch))


interests_working$target_pplwhomatch <- str_remove_all(interests_working$target_pplwhomatch, "Interests: ")
interests_working$target_pplwhomatch <- str_remove_all(interests_working$target_pplwhomatch, "Behaviors: ")
interests_working$target_pplwhomatch <- str_remove_all(interests_working$target_pplwhomatch, "School: ")
interests_working$target_pplwhomatch <- str_remove_all(interests_working$target_pplwhomatch, "Multicultural Affinity: ")
interests_working$target_pplwhomatch <- str_remove_all(interests_working$target_pplwhomatch, "Friends of connections: ")
interests_working$target_pplwhomatch <- str_remove_all(interests_working$target_pplwhomatch, "Politics: ")
  
tidyinterests_timeline <- interests_working %>% 
  separate_rows(target_pplwhomatch, sep = ",") %>% 
  mutate(
    target_pplwhomatch = str_squish(target_pplwhomatch)
  )

tidyinterests_timeline

#group by each state's ad count and spending
tidyinterests_timeline %>% 
  filter(target_pplwhomatch != "NA",
         target_pplwhomatch != "") %>% 
  group_by(target_pplwhomatch, ad_creation_year, ad_creation_month) %>% 
  summarise(ad_count = sum(numads, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(ad_creation_year, ad_creation_month, desc(ad_count)) %>% 
  View()


