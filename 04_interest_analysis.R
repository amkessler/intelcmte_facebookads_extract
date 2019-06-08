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




#............................................
### INTEREST TARGETING BREAKDOWNS ####

names(data)

tidyinterests <- data %>% 
  select(target_pplwhomatch) %>% 
  mutate(numads = 1) %>% 
  separate_rows(target_pplwhomatch, sep = ",") %>% 
  mutate(target_pplwhomatch = str_squish(target_pplwhomatch))
  

#clean out precedors like interest: and politics:
tidyinterests <- tidyinterests %>% 
  mutate(
    match = str_trim(str_remove_all(target_pplwhomatch, ".*:"))
  )



#count num of ads for each match label
tidyinterests %>% 
  count(match) %>% 
  arrange(desc(n)) %>% 
  View()




#group by each state's ad count and spending
tidystates %>% 
  filter(statecombo != "NA") %>% 
  group_by(statecombo) %>% 
  summarise(ad_count = sum(numads, na.rm = TRUE), ad_spending = sum(ad_spend, na.rm = TRUE)) 



### pull out interests from fulltext targeting ####

working <- data %>% 
  select(ad_targeting_fulltext) %>% 
  mutate(
    numads = 1,
    fulltext = str_squish(ad_targeting_fulltext)
    ) %>% 
  select(-ad_targeting_fulltext)

working %>% 
  mutate(
    ft_interests = if_else(
      str_detect(fulltext, "Interests:"),
      str_trim(gsub(".*Interests\\s*|Age.*", "", fulltext)),
      "")    
  ) %>% 
  View()



#### COMBINE BOTH METHODS ####

glimpsenames(data)

#first handle people who match field
working_combo <- data %>% 
  select(target_pplwhomatch, ad_targeting_fulltext) %>% 
  mutate(
    numads = 1,
    pplwhomatch = str_squish(target_pplwhomatch),
    pplwhomatch = str_trim(str_remove_all(target_pplwhomatch, ".*:"))
  )

#then handle full text targeting field
working_combo <- working_combo %>% 
  mutate(
    fulltext = str_squish(ad_targeting_fulltext),
    ft_interests = if_else(
      str_detect(fulltext, "Interests:"),
      str_trim(gsub(".*Interests\\s*|Age.*", "", fulltext)),
      "")  
  )



working <- data %>% 
  select(ad_targeting_fulltext) %>% 
  mutate(
    numads = 1,
    fulltext = str_squish(ad_targeting_fulltext)
  ) %>% 
  select(-ad_targeting_fulltext)

working %>% 
  mutate(
    ft_interests = if_else(
      str_detect(fulltext, "Interests:"),
      str_trim(gsub(".*Interests\\s*|Age.*", "", fulltext)),
      "")    
  ) %>% 
  View()
