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



### aiming to see if state-by-state spending can be compiled ####
names(data)

tidyinterests <- data %>% 
  select(target_pplwhomatch) %>% 
  mutate(numads = 1) %>% 
  separate_rows(target_pplwhomatch, sep = ",") %>% 
  mutate(target_pplwhomatch = str_squish(target_pplwhomatch))
  

#clean out precedors like interest: and politics:
tidyinterests

tidyinterests


tidyinterests %>% 
  count(target_pplwhomatch) %>% 
  arrange(desc(n)) %>% 
  View()


#group by each state's ad count and spending
tidystates %>% 
  filter(statecombo != "NA") %>% 
  group_by(statecombo) %>% 
  summarise(ad_count = sum(numads, na.rm = TRUE), ad_spending = sum(ad_spend, na.rm = TRUE)) 


