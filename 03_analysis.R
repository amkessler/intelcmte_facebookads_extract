# source of pdf files:
# https://intelligence.house.gov/social-media-content/social-media-advertisements.htm

library(pdftools)
library(tidyverse)
library(janitor)
library(lubridate)


#### run function in step 02 to handle processing of extracting from the pdf file
# source("02_solve_for_all_function.R")


# bring in the results data 
data <- readRDS("myresults_formatted.rds")


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

data %>% 
  count(ad_creation_year) %>% 
  arrange(desc(n))

data %>% 
  count(ad_creation_year, ad_creation_month) %>% 
  arrange(ad_creation_year, ad_creation_month)
