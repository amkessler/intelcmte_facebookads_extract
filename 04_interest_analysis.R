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

glimpse(data)

#first handle people who match field
working_combo <- data %>% 
  select(ad_creation_year,
         ad_creation_month,
         target_pplwhomatch, 
         ad_targeting_fulltext) %>% 
  mutate(
    numads = 1,
    pplwhomatch = str_squish(target_pplwhomatch),
    pplwhomatch = str_trim(str_remove_all(target_pplwhomatch, ".*:"))
  )

#ad unique ID column to help in later steps
working_combo <- rowid_to_column(working_combo, "ID")

#handle full text targeting field
working_combo <- working_combo %>% 
  mutate(
    fulltext = str_squish(ad_targeting_fulltext),
    ft_interests = if_else(
      str_detect(fulltext, "Interests:"),
      str_trim(gsub(".*Interests\\s*|Age.*", "", fulltext)),
      "")  
  )

#clean up and isolate the new columns
working_combo <- working_combo %>% 
  select(ID, 
         numads, 
         ad_creation_year,
         ad_creation_month,
         pplwhomatch, 
         ft_interests) %>% 
  mutate(
    pplwhomatch = str_squish(pplwhomatch),
    ft_interests = str_squish(ft_interests)
  )

#look for instances where pplwhomatch is blank but ft_interests is not
working_combo %>% 
  filter(pplwhomatch == "",
         ft_interests != "")
  
#create column combining interest columns
working_combo <- working_combo %>% 
  mutate(
    interests_combined = if_else(pplwhomatch == "", ft_interests, pplwhomatch)
  )



#### GROUPING THE RESULTS ####


#tidy format from the interests combined column
working_tidycombo <- working_combo %>% 
  select(ID, 
         numads, 
         ad_creation_year,
         ad_creation_month,
         interests_combined) %>% 
  separate_rows(interests_combined, sep = ",") %>% 
  mutate(
    interests_combined = str_squish(interests_combined)
  )
  
#clean up by removing certain titles
working_tidycombo$interests_combined <- str_remove_all(working_tidycombo$interests_combined, "Interests: ")
working_tidycombo$interests_combined <- str_remove_all(working_tidycombo$interests_combined, "Behaviors: ")
working_tidycombo$interests_combined <- str_remove_all(working_tidycombo$interests_combined, "School: ")
working_tidycombo$interests_combined <- str_remove_all(working_tidycombo$interests_combined, "Multicultural Affinity: ")
working_tidycombo$interests_combined <- str_remove_all(working_tidycombo$interests_combined, "Friends of connections: ")
working_tidycombo$interests_combined <- str_remove_all(working_tidycombo$interests_combined, "Politics: ")
working_tidycombo$interests_combined <- str_remove_all(working_tidycombo$interests_combined, "Jr.")
working_tidycombo$interests_combined <- str_remove(working_tidycombo$interests_combined, ":")

#remove blank rows
working_tidycombo <- working_tidycombo %>% 
  filter(interests_combined != "") %>% 
  mutate(
    interests_combined = str_trim(interests_combined)
  )

#group by each target term's total ad count 
working_tidycombo %>% 
  group_by(interests_combined) %>% 
  summarise(ad_count = sum(numads, na.rm = TRUE)) %>% 
  arrange(desc(ad_count))


#group by each target term's total ad count BY MONTH
working_tidycombo %>% 
  group_by(ad_creation_year, ad_creation_month, interests_combined) %>% 
  summarise(ad_count = sum(numads, na.rm = TRUE)) %>% 
  arrange(ad_creation_year, ad_creation_month, desc(ad_count))

