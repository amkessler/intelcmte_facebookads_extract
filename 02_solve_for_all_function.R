# source of pdf files:
# https://intelligence.house.gov/social-media-content/social-media-advertisements.htm

library(pdftools)
library(tidyverse)
library(janitor)
library(lubridate)


# function to handle processing of extracting from the pdf file

extractmydata <- function(myfile) {
  
  text <- pdf_text(myfile)[1]
  
  df <- tibble(
    document_name = myfile,
    # ad_id = str_trim(gsub(".*Ad ID\\s*|Ad Text.*", "", text)),
    ad_id = str_trim(str_remove(str_extract(text, "Ad ID.*"), "Ad ID")),
    ad_text = if_else(
      str_detect(text, "Ad Text"),
      str_trim(gsub(".*Ad Text\\s*|Ad Landing.*", "", text)),
      ""),
    ad_landing_page = str_trim(gsub(".*Ad Landing Page\\s*|Ad Targeting.*", "", text)),
    ad_impressions = if_else(
      str_detect(text, "Ad Impressions"),
      str_trim(gsub(".*Ad Impressions\\s*|Ad Clicks.*", "", text)),
      ""),
    ad_clicks = if_else(
      str_detect(text, "Ad Clicks"),
      str_trim(gsub(".*Ad Clicks\\s*|Ad Spend.*", "", text)),
      ""),
    ad_spend = if_else(
      str_detect(text, "Ad Spend"),
      str_trim(gsub(".*Ad Spend\\s*|Ad Creation.*", "", text)),
      ""),
    ad_creation_date = str_squish(gsub(".*Ad Creation Date\\s*|P.*", "", text)), #stop at the P in PST/PDT
    ad_end_date = if_else(
      str_detect(text, "Ad End Date"),
      str_squish(gsub(".*Ad End Date\\s*|P.*", "", text)), #stop at the P in PST/PDT
      ""),
    target_age = str_trim(str_remove(str_extract(text, "Age.*"), "Age:")),
    target_location = str_trim(str_remove(str_extract(text, "Location.*"), "Location -")),
    target_language = str_trim(str_remove(str_extract(text, "Language.*"), "Language:")),
    target_pplwhomatch = if_else(
      str_detect(text, "People Who Match"),
      str_trim(gsub(".*People Who Match\\s*|Ad Impressions.*", "", text)),
      ""),
    ad_targeting_fulltext = str_squish(gsub(".*Ad Targeting\\s*|Ad Impressions.*", "", text))
    )
  
  print(myfile)
  return(df) 

  }

# run the function on a single pdf
extractmydata("pdfs/P(1)0000001.pdf")

# success!


#### Now let's do this for ALL the files ####

# get a list of all the files in the pdfs directory
allfiles <- list.files("./pdfs", full.names = TRUE)
allfiles

# run a purrr map function (ie looping) to apply our processing to every file
# map_df(allfiles, extractmydata)

# save results into new dataframe
myresults <- map_df(allfiles, extractmydata)

#write to file
write_csv(myresults, "myresults.csv")



### now we'll clean up some formatting of the results data ####

glimpse(myresults)

#convert impressions and clicks to numberic, dates to date format
#also pull out y, m, d from dates and trim document_name to remove path itself
myresults_formatted <- myresults %>% 
  mutate(
    ad_impressions = parse_number(ad_impressions), #use readr's parse_number to handle commas in text
    ad_clicks = parse_number(ad_clicks),
    ad_creation_date = mdy_hms(ad_creation_date),
    ad_creation_year = year(ad_creation_date),
    ad_creation_month = month(ad_creation_date),
    ad_creation_day = day(ad_creation_date),
    ad_end_date = mdy_hms(ad_end_date),
    document_name = str_sub(document_name, 8, 50),
    target_pplwhomatch = str_remove(target_pplwhomatch, ": ")
  )

#handle ocassional "None" in ad spend instead of a zero/blank
myresults_formatted <- myresults_formatted %>% 
  mutate(
    ad_spend = str_replace(ad_spend, "None", "")
  )

# pull currency and ad spend work
currency_vector <- str_extract(myresults_formatted$ad_spend, "RUB")

myresults_formatted <- myresults_formatted %>% 
  mutate(
    currency = currency_vector,
    ad_spend = str_replace(ad_spend, "RUB", ""),
    ad_spend = parse_number(ad_spend)
  )

# move currency column next to ad spend, year month and day next to date
myresults_formatted <- myresults_formatted %>% 
  select(1:7, currency, ad_creation_date, ad_creation_year, ad_creation_month,
        ad_creation_day, everything())

#write to file
write_csv(myresults_formatted, "myresults_formatted.csv")