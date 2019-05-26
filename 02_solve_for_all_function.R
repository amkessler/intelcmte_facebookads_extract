# source of pdf files:
# https://intelligence.house.gov/social-media-content/social-media-advertisements.htm

library(pdftools)
library(tidyverse)


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
      "None"),
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
    target_age = str_trim(str_remove(str_extract(text, "Age.*"), "Age:")),
    target_location = str_trim(str_remove(str_extract(text, "Location.*"), "Location -")),
    target_language = str_trim(str_remove(str_extract(text, "Language.*"), "Language:")),
    target_pplwhomatch = if_else(
      str_detect(text, "People Who Match"),
      str_trim(gsub(".*People Who Match\\s*|Ad Impressions.*", "", text)),
      "None"),
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
