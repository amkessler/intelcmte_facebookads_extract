# source of pdf files:
# https://intelligence.house.gov/social-media-content/social-media-advertisements.htm

library(pdftools)
library(tidyverse)


# function to handle processing of extracting from the pdf file

extractmydata <- function(myfile) {
  
  text <- pdf_text(myfile)[1]
  
  df <- tibble(
    ad_id = str_trim(gsub(".*Ad ID\\s*|Ad Text.*", "", text)),
    ad_text = str_trim(gsub(".*Ad Text\\s*|Ad Landing.*", "", text)),
    ad_landing_page = str_trim(gsub(".*Ad Landing Page\\s*|Ad Targeting.*", "", text)),
    ad_impressions = str_trim(gsub(".*Ad Impressions\\s*|Ad Clicks.*", "", text)),
    ad_clicks = str_trim(gsub(".*Ad Clicks\\s*|Ad Spend.*", "", text)),
    ad_spend = str_trim(gsub(".*Ad Spend\\s*|Ad Creation.*", "", text)),
    ad_creation_date = str_trim(gsub(".*Ad Creation Date\\s*|Redactions.*", "", text)),
    target_age = str_trim(str_remove(str_extract(text, "Age.*"), "Age:")),
    target_location = str_trim(str_remove(str_extract(text, "Location.*"), "Location -")),
    target_language = str_trim(str_remove(str_extract(text, "Language.*"), "Language:")),
    target_pplwhomatch = str_trim(gsub(".*People Who Match\\s*|Ad Impressions.*", "", text)),
    ad_targeting_fulltext = str_squish(gsub(".*Ad Targeting\\s*|Ad Impressions.*", "", text))
    )
  
  return(df) 

  }

# run the function on a single pdf
extractmydata("pdfs/P10004855.pdf")

# success!


#### Now let's do this for ALL the files ####

# get a list of all the files in the pdfs directory
allfiles <- list.files("./pdfs", full.names = TRUE)
allfiles

# run a purrr map function (ie looping) to apply our processing to every file
map_df(allfiles, extractmydata)


#write to file
# write_csv(sampleresults_combined, "sampleresults_combined.csv")
