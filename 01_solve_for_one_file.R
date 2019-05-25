# source of pdf files:
# https://intelligence.house.gov/social-media-content/social-media-advertisements.htm

library(pdftools)
library(tidyverse)

myfile <- "pdfs/P10004855.pdf"

text <- pdf_text(myfile)[1]

text

txt <- pdf_text(myfile)
cat(txt[1])


# All textboxes on page 1 - doesn't seem to help here
textboxes <- pdf_data(myfile)[[1]]
textboxes


##extract between two words using gsub and regex ####

text

gsub(".*Ad ID\\s*|Ad Text.*", "", text)

gsub(".*Ad Text\\s*|Ad Landing.*", "", text)

gsub(".*Ad Targeting\\s*|Ad Impressions.*", "", text)

gsub(".*Ad Spend\\s*|Ad Creation.*", "", text)

cat(text)

#trying out a tibble
tibble("AdID", gsub(".*Ad ID\\s*|Ad Text.*", "", text),
       "fdsa")

#building the whole thing out to capture all the information on the page
sampleresult <- tibble(
  ad_id = str_trim(gsub(".*Ad ID\\s*|Ad Text.*", "", text)),
  ad_text = str_trim(gsub(".*Ad Text\\s*|Ad Landing.*", "", text)),
  ad_landing_page = str_trim(gsub(".*Ad Landing Page\\s*|Ad Targeting.*", "", text)),
  ad_impressions = str_trim(gsub(".*Ad Impressions\\s*|Ad Clicks.*", "", text)),
  ad_clicks = str_trim(gsub(".*Ad Clicks\\s*|Ad Spend.*", "", text)),
  ad_spend = str_trim(gsub(".*Ad Spend\\s*|Ad Creation.*", "", text)),
  ad_creation_date = str_trim(gsub(".*Ad Creation Date\\s*|Redactions.*", "", text)),
  ad_targeting = str_squish(gsub(".*Ad Targeting\\s*|Ad Impressions.*", "", text))
)

sampleresult

#write to file
write_csv(sampleresult, "sampleresult.csv")



#...................................

#experiment with getting targeting broken down further - end of line regex ####

# https://stackoverflow.com/questions/830855/what-regex-would-capture-everything-from-mark-to-the-end-of-a-line

# (?<=MYCHAR).*$
gsub("Age.*", "", text)
text

#to visually see regex matching
# https://r4ds.had.co.nz/strings.html
str_view(text, "Age.*")

str_subset(text, "Age.*")

#yes! This appears to do it. Pulling until end of line.
str_extract(text, "Age.*")

#Applying to other variations
str_extract(text, "Language.*")
str_extract(text, "Location.*")
str_extract(text, "Excluded Connections.*")

#since this one goes beyond just one line, it gets cut off 
str_extract(text, "Placement.*")


sample2 <- tibble(
  target_age = str_trim(str_remove(str_extract(text, "Age.*"), "Age:")),
  target_location = str_trim(str_remove(str_extract(text, "Location.*"), "Location -")),
  target_language = str_trim(str_remove(str_extract(text, "Language.*"), "Language:"))
  )

sample2


### Combining everything together ####
# plus adding some new fields


sampleresults_combined <- tibble(
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

sampleresults_combined
