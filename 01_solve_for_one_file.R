# https://intelligence.house.gov/social-media-content/social-media-advertisements.htm

library(pdftools)
library(tidyverse)

myfile <- "pdfs/P0004855.pdf"

text <- pdf_text(myfile)[1]

text

txt <- pdf_text(myfile)
cat(txt[1])


# All textboxes on page 1 - doesn't seem to help here
textboxes <- pdf_data(myfile)[[1]]


##extract between two words using gsub and regex

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

#write to file
write_csv(sampleresult, "sampleresult.csv")
