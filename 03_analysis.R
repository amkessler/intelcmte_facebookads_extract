# source of pdf files:
# https://intelligence.house.gov/social-media-content/social-media-advertisements.htm

library(pdftools)
library(tidyverse)
library(janitor)
library(lubridate)


# run function in step 02 to handle processing of extracting from the pdf file
source("02_solve_for_all_function.R")

# check if formatted dataset came in successfully
myresults_formatted

glimpse(myresults_formatted)
