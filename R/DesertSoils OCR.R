library(rJava)
library(tabulizer)
library(dplyr)
library(tabulizerjars)
library(tidyverse)


tab <- extract_tables("C:/Projects/TernLandscapes/Site Data/recentlyUploadedProjectsListing20190404.pdf")

location <- 'http://www.edd.ca.gov/jobs_and_training/warn/WARN-Report-for-7-1-2016-to-10-25-2016.pdf'

# Extract the table
out <- extract_tables('C:/Projects/TernLandscapes/Site Data/DuneFields/b19059048-pell_s_appendices.pdf')
