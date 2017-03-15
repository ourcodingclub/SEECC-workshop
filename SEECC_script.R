# SEECC LPI Data 
# John Godlee (johngodlee@gmail.com)

# Packages ----
library(dplyr)
# Set working directory to source location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load data ----
load("LPIUKall.RData")
load("LPIdata_Feb2016.RData")

# Clean data ----
## Remove whitespace in variable names 
names(LPIdata_Feb2016) <- gsub(" ", "_", names(LPIdata_Feb2016), fixed = TRUE)
names(LPIdata_Feb2016)

## Get only records containing UK
LPIdata_Feb2016_UK <- LPIdata_Feb2016 %>%
  filter(., grepl("United Kingdom", Country_list))



# Create ggmaps of record distributions ----
## Single species

## Automate for all species using *apply()



