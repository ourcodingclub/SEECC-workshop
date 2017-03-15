# SEECC LPI Data 
# John Godlee (johngodlee@gmail.com)

# Packages ----
library(dplyr)
library(ggplot2)
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

# Explore data ----
## Plot a dot plot of the number of counts of various `Class` groups
### Create summary data frame
LPI_UK_class_summ <- LPIdata_Feb2016_UK %>%
  group_by(Class) %>%
  summarise(n = n())

### Create dot plot
ggplot(LPI_UK_class_summ, aes(x = Class, y = n)) + 
  geom_point(aes(colour = Class), size = 5) + 
  theme(axis.title = element_text(size = 16), 
        axis.text = element_text(size = 12))

# Create ggmaps of record distributions ----
## Single species

## Automate for all species using *apply()



