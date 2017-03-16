# SEECC LPI Data 
# John Godlee (johngodlee@gmail.com)

# Packages ---
library(devtools)
library(ggmap)
library(ggplot2)
library(dplyr)
library(data.table)

# Set working directory to source location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load data ----
load("LPIUKall.RData")
load("LPIdata_Feb2016.RData")
  # load("GBIF.RData")
Atlantic_puffin <- fread("Atlantic_puffin.csv", sep = "\t")

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
  geom_point(aes(colour = Class), size = 5)

### Maps of class spatial distributions
ggplot(LPIdata_Feb2016, aes(x = Decimal_Longitude, y = Decimal_Latitude, colour = Class)) + geom_point()
ggplot(LPIdata_Feb2016_UK, aes(x = Decimal_Longitude, y = Decimal_Latitude, colour = Class)) + geom_point()

### How does the length of time the population has been studied affect the slope estimate?
ggplot(LPIUKall, aes(x = lengthyear, y = slope)) + 
  geom_point()

# Create ggmaps of record distributions ----
## Single species

## Automate for all species using *apply()

# Create a map of GBIF data from the Puffin ----
bbox <- c(min(Atlantic_puffin$decimallongitude) - 2,
          min(Atlantic_puffin$decimallatitude) - 2,
          max(Atlantic_puffin$decimallongitude) + 2,
          max(Atlantic_puffin$decimallatitude) + 2
          )

Map <- get_map(location=bbox, source="stamen", maptype="toner")

ggmap(Map) +
  geom_point(aes(x = decimallongitude,
                 y = decimallatitude), colour = "blue",
             data = Atlantic_puffin, 
             alpha = 0.1,
             size = 2) +
  xlab(expression("Decimal Longitude ("*degree*")")) +
  ylab(expression("Decimal Latitude ("*degree*")")) + 
  theme(legend.title=element_blank())

