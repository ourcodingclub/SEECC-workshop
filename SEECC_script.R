# Visualising species occurrence and quantifying population change
# John Godlee (johngodlee@gmail.com)
# Gergana Daskalova (gndaskalova@gmail.com)
# Isla Myers-Smith (isla.myers-smith@ed.ac.uk)

# Packages ---
library(devtools)
library(ggmap)
library(ggplot2)
library(dtplyr)
library(dplyr)
library(data.table)
library(ggExtra)

# Set working directory to source location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load data ----
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
  geom_point(aes(colour = Class), size = 5)

### Maps of class spatial distributions
ggplot(LPIdata_Feb2016, aes(x = Decimal_Longitude, y = Decimal_Latitude, colour = Class)) + geom_point()
ggplot(LPIdata_Feb2016_UK, aes(x = Decimal_Longitude, y = Decimal_Latitude, colour = Class)) + geom_point()

### How does the length of time the population has been studied affect the slope estimate?
ggplot(LPIUKall, aes(x = lengthyear, y = slope)) + 
  geom_point()

# Plots by biome (histograms of lm estimates of pop change)
biome <- data %>%
  mutate(Year = parse_number(Year)) %>%
  filter(!is.na(Pop) & Are.coordinates.for.specific.location.=="TRUE") %>%
  select(Common.Name,Location.of.population,Country.list,biome,Year,Pop,system,Native,Alien) %>%
  group_by(Common.Name,Location.of.population,Country.list,biome,system,Native,Alien) %>%
  filter(length(unique(Year)) > 2) %>%
  do(fit = lm(Pop ~ Year, data = .)) %>%
  tidy(fit) %>%
  ungroup() %>%
  group_by(biome) %>%
  do(ggsave(ggplot(.,aes(x = estimate))+geom_histogram(),filename = gsub(" ","",paste("Biome_LPI/",unique(as.character(.$biome)),".pdf",sep="")),device="pdf"))
  
<<<<<<< HEAD
# Make linear models for each population ----


=======
# Gergana will add in plot with ggExtra and marginal histograms
>>>>>>> a690ff0f18ad49a2ed9322cb7a5f57a68eb6f102

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

