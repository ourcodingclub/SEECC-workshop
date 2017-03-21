# Visualising species occurrence and quantifying population change
# John Godlee (johngodlee@gmail.com)
# Gergana Daskalova (gndaskalova@gmail.com)
# Isla Myers-Smith (isla.myers-smith@ed.ac.uk)

# Packages ---
library(devtools)
library(ggplot2)
library(ggmap)
library(ggExtra)
library(dplyr)
library(data.table)
library(tidyr)
library(readr)

# Set working directory to source location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load data ----
LPIdata_Feb2016 <- read.csv("LPIdata_Feb2016.csv")
LPIdata_Feb2016 <- LPIdata_Feb2016[-3796,]      # We should delete these rows and save the file again just for the purpose of the tutorial since they mess up plots
LPIdata_Feb2016 <- LPIdata_Feb2016[-3798,]
LPIdata_Feb2016 <- LPIdata_Feb2016[-3825,]
LPIdata_Feb2016 <- LPIdata_Feb2016[-4193,]
LPIdata_Feb2016 <- LPIdata_Feb2016[-7886,]
LPIdata_Feb2016 <- LPIdata_Feb2016[-13101,]
LPIdata_Feb2016 <- LPIdata_Feb2016[-14354,]
LPIdata_Feb2016 <- LPIdata_Feb2016[-15310,]
LPIdata_Feb2016 <- LPIdata_Feb2016[-15327,]
LPIdata_Feb2016 <- LPIdata_Feb2016[-15327,]
LPIdata_Feb2016 <- LPIdata_Feb2016[-15327,]
LPIdata_Feb2016 <- LPIdata_Feb2016[-15327,]
LPIdata_Feb2016 <- LPIdata_Feb2016[-15327,]
LPIdata_Feb2016 <- LPIdata_Feb2016[-15327,]
LPIdata_Feb2016 <- LPIdata_Feb2016[-15327,]
LPIdata_Feb2016 <- LPIdata_Feb2016[-15327,]
LPIdata_Feb2016 <- LPIdata_Feb2016[-15327,]
LPIdata_Feb2016 <- LPIdata_Feb2016[-15327,]
LPIdata_Feb2016 <- LPIdata_Feb2016[-15327,]
LPIdata_Feb2016 <- LPIdata_Feb2016[-16412,]
save(LPIdata_Feb2016, file = "LPIdata_Feb2016.RData")

# Inspect data ----
View(head(LPIdata_Feb2016))

# Clean data ----
## Remove whitespace in variable names 
names(LPIdata_Feb2016)
names(LPIdata_Feb2016) <- gsub(" ", "_", names(LPIdata_Feb2016), fixed = TRUE)
names(LPIdata_Feb2016)

lower <- function (df) {
  names(df) <- tolower(names(df))
  df
}
LPIdata_Feb2016 <- lower(LPIdata_Feb2016)

## Transform to long format, add useful columns, remove rows without sufficient data
LPI_long <- LPIdata_Feb2016 %>%
  gather("year", "pop", select = 26:70) %>%  # Transform to long format
  mutate(year = parse_number(.$year)) %>%  # Deprecated, extract_numeric() -> parse_numeric() -> parse_number(), extract numeric from atomic
  mutate(., genus_species = paste(genus, species, sep = '_')) %>%  # Create a species column by concatenating genus and species
  distinct(.) %>%  # Remove duplicate rows
  filter(., is.finite(pop)) %>%  # Keep only rows with a population estimate
  group_by(., common_name, genus_species, id) %>%  # group rows so that each group is one population (id) from one species (species+Common.Name)
  mutate(., maxyear = max(year), minyear = min(year)) %>%  # Create a column for the max and min years for each group
  mutate(., lengthyear = maxyear-minyear) %>%  # Create a column for the length of time data available
  ungroup(.) %>%  # Remove groupings
  group_by(., common_name, genus_species, id, units) %>%  # Groups Measurement_type(Units)>population(id)>species(Common.Name+species)
  mutate(., scalepop = (pop-min(pop))/(max(pop)-min(pop))) %>%  # Scale population trend from 0 to 1
  filter(., is.finite(scalepop)) %>%  # Remove rows without a scalepop
  mutate(., meanpop = mean(pop)) %>%  # Create column for mean population
  ungroup(.) %>%
  group_by(., common_name, genus_species, id) %>%
  mutate(., meanpop.size = mean(meanpop)) %>%  # Create column for mean mean population
  ungroup(.)



# Make linear models for each population ----
LPI_models <- LPI_long %>%
  group_by(., common_name, genus_species, id, units) %>%  # Groups Measurement_type(Units)>population(id)>species(Common.Name+species)
  do(mod = lm(scalepop~year, data = .)) %>%  # Create a linear model for each group
  mutate(., n = df.residual(mod),  # Create columns: degrees of freedom
         intercept=summary(mod)$coeff[1],  # intercept coefficient
         slope=summary(mod)$coeff[2],  # slope coefficient
         intercept_se=summary(mod)$coeff[3],  # standard error of intercept
         slope_se=summary(mod)$coeff[4],  # standard error of slope
         intercept_p=summary(mod)$coeff[7],  # p value of intercept
         slope_p=summary(mod)$coeff[8]) %>%  # p value of slope
  filter(., n > 5) # Remove rows where degrees of freedom <5

# Merge data frames back together
LPI_models_slopes <- merge(LPI_long, LPI_models) %>%
  select(genus_species, class, id, units, mod, n,  intercept, slope, intercept_se, slope_se,
         intercept_p, slope_p, decimal_latitude, decimal_longitude, biome, realm,
         maxyear, minyear, lengthyear, meanpop, meanpop.size, country_list, 
         location_of_population, biome, realm, region, system, native, alien, pop) %>% # select only useful columns
  distinct(.)  # Remove any duplicate rows

# Explore data ----
## Dot plot of species dataset size, grouped by respective species class
LPI_class_summ <- LPI_long %>%
  group_by(genus_species, class) %>%
  summarise(n = n())

ggplot(LPI_class_summ, aes(x = class, y = n)) + 
  geom_point(aes(colour = class), size = 2, alpha = 0.6)

## Map of class distributions
map_world <- borders("world", colour="gray50")
ggplot() + map_world + 
  geom_point(data = LPI_models_slopes, aes(x = decimal_longitude, y = decimal_latitude, colour = class))

### How does the length of time the population has been studied affect the slope estimate?
ggplot(LPI_models_slopes, aes(x = lengthyear, y = slope)) + 
  geom_point()

# Plots by biome (histograms of lm estimates of pop change)
## This doesn't work and I don't know what you're trying to do, is it similar to the histograms I've made below?
biome <- LPI_models_slopes %>%
  mutate(Year = parse_number(Year)) %>%
  filter(!is.na(pop)) %>%
  select(common_name, location_of_population, country_list, biome, year, pop, system, native, alien) %>%
  group_by(common_name, location_of_population, country_list, biome, system, native, alien) %>%
  filter(length(unique(Year)) > 2) %>%
  do(fit = lm(pop ~ Year, data = .)) %>%
  tidy(fit) %>%
  ungroup() %>%
  group_by(biome) %>%
  do(ggsave(ggplot(.,aes(x = estimate))+geom_histogram(),filename = gsub(" ","",paste("Biome_LPI/",unique(as.character(.$biome)),".pdf",sep="")),device="pdf"))
 
ggplot(LPI_models_slopes, aes(x=slope, fill=system)) + geom_density(alpha=.3)
ggplot(LPI_models_slopes, aes(x=slope, fill=biome)) + geom_density(alpha=.3)
ggplot(LPI_models_slopes, aes(x=slope, fill=realm)) + geom_density(alpha=.3)





# Gergana will add in plot with ggExtra and marginal histograms ----

