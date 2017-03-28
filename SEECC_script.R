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




#############################
### Download GBIF pufin data
#############################

# load the packages
library(rgbif)
library(sp)

# get the code for the UK
UK_code <- isocodes[grep("United Kingdom", isocodes$name), "code"]

# scientific name for puffin
species<-"Fratercula arctica"

# download all the occurrences of Fratercula arctica in the UK 
# between 2006 and 2016
# that have geographic coordinates and return dataset
occur<-occ_search(scientificName = species, country = UK_code, hasCoordinate = TRUE, limit=3000, year = '2006,2016', return = "data")
str(occur)

# map the occurrences
# run this to see region names for the world database layer
sort(unique(ggplot2::map_data("world")$region))

#map the data
gbifmap(occur, region="UK")

##########################
# Flickr data
##########################

# this script only manipulates the data
# I include a separate script that downloads it from Flickr

# load the package
library(lubridate)

# read the dataset
flickr<-read.table("./data/flickr_puffins.txt",header=T)
str(flickr)

# change the format of some of the variables
flickr$id<-as.character(flickr$id)
flickr$owner<-as.character(flickr$owner)
flickr$datetaken<-as.character(flickr$datetaken)
flickr$year<-as.factor(flickr$year)
flickr$month<-factor(flickr$month,levels=c("Jan","Feb","Mar","Apr","May",
                                                 "Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

flickr$date<-parse_date_time(as.character(flickr$dateonly),"ymd")  #transform character strings into time format

# Plotting the data to check it is accurate

#load required package
library(dismo)

geopics<- flickr[,c(4,5)]                    #subset coordinates only

coordinates(geopics)<-c("longitude","latitude") #make it spatial

plot(geopics)                                   #plot

# one point clearly not in the UK
# several points in the Channel Islands
# for simplicity we will delete these

# find the points with low latitude
which(flickr$latitude<49.9)

#and delete them from the dataset
flickr<-flickr[-which(flickr$latitude<49.9),]

# check that data is all in the UK
# using a nicer plot

coordinates(flickr)<-c("longitude","latitude") # go back to original dataframe and make it spatial

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(flickr) <- crs.geo                             # assign the coordinate system

# plot the data
plot(flickr, pch = 20, col = "steelblue")

# we can now add the UK coastline with the package rworldmap

# library rworldmap provides different types of global maps
library(rworldmap)

# and plot the UK's coastline
data(countriesLow)
plot(countriesLow, add = T)

# one more problem
# some puffin photos on land
# we need to remove those

# load the packages
library(rgdal)
library(rgeos)
library(raster)
library(maptools)

# read UK shapefile from GADM
UK_2<-getData("GADM", country="GB", level=0)

# change the coordinates system to UTM so everything is in meters
UK_proj2 <- spTransform(UK_2, CRS("+proj=utm +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
flickr_proj <- spTransform(flickr, CRS("+proj=utm +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))

# dissolve the polygons
UK_diss<-gUnaryUnion(UK_proj2)

# select only the points that fall into the UK polygon (terrestrial photos)
flickr_terr<-flickr_proj[which(is.na(over(flickr_proj, UK_diss, fn = NULL))==FALSE),]
plot(flickr_terr)

# select only the points that fall outside the UK polygon (marine photos)
flickr_mar<-flickr_proj[which(is.na(over(flickr_proj, UK_diss, fn = NULL))==TRUE),]
plot(flickr_mar)

# transform the UK polygon to line
UK_coast <- as(UK_diss, 'SpatialLines')
plot(UK_coast)

# calculate the distance of every point to the coastline 
# and select those that are within 1Km of the coastline
dist<-gWithinDistance(flickr_terr,UK_coast, dist = 1000, byid=T)
dist.df<-as.data.frame(dist)

str(dist.df)

#select only coastal points
flickr_coast<-flickr_terr[which(dist.df=="TRUE"),]
plot(flickr_coast)

# put coastal and marine points together
flickr_correct<-spRbind(flickr_mar,flickr_coast)

#check
plot(UK_coast)
points(flickr_correct, pch = 20, col = "steelblue")

#############################################
##Compare Flickr and GBIF with density maps
#############################################
library(ggplot2)

# transform data into readable format for ggplot2
UK.Df<-fortify(UK_diss,region="ID_0")

# plot Flickr data

# extract data and coordinates from dataset and fortify
flickr.points<-fortify(cbind(flickr_correct@data,flickr_correct@coords))

# plot the ponts on top of the UK shapefile
# then plot a density map created with stat_density2d

plot.years <- ggplot(data=flickr.points,aes(x=longitude, y=latitude))+            # plot the flickr data
              geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group),            # plot the UK
              color="black", fill="gray82") + coord_fixed() +                     # graphical parameters for the polygon + ensures that one unit on the x-axis is the same length as one unit on the y-axis
              geom_point(color="dodgerblue4",size=2,shape=".")+                   # graphical parameters for points
              stat_density2d(aes(x = longitude,                                   # create the density layer based on where the points are
              y = latitude,  fill = ..level.., alpha = ..level..),                # colour and transparency depend on density
              geom = "polygon", colour = "grey95",size=0.3) +                     # graphical parameters for the density layer
              scale_fill_gradient(low = "yellow", high = "red") +                 # set colour palette for density layer
              scale_alpha(range = c(.25, .5), guide = FALSE) +                    # set transparency for the density layer 
              facet_wrap(~ year)+                                                 # multipanel plot according to the variable "year" in the flickr dataset
              theme(axis.title.x=element_blank(), axis.text.x=element_blank(),    # don't display x and y axes labels, titles and tickmarks 
                    axis.ticks.x=element_blank(),axis.title.y=element_blank(), 
                    axis.text.y=element_blank(), axis.ticks.y=element_blank(),
                    text=element_text(size=18),legend.position = c(.9, .15))      # size of text and position of the legend

# now plot
# it takes a while!
plot.years


# plot gbif data

coordinates(occur)<-c("decimalLongitude","decimalLatitude") #make it spatial

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(occur) <- crs.geo                              # assign the coordinate system

# transform to UTM so they have the same coordinate system as flickr
occur_proj <- spTransform(occur, CRS("+proj=utm +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))

# transform into readable format for ggplot2
gbif.points<-fortify(cbind(occur_proj@data,occur_proj@coords))


# same as before
plot.years.gbif <- ggplot(data=gbif.points,aes(x=decimalLongitude, y=decimalLatitude))+
  geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
               color="black", fill="gray82") + coord_fixed() +
  geom_point(color="dodgerblue4",size=2,shape=".")+
  stat_density2d(aes(x = decimalLongitude, 
                     y = decimalLatitude,  fill = ..level.., alpha = ..level..), 
                 geom = "polygon", colour = "grey95",size=0.3) +
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_alpha(range = c(.25, .5), guide = FALSE) +
  facet_wrap(~ year)+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y=element_blank(), 
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        text=element_text(size=18),legend.position = c(.9, .15))

plot.years.gbif




