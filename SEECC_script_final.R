# Visualising species occurrence and quantifying population change
# John Godlee (johngodlee@gmail.com)
# Gergana Daskalova (gndaskalova@gmail.com)
# Isla Myers-Smith (isla.myers-smith@ed.ac.uk)

# Packages ---
library(readr)
library(tidyr)
library(dplyr)
library(broom)
library(ggplot2)
library(ggExtra)
library(maps)
library(RColorBrewer)

# Set working directory to source location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load data ----
load("LPIdata_Feb2016.RData")
load("puffin_GBIF.RData")

# Inspect data ----
View(head(LPIdata_Feb2016))

# Format data for analysis ----

# Transform from wide to long format
LPI_long <- gather(data = LPIdata_Feb2016, key = "year", value = "pop", select = 26:70)

# Get rid of the X in front of years
LPI_long$year <- parse_number(LPI_long$year)

# Rename variable names for consistency
names(LPI_long)
names(LPI_long) <- gsub(".", "_", names(LPI_long), fixed = TRUE)
names(LPI_long) <- tolower(names(LPI_long))
names(LPI_long)

# Create new column with genus and species together
LPI_long$genus_species_id <- paste(LPI_long$genus, LPI_long$species, LPI_long$id, sep = "_")

# Check data are displayed fine
View(LPI_long[c(1:5,500:505,1000:1005),])  
# You can use [] to subset data frames [rows, columns]
# If you want all rows/columns, add a comma in the row/column location

# Get rid of strange characters like " / "
LPI_long$country_list <- gsub(",", "", LPI_long$country_list, fixed = TRUE)
LPI_long$biome <- gsub("/", "", LPI_long$biome, fixed = TRUE)

# Examine the tidy data frame
View(head(LPI_long))

# Data manipulation ----

# Remove duplicate rows
LPI_long <- distinct(LPI_long)

# Remove missing / infinite data
LPI_long <- filter(LPI_long, is.finite(pop))

# Keep species with >5 years worth of data and calculate length of monitoring
LPI_long <- LPI_long %>%
  group_by(., genus_species_id) %>%  # group rows so that each group is one population
  mutate(., maxyear = max(year), minyear = min(year)) %>%  # Create columns for the first and most recent years that data was collected
  mutate(., lengthyear = maxyear-minyear) %>%  # Create a column for the length of time data available
  mutate(., scalepop = (pop-min(pop))/(max(pop)-min(pop))) %>%  # Scale population trend data
  filter(., is.finite(scalepop)) %>%
  filter(., lengthyear > 5) %>%  # Only keep rows with more than 5 years of data
  ungroup(.)  # Remove any groupings you've greated in the pipe, not entirely necessary but it's better to be safe

# Calculate summary statistics for each biome
LPI_biome_summ <- LPI_long %>%
  group_by(biome) %>%  # Group by biome
  summarise(populations = n(),   # Create columns, number of populations
            mean_study_length_years = mean(lengthyear),  # mean study length
            max_lat = max(decimal_latitude),  # max latitude
            min_lat = min(decimal_latitude),  # max longitude
            dominant_sampling_method = names(which.max(table(sampling_method))),  # modal sampling method
            dominant_units = names(which.max(table(units))))  # modal unit type

# Modelling population change over time ----

# Run linear models of abundance trends over time for each population and extract model coefficients

# Using lapply()

# Create a list of data frames by splitting `LPI_long` by population (`genus_species_id`)
LPI_long_list <- split(LPI_long, f = LPI_long$genus_species_id)

# lapply() a linear model (`lm`) to each data frame in the list and store as a list of linear models
LPI_list_lm <- lapply(LPI_long_list, function(x) lm(scalepop ~ year, data = x))

# Extract model coefficients and store them in a data frame
LPI_models_lapply <- filter(data.frame(
  "genus_species_id" = names(LPI_list_lm),
  "n" = unlist(lapply(LPI_list_lm, function(x) df.residual(x))),
  "intercept" = unlist(lapply(LPI_list_lm, function(x) summary(x)$coeff[1])),
  "slope" = unlist(lapply(LPI_list_lm, function(x) summary(x)$coeff[2])),
  "intercept_se" = unlist(lapply(LPI_list_lm, function(x) summary(x)$coeff[3])),
  "slope_se" = unlist(lapply(LPI_list_lm, function(x) summary(x)$coeff[4])),
  "intercept_p" = unlist(lapply(LPI_list_lm, function(x) summary(x)$coeff[7])),
  "slope_p" = unlist(lapply(LPI_list_lm, function(x) summary(x)$coeff[8])),
  "lengthyear" = unlist(lapply(LPI_long_list, function(x) max((x)$lengthyear)))
), n > 5)

# Using a loop !!! This can take a long time to run depending on your laptop !!!

# Create a data frame to store results
LPI_models_loop <- data.frame()

for(i in unique(LPI_long$genus_species_id)) {
  frm <- as.formula(paste("scalepop ~ year"))
  mylm <- lm(formula = frm, data = LPI_long[LPI_long$genus_species_id == i,])
  sum <- summary(mylm)
  
  # Extract model coefficients
  n <- df.residual(mylm)
  intercept <- summary(mylm)$coeff[1]
  slope <- summary(mylm)$coeff[2]
  intercept_se <- summary(mylm)$coeff[3]
  slope_se <- summary(mylm)$coeff[4]
  intercept_p <- summary(mylm)$coeff[7]
  slope_p <- summary(mylm)$coeff[8]
  
  # Create temporary data frame
  df <- data.frame(genus_species_id = i, n = n, intercept = intercept, 
                   slope = slope, intercept_se = intercept_se, slope_se = slope_se,
                   intercept_p = intercept_p, slope_p = slope_p, 
                   lengthyear = LPI_long[LPI_long$genus_species_id == i,]$lengthyear, stringsAsFactors = F)
  
  # Bind rows of temporary data frame to the LPI_mylmels_loop data frame
  LPI_models_loop <- rbind(LPI_models_loop, df)
  
}

# Remove duplicate rows and rows where degrees of freedom <5
LPI_models_loop <- distinct(LPI_models_loop)
LPI_models_loop <- filter(LPI_models_loop, n > 5)

# Using a pipe
LPI_models_pipes <- LPI_long %>%
  group_by(., genus_species_id, lengthyear) %>% 
  do(mod = lm(scalepop ~ year, data = .)) %>%  # Create a linear model for each group
  mutate(., n = df.residual(mod),  # Create columns: degrees of freedom
         intercept = summary(mod)$coeff[1],  # intercept coefficient
         slope = summary(mod)$coeff[2],  # slope coefficient
         intercept_se = summary(mod)$coeff[3],  # standard error of intercept
         slope_se = summary(mod)$coeff[4],  # standard error of slope
         intercept_p = summary(mod)$coeff[7],  # p value of intercept
         slope_p = summary(mod)$coeff[8]) %>%  # p value of slope
  ungroup() %>%
  mutate(., lengthyear = lengthyear) %>%
  filter(., n > 5) # Remove rows where degrees of freedom <5


# Saving data frame as RData file
save(LPI_models_pipes, file = "LPI_models_pipes.RData")

# Visualising model outputs ----

# Setting a custom ggplot2 function
theme_LPI <- function(){
  theme_bw()+
    theme(axis.text.x=element_text(size=12, angle=45, vjust=1, hjust=1),
          axis.text.y=element_text(size=12),
          axis.title.x=element_text(size=14, face="plain"),             
          axis.title.y=element_text(size=14, face="plain"),             
          panel.grid.major.x=element_blank(),                                          
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size=20, vjust=1, hjust=0.5),
          legend.text = element_text(size=12, face="italic"),          
          legend.title = element_blank(),                              
          legend.position=c(0.9, 0.9))
}

# Making histograms of slope estimates for each biome
biome.plots <- LPI_long %>%
  group_by(., genus_species_id, biome) %>% 
  do(mod = lm(scalepop ~ year, data = .)) %>% 
  tidy(mod) %>%
  ungroup() %>%
  group_by(., biome) %>%
  do(ggsave(ggplot(.,aes(x = estimate)) + geom_histogram(colour="#8B5A00", fill="#CD8500") + theme_LPI(), 
            filename = gsub("", "", paste("Biome_LPI/", unique(as.character(.$biome)), ".pdf", sep="")), device="pdf"))

# Plotting slope estimates and standard errors for all populations and adding histograms along the margins
p1 <- (all_slopes <- ggplot(LPI_models_lapply, aes(x = lengthyear, y = slope)) +
         geom_point() +
         geom_pointrange(aes(ymin = slope - slope_se, ymax = slope + slope_se)) +
         geom_hline(yintercept = 0, linetype = "dashed") +
         theme_LPI() +
         ylab("Population change\n") +
         xlab("\nDuration (years)"))

ggExtra::ggMarginal(
  p = p1,
  type = 'histogram',
  margins = 'both',
  size = 5,
  col = 'black',
  fill = 'gray'
)

# Visualising species occurrence ----
# Atlantic puffin as an example

# Use borders() to pull some world map data from the maps package
map_world <- borders(database = "world", colour = "gray50", fill = "#383838")

ggplot() + map_world +  # Plot the map
  geom_point(data = puffin_GBIF,  # Specify the data for geom_point()
             aes(x = decimallongitude,  # Specify the x axis as longitude
                 y = decimallatitude,  # Specify the y axis as latitude
                 colour = scientificname),  # Colour the points based on species name
             alpha = 0.4,  # Set point opacity to 40%
             size = 1) +  # Set point size to 1
  scale_color_brewer(palette = "Set1") +   # Specify the colour palette to colour the points
  theme_classic() +  # Remove gridlines and shading inside the plot
  ylab(expression("Latitude ("*degree*")" )) +  # Add a smarter x axis label
  xlab(expression("Longitude ("*degree*")" )) +  # Add a smarter y axis label
  theme(legend.position = "bottom",  # Move the legend to below the plot
        legend.title = element_blank())  # Remove the legend title


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
flickr<-read.table("./flickr_puffins.txt",header=T)
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
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),  # don't display x and y axes labels, titles and tickmarks 
        axis.ticks.x=element_blank(),axis.title.y=element_blank(),   
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        text=element_text(size=18),legend.position = c(.9, .15),       # size of text and position of the legend
        panel.grid.major = element_blank(),                            # eliminates grid lines from background
        panel.background = element_blank())                            # set white background

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
        text=element_text(size=18),legend.position = c(.9, .15),       
        panel.grid.major = element_blank(),                            
        panel.background = element_blank())                            

plot.years.gbif



