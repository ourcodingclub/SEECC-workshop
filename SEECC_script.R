# SEECC LPI Data 
# John Godlee (johngodlee@gmail.com)

# Packages

# Set working directory to source location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load data
load("LPIUKall.RData")
load("LPIdata_Feb2016.RData")

length(unique(LPIUKall$species))
length(unique(LPIdata_Feb2016$`Country list`))

# Remove whitespace in variable names
names(LPIdata_Feb2016) <- gsub(" ", "_", names(LPIdata_Feb2016), fixed = TRUE)
