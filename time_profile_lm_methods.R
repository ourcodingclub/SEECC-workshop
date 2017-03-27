# Timing the model calls using different methods ----
# Run linear models of abundance trends over time for each population and extract model coefficients

## Using lapply() ----

### Create a list of data frames by splitting `LPI_long` by population (`genus_species_id`)
lapply_list <- system.time(LPI_long_list <- split(LPI_long, f = LPI_long$genus_species_id))

### lapply() a linear model (`lm`) to each data frame in the list and store as a list of linear models
lapply_lm <- system.time(LPI_list_lm <- lapply(LPI_long_list, function(x) lm(scalepop ~ year, data = x)))

### Extract model coefficients and store them in a data frame
lapply_extract <- system.time(LPI_models_lapply <- filter(data.frame(
  "genus_species_id" = names(LPI_list_lm),
  "n" = unlist(lapply(LPI_list_lm, function(x) df.residual(x))),
  "intercept" = unlist(lapply(LPI_list_lm, function(x) summary(x)$coeff[1])),
  "slope" = unlist(lapply(LPI_list_lm, function(x) summary(x)$coeff[2])),
  "intercept_se" = unlist(lapply(LPI_list_lm, function(x) summary(x)$coeff[3])),
  "slope_se" = unlist(lapply(LPI_list_lm, function(x) summary(x)$coeff[4])),
  "intercept_p" = unlist(lapply(LPI_list_lm, function(x) summary(x)$coeff[7])),
  "slope_p" = unlist(lapply(LPI_list_lm, function(x) summary(x)$coeff[8]))
), n > 5))

## Using a loop !!! This takes hours to run !!! ----

### Create data frame to store results
loop_df <- system.time(LPI_models_loop <- data.frame())

### Run the loop
loop_loop <- system.time(for(i in unique(LPI_long$genus_species_id)) {
  frm <- as.formula(paste("scalepop ~ year"))
  mylm <- lm(formula = frm, data = LPI_long[LPI_long$genus_species_id == i,])
  sum <- summary(mylm)
  
  # Extract mylmel coefficients
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
                   intercept_p = intercept_p, slope_p = slope_p, stringsAsFactors = F)
  
  # Bind rows of temporary data frame to the LPI_mylmels_loop data frame
  LPI_models_loop <- rbind(LPI_models_loop, df)
  
}
)
## Using a pipe ----
pipe_pipe <- system.time(LPI_models_pipes <- LPI_long %>%
  group_by(., genus_species_id) %>%  # Groups Measurement_type(Units)>population(id)>species(Common.Name+species)
  do(mod = lm(scalepop ~ year, data = .)) %>%  # Create a linear model for each group
  mutate(., n = df.residual(mod),  # Create columns: degrees of freedom
         intercept=summary(mod)$coeff[1],  # intercept coefficient
         slope=summary(mod)$coeff[2],  # slope coefficient
         intercept_se=summary(mod)$coeff[3],  # standard error of intercept
         slope_se=summary(mod)$coeff[4],  # standard error of slope
         intercept_p=summary(mod)$coeff[7],  # p value of intercept
         slope_p=summary(mod)$coeff[8]) %>%  # p value of slope
  filter(., n > 5) # Remove rows where degrees of freedom <5
)

# Create rows for each methods total time
lapply_time <- as.data.frame(t(as.vector(lapply_list + lapply_lm + lapply_extract)))
loop_time <- as.data.frame(t(as.vector(loop_df + loop_loop)))
pipe_time <- as.data.frame(t(as.vector(pipe_pipe)))

# Bind rows together
elapsed_time <- rbind(lapply_time, loop_time, pipe_time)

# Add column names
colnames(elapsed_time) <- names(lapply_list)

# Add column for methods name
elapsed_time$Method <- c("lapply", "loop", "pipe")

# Reorder columns for easy reading
elapsed_time <- elapsed_time[,c(6,3,1,2,4,5)]

elapsed_time



