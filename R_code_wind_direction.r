

# Wind direction


###############
# 1. Packages #
###############

# First, create a list of required packages
list.of.packages <- c("raster", "tidyverse", "gridExtra", "stargazer", "lubridate", "ggpubr", "gganimate", "patchwork", "gifski")

# install required packages, if necessary, and load them
{
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  
  if(length(new.packages)) install.packages(new.packages)
  
  lapply(list.of.packages, require, character.only = TRUE)
}

##################
# 2. Data import #
##################

setwd("C:/Users/fedet/OneDrive/Documenti/R/asinara")

#-----------------------------------------------------------------------------------------#

dat_raw <- brick("wind_direction/asinara_climate_copernicus.grib")

dat_raw_grib <- ReadGrib("wind_direction/asinara_climate_copernicus.grib")

plot(dat_raw)


