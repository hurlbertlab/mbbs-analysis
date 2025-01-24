########################
# Traits: Calculate Local Habitat Selection with NDVI/DVI
# Take an ndvi map from like 2012/2015
# Use monthly data, either June or average May-June-July
# Load in the mbbs stop lat/lon
# and buffer to 400m,
# extract ndvi values
# Take a list of species
# for each species, get their mean ndvi + sd
# (the habitat selection metric, 
#     captures secondary v. primary forest difs)
# from every stop w/in the mbbs that the species has been observed
#     on in the last year or two (2024/2023)
# This mean represents their local habitat selection metric
########################

#libraries
library(dplyr)
library(terra)
library(sf)

study_boundaries <- terra::ext(-79.55, -78.74, 35.57, 36.24)

ndvi <- terra::rast("Z:/GIS/MODIS/NDVI/MOD13A2_M_NDVI_2012-06.FLOAT.TIFF") %>%
  terra::rectify(aoi = study_boundaries)


