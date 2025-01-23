########################
# Traits: Calculate Climate Position
# Take list of species
# Load in range files
# Load in worldclim 2.1 BIO 10 minute data
# For each species breeding distribution polygon,
#     calculate mean and standard deviation for
#     mean wamest quarter temp and precipitation
# Assume those are basically normal distributions,
# Take the mean climate values for the study area
# Z-score to meausre how close to median conditions
# of the species climate niche the study area is.
# Multiply the z-scores for mean_wq_temp and precip
# this is the final 'climate position' score.
########################

# libraries
library(dplyr)
##library(terra)
library(sf) #for vectors
library(stars) #works with sf but for vectors

# species range maps directory
  range_dir <- "Z:/GIS/birds/All/All/"

# output directory
  output_dir <- "data/species-traits/"

# load in species list
  species_list <- read.csv("data/species-traits/species_list_match_range_files.csv")

# read in the climate data
  worldclim_precipwarmq <- terra::rast("data/species-traits/worldclim2.1_10m_bio/wc2.1_10m_bio_18.tif") %>% #BIO18 = Precipitation of Warmest Quarter
    terra::crop(terra::ext(-150,-50,15,60))
  
  worldclim_meantempwarmq <- terra::rast("data/species-traits/worldclim2.1_10m_bio/wc2.1_10m_bio_10.tif") %>% #BIO10 = Mean Temperature of Warmest Quarter
  terra::crop(terra::ext(-150,-50,15,60))
  
# create empty new columns to add data to on our species_list
  species_list <- species_list %>%
    mutate(mean_tempwq = 999,
           sd_tempwq = 999,
           mean_precipwq = 999,
           sd_precipwq = 999)
  
#####testing env.
  i = 1
  ####some error at i = 30
for(i in 1:nrow(species_list)) {
  # Using package sf (easy to read, but doesn't plot well)
  breeding_range <- sf::read_sf(paste0(
    range_dir,
    species_list$file[i]
  )) %>%
    # Re-project to the worldclim crs
    sf::st_transform(sf::st_crs(worldclim_meantempwarmq)) %>%
    #  Use extant (PRESENCE 1-3) breeding and resident ranges (SEASONAL 1-2) only
    filter(PRESENCE %in% c(1:3),
           SEASONAL %in% c(1:2))
  
  # convert to terra for it's clipping functions + plotting ease
  breeding_range_t <- vect(breeding_range)
  #CONFIRM nothing got lost in the switch
  assertthat::assert_that(
    nrow(breeding_range) == nrow(as.data.frame(breeding_range_t))
  )
  #now can easily plot(breeding_range_t) to see if it looks correct

  # cut the climate variables down to size
  clip_meantempwarmq <- terra::mask(worldclim_meantempwarmq, breeding_range_t)
  clip_precipwarmq <- terra::mask(worldclim_precipwarmq, breeding_range_t)
  #note: you can check these are the correct shape by plotting them
  
  # extract the data
  data_meantempwarmq <- terra::extract(clip_meantempwarmq, breeding_range_t)
  data_precipwarmq <- terra::extract(clip_precipwarmq, breeding_range_t)
  
  # summarize the data
  data_meantempwarmq <- data_meantempwarmq %>%
    summarize(mean_tempwq = mean(wc2.1_10m_bio_10, na.rm = TRUE),
              sd_tempwq = sd(wc2.1_10m_bio_10, na.rm = TRUE))
  data_precipwarmq <- data_precipwarmq %>%
    summarize(mean_precipwq = mean(wc2.1_10m_bio_18, na.rm = TRUE),
              sd_precipwq = sd(wc2.1_10m_bio_18, na.rm = TRUE))
  
  # add the results back to the species list
  temp_row <- species_list[i,] %>%
    mutate(data_meantempwarmq) %>%
    mutate(data_precipwarmq)
  
  species_list <- species_list %>%
    dplyr::rows_update(temp_row, by = "common_name")
}
#for each species:
#read in the breeding range
#clip climate variables to extent of breeding range
#get mean and std of each climate variable
    #sidebar: I think unnessesary to z-transform these variables first. because what I finally pull out is going to be a z-score. But I think it's the exact same to take a z-score from raw data as from z-transformed data. and that way the numbers remain interpretable to me throughout. 
#add info to new columns in the species_list
#save the species_list

#then: get the climate info for chapel hill
#z-score climate info for chapel hill for each species
#multiply together
