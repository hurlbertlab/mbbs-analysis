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
#     on in the last four years (2024/2023/2022/2021)
# This mean represents their local habitat selection metric
########################

#libraries
library(dplyr)
library(terra)
library(sf)

study_boundaries <- terra::ext(-80.55, -77.74, 34.57, 37.24)

#Files located on the lab Z: drive
#Downloaded from AppEEARS, requested MODIS monthly data from 2012 May-July, with a polygon covering most of North Carolina
ndvi <- terra::rast(c(
  "Z:/Goulden/mbbs-analysis/MOD13A3.061_1_km_monthly_NDVI_2012MAY_MBBS.tif", 
  "Z:/Goulden/mbbs-analysis/MOD13A3.061_1_km_monthly_NDVI_2012JUNE_MBBS.tif",
  "Z:/Goulden/mbbs-analysis/MOD13A3.061_1_km_monthly_NDVI_2012JULY_MBBS.tif")) %>%
  project("epsg:4326")

terra::plot(ndvi)
#great, looks like expected, let's crop

study_area <- terra::crop(ndvi, study_boundaries)
terra::plot(study_area) #some areas are white - that's okay, these are out of bounds. All the stops wind up with data at the end.

#mini bbs stops
  mbbs_buffers <- read.csv("spatial/route_stop_coordinates.csv", header = TRUE) %>%
    dplyr::select(-stop_notes) #remove notes column
  #create 400m buffers from route stop points
  #convert lat/lon to points geometry
  mbbs_buffers <- st_as_sf(mbbs_buffers, coords = c('lon', 'lat'), crs = 4269) 
  #transform to a meters based crs
  mbbs_buffers <- st_transform(mbbs_buffers, crs = 5070) #meters, NC 
  #buffer each point by 400m, now geometry is polygons
  mbbs_buffers <- st_buffer(mbbs_buffers, dist = 400) 
  #change crs
  mbbs_buffers <- st_transform(mbbs_buffers, crs(study_area))
  
  buffer_ID <- mbbs_buffers %>%
    as.data.frame() %>%
    dplyr::select(county, route, route_num, stop_num) %>%
    group_by(route, stop_num) %>%
    mutate(ID = cur_group_id())
    
  
#want to trim to just the parts of the raster than intersect with each stop, then extract
  data_ndvi <- terra::extract(study_area, mbbs_buffers) %>%
    group_by(ID) %>%
    summarize(may_ndvi = mean(MOD13A3.061_1_km_monthly_NDVI_2012MAY_MBBS, na.rm = TRUE),
              june_ndvi = mean(MOD13A3.061_1_km_monthly_NDVI_2012JUNE_MBBS, na.rm = TRUE),
              july_ndvi = mean(MOD13A3.061_1_km_monthly_NDVI_2012JULY_MBBS, na.rm = TRUE),
              mean_ndvi = mean(c(may_ndvi, june_ndvi, july_ndvi), na.rm = TRUE)) %>%
    left_join(buffer_ID, by = "ID")
      
#now, we want a record of every stop each bird species on the species list was seen on in 2023/2024. 
  #stop_level_mbbs freshly pulled from the website
species_list <- read.csv("data/species-traits/species_list.csv") %>%
  dplyr::select(common_name)
mbbs_sl <- read.csv("data/mbbs/mbbs_stops_counts.csv") %>% #sl for stop level 
  filter(common_name %in% species_list$common_name) %>% 
  #for rn at least only want the last three years of habitat selection data
  filter(year >= 2021) %>% 
  #filter out where the species count was 0
  filter(count > 0) %>%
  #left_join the information about the buffers so we can move to summarizing
  left_join(data_ndvi, by = c("route", "stop_num")) %>%
  group_by(common_name) %>%
  summarize(habitat_selection = mean(mean_ndvi),
            habitat_sd = sd(mean_ndvi),
            habitat_ndvi_min = min(mean_ndvi),
            habitat_ndvi_max = max(mean_ndvi)) #hehe a lot of the habitat maxes are the same, like a lot of the birds are seen all at the same location with great habitat, makes a lot of sense. European Starling and House Sparrow have the lowest max ndvi :)
  #lol, even though there's a wide range of habitat ndvis, totally makes sense these birds are all selecting for the higher end of the ndvi range. Not a ton of variation btwn ndvis.
  #one note, stops that the species was recorded on twice are given more weight in this mean. To remove that, would add a layer of unique(common_name, route, stop_num) and get rid of the year aspect.

  #done!
  write.csv(mbbs_sl, "data/species-traits/ndvi_habitat_selection.csv", row.names = FALSE)
  
  #for the species we have grace's habitat selection metric on, what's the degree of correlation?
  dicecco_habitat_niche <- read.csv("data/species-traits/gdicecco-avian-range-shifts/habitat_niche_ssi_true_zeroes.csv") %>%
    filter(english_common_name %in% mbbs_sl$common_name) %>%
    left_join(mbbs_sl, by = c("english_common_name" = "common_name"))
  
  #calculate corrleation
  cor(dicecco_habitat_niche$ssi, dicecco_habitat_niche$habitat_selection)
  #Cor of -0.072. Okay cool! Not very correlated. 
  #Do keep in mind...we've only used a dummy variable for habitat niche in the data so far. It may be that Grace's habitat niche IS predictive.
  