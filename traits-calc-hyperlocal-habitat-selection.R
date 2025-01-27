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

study_boundaries <- terra::ext(-80.55, -77.74, 34.57, 37.24)

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
    select(county, route, route_num, stop_num) %>%
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
mbbs_sl <- read.csv("data/mbbs_stops_counts.csv") #sl for stop level
