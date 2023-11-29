#process NDVI
#this script...
#takes a TIF of NDVI 
#loads in route_stop/buffer information
#for each buffer extracts the average NDVI value
#and does that in a for loop so you can feed in any NDVI tif and run it multiple times if you want.

library(dplyr)
library(terra)
library(sf)

#load in NDVI images
ndvi_2001 <- rast("spatial/ndvi/landsat5_2001_forest_ndvi.tif")


#load in RouteStop
mbbs_buffers <- read.csv("spatial/RouteStops.csv", header = TRUE) %>%
  dplyr::select(- c(notes)) #remove notes column
#re-create buffers geometry
#create 400m buffers from route stop points
#convert lat/lon to points geometry
mbbs_buffers <- st_as_sf(mbbs_buffers, coords = c('lon', 'lat'), crs = 4269) 
#transform to a meters based crs
mbbs_buffers <- st_transform(mbbs_buffers, crs = 5070) #meters, NC 
#buffer each point by 400m, now geometry is polygons
mbbs_buffers <- st_buffer(mbbs_buffers, dist = 400) 
#change projection back
mbbs_buffers <- st_transform(mbbs_buffers, crs = 4269)

#for each buffer (1-640, extract the average NDVI value)
#.....1-640... let's see how long doing one takes but this may need to be run on longleaf. cause is 640. for one image.
