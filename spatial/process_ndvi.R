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
mbbs_buffers <- st_transform(mbbs_buffers, crs = crs(ndvi_2001))

#for each buffer (1-640, extract the average NDVI value)
#.....1-640... let's see how long doing one takes but this may need to be run on longleaf. cause is 640. for one image.

#fantastic - don't even have to put it in a for loop. Maintains ID, and terra can do all the extracts for all 680 buffers in about 5 seconds
#example:
test <- mbbs_buffers # %>% filter(County_Route_Stop == "Chatham-1-1")

#turn the geometry into a spatvector
testvect <- vect(test$geometry)

testcrop <- crop(ndvi_2001, testvect)

testextract <- extract(ndvi_2001, testvect)

plot(testvect) #yep
plot(ndvi_2001, col = 'red') #yep....wait no. That's WAY fewer points than I expect, and each of them is like a single pixel, rather than representative of all the pixels in the data. 
plot(testcrop)

finex <- testextract %>% group_by(ID) %>% mutate(ave = mean(NDVI, na.rm = TRUE))
hist(finex$ave)
