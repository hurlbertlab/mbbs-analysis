##working out how to work with landfire data
library(dplyr)
library(sf) #this is the spatial package
library(terra)

evh_2001 <- rast("scratch/landfire_durham61/US_140_EVH/us_140evh.tif")

plot(evh_2001)

#load the spatial dataset (comes from mbbs-analysis, https://github.com/hurlbertlab/mbbs-analysis)
#Just durham 1 and 6 for now
mbbs_buffers <- read.csv("C:/git/mbbs-analysis/spatial/RouteStops.csv", header = TRUE) %>%
  dplyr::select(- c(notes)) %>% #remove notes column 
  filter(mbbs_county == "Durham") %>%
  filter(route_num %in% c(1,6))

#make 400m buffers
mbbs_buffers <- st_as_sf(mbbs_buffers, coords = c('lon', 'lat'), crs = 4269)
#transform to a meters based crs
mbbs_buffers <- st_transform(mbbs_buffers, crs = 5070) #meters, NC 
#buffer each point by 400m, now geometry is polygons
mbbs_buffers <- st_buffer(mbbs_buffers, dist = 400) 

#put the buffers in the same projection as the landfire data
mbbs_buffers <- st_transform(mbbs_buffers, crs(evh_2001))

#clip rasters
clip_evh <- mask(evh_2001, mbbs_buffers)
clip_evh <- crop(clip_evh, mbbs_buffers)

plot(clip_evh)
#durham 6 has low heights, this makes sense as it's a lot of industry
#durham 1 has much higher heights, makes sense as it's got a lot of forest. I don't think the numbers translate 1:1 though. 
#https://landfire.gov/documents/LF_Data_Dictionary.pdf
#numbers 100+ are the tree heights in meteres. The 60's represent different agricultural categories. 
#highest pixel value in this rastert is 111: tree height of 11 meters / 36 ft hight. Huh! Not very tall!

#let's read in another year
evh_2023 <- rast("scratch/landfire_durham61/LF2022_EVH_230_CONUS/LC22_EVH_230.tif")

#clip 2023
clip_evh_2023 <- mask(evh_2023, mbbs_buffers)
clip_evh_2023 <- crop(clip_evh_2023, mbbs_buffers)

plot(clip_evh_2023)
#okay, heights are actually labeled on these pixels - that's nice! Lots of taller trees, and shrub categories are represented
#there are differences in the data structure between these years for sure. 

#to do any work with filtering or summarizing etc, then we have to extract the buffers and work with the data from there. 
extracted <-  extract(x = clip_evh, y = mbbs_buffers, df = TRUE)
#ID 1:40 is the 40 different buffers from mbbs_buffers, in the listed order.
#we can do
extracted <- extracted 
