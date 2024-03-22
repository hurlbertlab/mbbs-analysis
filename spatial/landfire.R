#extract and save landfire data

library(dplyr)
library(sf) #this is the spatial package
library(terra)

#read in the longleaf files into a stack
path <- "spatial/shapefiles/landfire/"
rastlist <- list.files(path = path, pattern = '.tif$', all.files=TRUE, full.name=TRUE)
stack <- rast(rastlist)

#load the spatial dataset 
mbbs_buffers <- read.csv("spatial/Routestops.csv", header = TRUE) %>%
  dplyr::select(- c(notes)) %>% #remove notes column
  mutate(ID = row_number()) %>%
  relocate(ID, .before = County_Route_Stop)

#make 400m buffers
mbbs_buffers <- st_as_sf(mbbs_buffers, coords = c('lon', 'lat'), crs = 4269)
#transform to a meters based crs
mbbs_buffers <- st_transform(mbbs_buffers, crs = 5070) #meters, NC 
#buffer each point by 400m, now geometry is polygons
mbbs_buffers <- st_buffer(mbbs_buffers, dist = 400) 

#put the buffers in the same projection as the landfire data
mbbs_buffers <- st_transform(mbbs_buffers, crs(stack))

#clip rasters
bufferstack <- mask(stack, mbbs_buffers)
bufferstack <- crop(stack, mbbs_buffers)

#extract data
extracted <-  extract(x = bufferstack, y = mbbs_buffers, df = TRUE)

#add back in the route information
landfire_extracted <- left_join(extracted, mbbs_buffers, by = "ID")
#add in landfire classification information
landfire_extracted <- left_join()

#write csv of extracted data
write.csv(extractedbuffers_terra, "/proj/hurlbertlab/ijbgoulden/extractedbuffers.csv")