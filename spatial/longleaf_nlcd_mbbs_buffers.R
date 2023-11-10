#libraries
library(dplyr)
library(sf) #this is the spatial package
library(ggplot2)
library(tidyr)
library(terra)

#paths
path <- "/proj/hurlbertlab/ijbgoulden/"
RouteStopcsv <- paste(path, "csv/RouteStops.csv", sep = "")
nlcdclassificationscsv <- paste(path, "csv/nlcd_classifications.csv", sep="")
nlcdpath <- "nc_nlcd/nc_nlcd_"
nlcdfileextension <- ".tif"

#NLCD data are 2001, 2004, 2006, 2008, 2011, 2013, 2016, 2019, 2021
year <- c("2001", "2004", "2006", "2008", "2011", "2013", "2016", "2019", "2021")

#load the spatial dataset (comes from mbbs-analysis, https://github.com/hurlbertlab/mbbs-analysis)
mbbs_buffers <- read.csv(RouteStopcsv, header = TRUE) %>%
  dplyr::select(- c(notes)) #remove notes column

#create 400m buffers from route stop points
#convert lat/lon to points geometry
mbbs_buffers <- st_as_sf(mbbs_buffers, coords = c('lon', 'lat'), crs = 4269) 
#transform to a meters based crs
mbbs_buffers <- st_transform(mbbs_buffers, crs = 5070) #meters, NC 
#buffer each point by 400m, now geometry is polygons
mbbs_buffers <- st_buffer(mbbs_buffers, dist = 400) 

#read in the NLCD files into a stack
stackpath <- paste(path, "/nc_nlcd/", sep = "")
rastlist <- list.files(path = stackpath, pattern = '.tif$', all.files=TRUE, full.name=TRUE)
nlcdstack <- rast(rastlist)
nlcdstack

#load in the NLCD legend, colors, and descriptions
classes <- read.csv(nlcdclassificationscsv, header = TRUE)

#put the buffers in the same projection as the nlcd
mbbs_buffers <- st_transform(mbbs_buffers, crs(nlcdstack))

#clip rasters
bufferstack <- mask(nlcdstack, mbbs_buffers)
bufferstack <- crop(bufferstack, mbbs_buffers)

print("nc clipped to buffers")

#save rasters
for(i in 1:9) {
  fname <- paste("/proj/hurlbertlab/ijbgoulden/buffer_nlcd_",year[i],sep="")
  writeRaster(bufferstack[[i]], fname, filetype = "GTiff", overwrite = TRUE) 
}

print('buffers saved')

extractedbuffers <- extract(x = bufferstack, y = mbbs_buffers, df = TRUE)

print("buffers extracted")

write.csv(extractedbuffers_terra, "/proj/hurlbertlab/ijbgoulden/extractedbuffers.csv")

print("buffers saved")