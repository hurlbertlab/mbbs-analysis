#libraries
library(dplyr)
library(sf) #this is the spatial package
library(ggplot2)
library(tidyr)
library(raster)

#paths
path <- "/proj/hurlbertlab/ijbgoulden/"
RouteStopcsv <- paste(path, "csv/RouteStops.csv", sep = "")
nlcdclassificationscsv <- paste(path, "csv/nlcd_classifications.csv", sep="")
nlcdpath <- "nc_nlcd/nc_nlcd_"
nlcdfileextension <- ".grd"

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
rastlist <- list.files(path = stackpath, pattern = '.grd$', all.files=TRUE, full.name=TRUE)
nlcdstack <- stack(rastlist)
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
  writeRaster(bufferstack[[i]], paste("/proj/hurlbertlab/ijbgoulden/buffer_nlcd_",year,sep=""), format = "GTiff", overwrite = TRUE) 
}

print('buffers saved')

extractedbuffers <- extract(x = bufferstack, y = mbbs_buffers, df = TRUE)

print("buffers extracted")

write.csv(extractedbuffers, "/proj/hurlbertlab/ijbgoulden/extractedbuffers.csv")

print("buffers saved")



filename <- paste(path, nlcdpath, year[1], nlcdfileextension, sep = "")

nc_nlcd <- raster(filename)



#clip raster
buffers_nlcd <- mask(nc_nlcd, mbbs_buffers)
buffers_nlcd <- crop(buffers_nlcd, mbbs_buffers)

#check the clipped raster looks how it should
pdffilename <- paste(path, "check_nlcdbuffers.pdf",sep="")
pdf(file = pdffilename,
    width = 10, # The width of the plot in inches
    height = 10) # The height of the plot in inches
plot(buffers_nlcd)
dev.off()

#and from here, if this is working ok we'll follow the tutorial we found







nc_nlcd_2001 <- raster("spatial/shapefiles/nc_nlcd_2001/nc_nlcd_2001.grd")

#put the buffers in the same projection as the nlcd
mbbs_buffers <- st_transform(mbbs_buffers, crs(nc_nlcd_2001))

#clip rasters
buffers_nlcd <- raster::mask(nc_nlcd_2001, mbbs_buffers)
buffers_nlcd <- raster::crop(buffers_nlcd, mbbs_buffers)

plot(buffers_nlcd) #gorgeous C:

#save the buffers
#writeRaster(buffers_nlcd, "spatial/shapefiles/buffers_nlcd_2001/buffers_nlcd_2001", format = "raster", overwrite = TRUE)

buffers_nlcd <- raster("spatial/shapefiles/buffers_nlcd_2001/buffers_nlcd_2001.grd")

extractedbuffers <- raster::extract(x= buffers_nlcd, y = mbbs_buffers, df = TRUE)

lt_bybuff <-as.data.frame(table(extractedbuffers))
#ID is going to be each buffer....not certain which order they're in. And then within that there's the information of landtype and it's frequency
#yeah, but there's something wrong because there should be like 20 nlcd landcover types. Totally possible I got longleaf working but using the wrong 2001 dataset option..hm! Yeah, we need access to longleaf and let's update the nlcd listings available
  
#clip raster - need something other than st join??? other option is to use extract like we did with the kestrel data
#nc_projectedforest <- st_join(x = projectedforest, y = mbbs_counties, join = st_within, left = FALSE)
 
#saved clipped nlcd information - however, do we also want to save the clipped nlcds themselves? how about we just get it working to get the %landcover of each time first, and then like, if we find out we need it we can do that. But we probably don't even need it for graphics