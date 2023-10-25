#libraries
library(dplyr)
library(sf) #this is the spatial package
library(tmap) #this is the mapping
library(ggplot2)
library(tidyr)
library(usmap)
library(mapview)
library(raster)


#read in the dataset
#mbbs <- read.csv("proj/hurlbertlab/ijbgoulden/mbbs/analysis.df.csv", header = TRUE)
#...from the library(mbbs yeah...)
mbbs <- read.csv("data/analysis.df.csv", header = TRUE)

#okay, yeah I need to be on campus for this

#NLCD data are 2001, 2004, 2006, 2008, 2011, 2013, 2016, 2019, 2021

#prep the spatial dataset
mbbs_buffers <- read.csv("spatial/RouteStops.csv", header = TRUE) %>%
  dplyr::select(- c(notes)) 

#apparently can't pipe set crs transformations, alas!
mbbs_buffers <- st_as_sf(mbbs_buffers, coords = c('lon', 'lat'), crs = 4269) 
mbbs_buffers <- st_transform(mbbs_buffers, crs = 5070) #meters, NC 
mbbs_buffers <- st_buffer(mbbs_buffers, dist = 400) #buffer each point with 400m, now the geometry is all polygons 

#read in the NLCD
nc_nlcd_2001 <- raster("spatial/shapefiles/nc_nlcd_2001/nc_nlcd_2001.grd")

#put the buffers in the same projection as the nlcd
mbbs_buffers <- st_transform(mbbs_buffers, crs(nc_nlcd_2001))

#clip raster
buffers_nlcd <- raster::mask(nc_nlcd_2001, mbbs_buffers)
buffers_nlcd <- raster::crop(buffers_nlcd, mbbs_buffers)

plot(buffers_nlcd) #gorgeous C:

#save the buffers
writeRaster(buffers_nlcd, "spatial/shapefiles/buffers_nlcd_2001/buffers_nlcd_2001", format = "raster", overwrite = TRUE)
  
#clip raster - need something other than st join??? other option is to use extract like we did with the kestrel data
#nc_projectedforest <- st_join(x = projectedforest, y = mbbs_counties, join = st_within, left = FALSE)
 
#saved clipped nlcd information - however, do we also want to save the clipped nlcds themselves? how about we just get it working to get the %landcover of each time first, and then like, if we find out we need it we can do that. But we probably don't even need it for graphics