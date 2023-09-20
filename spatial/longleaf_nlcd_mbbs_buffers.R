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
mbbs <- read.csv("proj/hurlbertlab/ijbgoulden/mbbs/analysis.df.csv", header = TRUE)

#okay, yeah I need to be on campus for this

#NLCD data are 2001, 2004, 2006, 2008, 2011, 2013, 2016, 2019, 2021

#prep the spatial dataset
mbbs_buffers <- read.csv("spatial/RouteStops.csv", header = TRUE) %>%
  dplyr::select(-notes) %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4269) %>%
  st_transform(crs = 5070) %>% #meters, NC 
  st_buffer(dist = 400) #buffer each point with 400m, now the geometry is all polygons

#put the data in the same projection as the nlcd
#spatial.df <-st_transform(crs = ???)
  
#clip raster - need something other than st join??? other option is to use extract like we did with the kestrel data
nc_projectedforest <- st_join(x = projectedforest, y = mbbs_counties, join = st_within, left = FALSE)
 
#saved clipped nlcd information - however, do we also want to save the clipped nlcds themselves? how about we just get it working to get the %landcover of each time first, and then like, if we find out we need it we can do that. But we probably don't even need it for graphics