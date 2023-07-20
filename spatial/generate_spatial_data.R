#---
#Generate a spatial dataset
#GOAL: create a lat/long for each stop/route (group_by) and map those points in the triangle area
#GOAL: create a buffer around each of those points
#GOAL: add in NLCD landcover data (excellent if met this week, but might not)
#---

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
mbbs <- read.csv("data/analysis.df.csv", header = TRUE)

  #generate a new spatial dataframe
  #spatial.df <- remake.spatial(mbbs)
  #or read in the spatial dataframe already in use
  spatial.df <- read.csv("spatial/RouteStops.csv", header = TRUE) %>%
    add.geometry() #and add geometry for mapping

  #check for problems in the dataframe (all columns in the table should be 1s for the county/route combos that exist)
  table(spatial.df$mbbs_county, spatial.df$stop_num, spatial.df$route_num)
  
  #make a 400m buffer
  stop.buffer <- st_buffer(spatial.df, dist = 400) 
  
  #take a look!
  #mapview(stop.buffer) +
  #  mapview(spatial.df)
  

  forest <- raster("Z:/GIS/LandCoverData/forest_3km.tif")
  
  indigolocal <- raster("Z:/GIS/LandCoverData/nlcd_local") #whole folder
  mapview(indigolocal) +
    mapview(mbbs_counties)
  
  plot(projectedforest)
  projectedforest <- projectRaster(forest, crs = 5070)
  
  #clip raster - need something other than st join
  nc_projectedforest <- st_join(x = projectedforest, y = mbbs_counties, join = st_within, left = FALSE)
  
  
  

#' Create a spatial dataset of lat/lon with the latest point given to every stop
#'
#' @param analysis.df an mbbs dataframe-
#' @return a `spatial.df` with the lat/lon of every county/route/stop combination
#' @importFrom tidyr group_by expand drop_na filter ungroup slice_max "%>%"
#to remake the dataset with the latest point given to every stop
remake.spatial <- function(analysis.df) {
  
  #generate dataset of coordinates broken out by year
  spatial.df <- analysis.df %>% 
                  group_by(mbbs_county, route_num, stop_num, year) %>% 
                  expand(nesting(lat,lon)) %>% #tidyr::expand() gives all combinations of values that appear in the data
                  drop_na(stop_num) %>% #remove stop_nums that are NA (older data)
                  filter(stop_num > 0) %>% #or where stop_num is 0 (precount) 
                  ungroup() %>% group_by(mbbs_county,route_num,stop_num) %>% #ungroup and regroup
                  slice_max(year) %>% #pick the row from each county/route/stop from the latest yr
                  ungroup() #always ungroup at end
    
  return(spatial.df)
}

#function to convert a spatial csv to like, a useable map thing for NC
add.geometry <- function(spatial.df) {
  
  spatial.sf <- st_as_sf(spatial.df, coords = c('lon', 'lat'), crs = 4269) #now we've added geometry, and read it in in the right crs
  
  #change the crs to one that works really well for the triangle area of NC and is a coordinate system that is in meters. I just looked here for a coordinate reference system that covers North Carolina and uses meters https://epsg.io/5070 .
  spatial.sf <- st_transform(spatial.sf, crs = 5070)
  
  return(spatial.sf)
}
















#--
#you can figure this out better later, and just use the api census key if that's what you need to use to get this working rn
census_api_key("3ae0b5f2990a2cc0b1386b85d0f937d66b8ddc74")
nc <- get_acs(geography = "tract", 
              year = 2018,
              variables = c(tpopr = "B03002_001"), #let's just get total population for now
              state = "NC",
              survey = "acs5", 
              geometry = TRUE) 
nc <- nc %>%  dplyr::select(-moe) %>% #remove margin of error
  rename(Census_Tract = NAME) %>% #make census tract name understandable 
  pivot_wider(names_from = "variable", values_from = "estimate")
#this is getting census tracts, not counties..figure out how to get counties
#can get counties using stringr select rows that contain "Wake/Durham/Orange" in the Census_Tract column

#convert to the right crs that uses meters 
#First we need to convert the data into a coordinate system that is in meters. I just looked here for a coordinate reference system that covers North Carolina and uses meters https://epsg.io/5070 .
nc <- st_transform(nc, crs = 5070)


#get county
nc$county <- sub(" County.*", "", nc$Census_Tract) 
nc$county <- sub(".*, ", "", nc$county)

#filter to mbbs counties
mbbs_counties <- nc %>% filter(county %in% c("Chatham", "Durham", "Orange"))

#get just the county outlines, don't really care about census tracts - TODO

#plot
ggplot() +
  geom_sf(data = mbbs_counties, fill = "lightblue")+
  theme(panel.background = element_blank()) +
  geom_sf(data = coords_sf) 
  geom_sf(data = buffer)

  
#zoom-able map
library(mapview)
  
  mapview(mbbs_counties) +
  mapview(coords_sf) +
    mapview(buffer)

mapview(projectedforest) +
mapview(mbbs_counties) + #remove bg with native.crs = TRUE argument
  mapview(buffer) #+
  mapview(lat.lons_sf)

mapview(projectedforest)

#get an NC sf + sf of the three counties
plot_usmap(regions = "counties", include = c("37183", "37135", "37063")) #map of the three survey counties, used FIPS code
plot_usmap(include = c("NC")) #map of NC 

library(raster)
forest <- raster("Z:/GIS/LandCoverData/forest_3km.tif")

indigolocal <- raster("Z:/GIS/LandCoverData/nlcd_local") #whole folder
mapview(indigolocal) +
  mapview(mbbs_counties)

plot(projectedforest)
projectedforest <- projectRaster(forest, crs = 5070)

#clip raster - need something other than st join
nc_projectedforest <- st_join(x = projectedforest, y = mbbs_counties, join = st_within, left = FALSE)




#library(tidycensus)
data("fips_codes") %>% filter(state == "NC") #county %in% c("Wake County", "Orange County", "Durham County"))

