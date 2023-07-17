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

#read in the dataset
mbbs <- read.csv("data/analysis.df.csv", header = TRUE)

#how many coordinates should we expect?
length(unique(mbbs$lon));length(unique(mbbs$lat)) #both 764

#let's take a look at lat/lons 
#tidyr::expand() gives all combinations of values that appear in the data
coords <- mbbs %>% group_by(mbbs_county, route_num, stop_num) %>% expand(nesting(lat, lon))

#break out by year
coods_year <- mbbs %>% group_by(mbbs_county, route_num, stop_num, year) %>% expand(nesting(lat,lon))

#create official coordinates
coords$official_lat <- coords$lat
coords$official_lon <- coords$lon

#Fix official coordinates that are in the wrong spot
  #Chatham 13, original location is more accurate to route description than ebird location,   official 1st stop lat and lon based off stop_num == NA
  coords$official_lat[coords$route_num ==53 & coords$stop_num == 1 & is.na(coords$stop_num) == FALSE] <- coords$lat[coords$route_num == 53 & is.na(coords$stop_num) == TRUE]
  coords$official_lon[coords$route_num ==53 & coords$stop_num == 1 & is.na(coords$stop_num) == FALSE] <- coords$lon[coords$route_num == 53 & is.na(coords$stop_num) == TRUE]
  
  #it perhaps introduces minor error, but I think it's perfectly okay to make the decision that starting points should be based on the new ebird points? In which case, at least any error is systematic. If we're going that we can just:
  coords <- coords %>% filter(is.na(stop_num) == FALSE)
  
  #now we can fix duplicates and decide on the official lat/lon
  #doesn't really matter, WHICH one we pick, just that we pick one in most cases
  #guide:
  #look for most recent, pick the most recent points
  #color-code by latest year, then I can tell them apart and assign (to do this, mapview coords_year_sf)
    #Orange 7 stop 2
    coords$official_lat[coords$route_num == 7 & coords$stop_num == 2] <- 36.13889
    coords$official_lon[coords$route_num == 7 & coords$stop_num == 2] <- -79.22034
    #Orange 7 stop 3
    coords$official_lat[coords$route_num == 7 & coords$stop_num == 3] <- 36.14466
    coords$official_lon[coords$route_num == 7 & coords$stop_num == 3] <- -79.21589
    #Orange 7 stop 4
    coords$official_lat[coords$route_num == 7 & coords$stop_num == 4] <- 36.14890
    coords$official_lon[coords$route_num == 7 & coords$stop_num == 4] <- -79.20770
    #Orange 7 stop 5
    coords$official_lat[coords$route_num == 7 & coords$stop_num == 5] <- 36.15205
    coords$official_lon[coords$route_num == 7 & coords$stop_num == 5] <- -79.20053
    #Orange 7 stop 6
    coords$official_lat[coords$route_num == 7 & coords$stop_num == 6] <- 36.15863
    coords$official_lon[coords$route_num == 7 & coords$stop_num == 6] <- -79.19863
    #Orange 7 stop 7
    #these points are a little further apart let's average them
    coords$official_lat[coords$route_num == 7 & coords$stop_num == 7] <- mean(36.16424,36.16648)
    coords$official_lon[coords$route_num == 7 & coords$stop_num == 7] <- mean(-79.19681, -79.19614)
    #Orange 7 stop 8
    coords$official_lat[coords$route_num == 7 & coords$stop_num == 8] <- 36.17145
    coords$official_lon[coords$route_num == 7 & coords$stop_num == 8] <- -79.19305
    #Orange 7 stop 9
    coords$official_lat[coords$route_num == 7 & coords$stop_num == 9] <- 36.17449
    coords$official_lon[coords$route_num == 7 & coords$stop_num == 9] <- -79.18509
    #O7,10
    coords$official_lat[coords$route_num == 7 & coords$stop_num == 10] <- 36.17928
    coords$official_lon[coords$route_num == 7 & coords$stop_num == 10] <- -79.17873
    #o7,11
    coords$official_lat[coords$route_num == 7 & coords$stop_num == 11] <- 36.18373
    coords$official_lon[coords$route_num == 7 & coords$stop_num == 11] <- -79.17701
    #o7, 12
    coords$official_lat[coords$route_num == 7 & coords$stop_num == 12] <- coods_year$lat[coods_year$route_num == 7 & coods_year$stop_num == 12 & coods_year$year == 2022]
    coords$official_lon[coords$route_num == 7 & coords$stop_num == 12] <- coods_year$lon[coods_year$route_num == 7 & coods_year$stop_num == 12 & coods_year$year == 2022]
    #Orange 7, rest
    for(i in 13:20) {
      coords$official_lat[coords$route_num == 7 & coords$stop_num == i] <- coods_year$lat[coods_year$route_num == 7 & coods_year$stop_num == i & coods_year$year == 2022]
      coords$official_lon[coords$route_num == 7 & coords$stop_num == i] <- coods_year$lon[coods_year$route_num == 7 & coods_year$stop_num == i & coods_year$year == 2022]
    }
    
    #Orange rt 2 stop 14
    for(i in 14) {
      coords$official_lat[coords$route_num == 2 & coords$stop_num == i] <- coods_year$lat[coods_year$route_num == 2 & coods_year$stop_num == i & coods_year$year == 2022]
      coords$official_lon[coords$route_num == 2 & coords$stop_num == i] <- coods_year$lon[coods_year$route_num == 2 & coods_year$stop_num == i & coods_year$year == 2022]
    }
    
    #Orange rt 10
    for(i in 1:20) {
      coords$official_lat[coords$route_num == 10 & coords$stop_num == i] <- coods_year$lat[coods_year$route_num == 10 & coods_year$stop_num == i & coods_year$year == 2021]
      coords$official_lon[coords$route_num == 10 & coords$stop_num == i] <- coods_year$lon[coods_year$route_num == 10 & coods_year$stop_num == i & coods_year$year == 2021]
    }
    
    #Chatham rt 9 (49)
    ##bigger issues lets leave for now
    
    #Chatham rt 8 (48)
    for(i in 1:20) {
      coords$official_lat[coords$route_num == 48 & coords$stop_num == i] <- coods_year$lat[coods_year$route_num == 48 & coods_year$stop_num == i & coods_year$year == 2022]
      coords$official_lon[coords$route_num == 48 & coords$stop_num == i] <- coods_year$lon[coods_year$route_num == 48 & coods_year$stop_num == i & coods_year$year == 2022]
    }
    
    
    
  #and then remove duplicates, and they're going to have the same official lat/lon so we can just keep the first instance
  coords <- coords %>% distinct(route_num, stop_num, .keep_all = TRUE)
  
  #and we can actually remove the non-official lat/lon columns, change to official lat/lon
  coords <- coords %>% mutate(lat = official_lat,
                              lon = official_lon)


#let's change that into a spatial dataset with geometry
coords_sf <- st_as_sf(coords, coords = c('official_lon', 'official_lat'), crs = 4269) #now we've added geometry, and read it in in the right crs

#change crs
coords_sf <- st_transform(coords_sf, crs = 5070)

#if needing to map coords_year
#coods_year_sf <- st_as_sf(coods_year, coords = c('lon', 'lat'), crs = 4269)
#coods_year_sf <- st_transform(coods_year_sf, crs = 5070)

#?sf_buffer - create a buffer of 400 m around each of the points
buffer <- st_buffer(coords_sf, dist = 400)
#buff <- st_buffer(coods_year_sf, dist = 400)

mapview(mbbs_counties) +
  mapview(coords_sf) +
  mapview(buffer)

#manipulate to upload to google maps
coords_sf <- coords_sf %>% mutate(
  county_factor = case_when(
    mbbs_county == "orange" ~0, #change route-nums back to original (not unique by county)
    mbbs_county == "durham"~-20,
    mbbs_county == "chatham" ~-40
  ),
  route_num = route_num + county_factor
) %>%
  dplyr::select(-county_factor) #remove county factor

coords_sf <- coords_sf %>%
  mutate(County_Route_Stop = paste(mbbs_county,as.character(route_num),as.character(stop_num), sep = "-"),
         County_Route = paste(mbbs_county, as.character(route_num), sep = "-")) %>% relocate(County_Route_Stop, mbbs_county) %>% relocate(County_Route, County_Route_Stop)


coords_sf <- coords_sf %>% rename(WKT = geometry) #change name of geometry column for google

#export for google 
write.csv(coords_sf, "spatial/GoogleMaps.csv", row.names = F)
#instuctions: https://support.google.com/mymaps/answer/3024836?hl=en&co=GENIE.Platform%3DDesktop#zippy=%2Cstep-prepare-your-info















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

