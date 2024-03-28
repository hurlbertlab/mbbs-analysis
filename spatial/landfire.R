#extract and save landfire data

library(tidyr)
library(dplyr)
library(sf) #this is the spatial package
library(terra)
library(stringr)
library(beepr)

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
extracted <-  terra::extract(x = bufferstack, y = mbbs_buffers, df = TRUE)

#add back in the route information
landfire <- left_join(extracted, mbbs_buffers, by = "ID") %>%
  #get the total number of pixels on each route_stop
  group_by(ID) %>%
  mutate(numpix = n()) %>%
  ungroup() %>%
  #get the frequency of each raw_landtype
  pivot_longer(cols = landfire_2001evh:landfire_2022evh, names_to = "year", names_prefix = "landfire_", values_to = "raw_landtype") %>% #pivot
  mutate(year = as.numeric(str_extract(year, "[0-9]+"))) %>% #make year a number
  group_by(County_Route_Stop, year, raw_landtype) %>%
  mutate(frequency = n()) %>%
  distinct() %>%
  ungroup() %>%
  #convert the raw land cover types to match the earlier kinds
  mutate(converted_landtype = case_when(
    year > 2014 & 
      raw_landtype > 99 & raw_landtype < 105 ~ 108,
    year > 2014 & 
      raw_landtype > 104 & raw_landtype < 111 ~ 109,
    year > 2014 & 
      raw_landtype > 110 & raw_landtype < 126 ~ 110,
    year > 2014 & 
      raw_landtype > 125 & raw_landtype < 151 ~ 111,
    year > 2014 & 
      raw_landtype > 150 & raw_landtype < 200 ~ 112,
    TRUE ~ raw_landtype
  ))

#create converted landfire database, getting the frequencies of the converted 
landfire_c <- landfire %>%
  group_by(ID, year, converted_landtype) %>%
  mutate(frequency_c = sum(frequency)) %>%
  arrange(ID, year, converted_landtype) %>%
  distinct(ID, year, converted_landtype, .keep_all = TRUE) %>%
  select(-raw_landtype, -frequency) %>%
  mutate(percent = frequency_c/numpix)

#if I'm only interested in forest height categories
landfire_cfh <- landfire_c %>% filter(converted_landtype > 100 & converted_landtype < 200)
#let's just plot for one route_stop........

#add in landfire classification information
  #this gets...complicated. early landfire 108-112 is information about tree heights. later landfire anything 101+ the part after the hundreds place is the m height of the trees. This changes so little, is essentially bins the tree categories into like, 2 segments. I Dont know that this is a valuable way to get tree heights. I think, the work is still work working to before ultimately coming to this conclusion. CONFIRM your thoughts before deciding offhand based on looking at the raw data and not clear summaries about how things change along routes. 







#write csv of extracted data
write.csv(extractedbuffers_terra, "/proj/hurlbertlab/ijbgoulden/extractedbuffers.csv")