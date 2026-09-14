##################
#
# Longleaf version of 
# longleaf_nlcd_annual_mbbs_buffers.R
# has stopped working correctly, probably bc
# of some bg changes to longleaf or the 
# terra:: package available on longleaf.. not sure. 
# Anyway, this is the file to get the 2025 nlcd data for the mbbs. And it works even if running the same code on longleaf doesn't.
#
#
####################


#libraries
library(dplyr)
library(sf) #this is the spatial package
library(ggplot2)
library(tidyr)
library(terra)


#paths
path <- "C:/users/ijbg/Downloads/Annual_NLCD_LndCov_2025_CU_C1V2/"

routestops <- read.csv("data/mbbs/route_stop_coordinates.csv")

nlcd_classifications <- read.csv("spatial/nlcd_classifications.csv")
nlcdfileextension <- ".tif"

#NLCD data (working with annual products) we have years from 1999:2025
year <- 2025
year <- as.character(year)

#load the mbbs buffer
mbbs_buffers_nogeom <- routestops |>
  dplyr::select(-stop_notes) %>%
  dplyr::group_by(route, stop_num) %>%
  dplyr::mutate(ID = dplyr::cur_group_id())
#create 400m buffers from route stop points
#convert lat/lon to points geometry
mbbs_buffers <- st_as_sf(mbbs_buffers_nogeom, coords = c('lon', 'lat'), crs = 4269) 
#transform to a meters based crs
mbbs_buffers <- st_transform(mbbs_buffers, crs = 5070) #meters, NC 
#buffer each point by 400m, now geometry is polygons
mbbs_buffers <- st_buffer(mbbs_buffers, dist = 400) 

#read the NLCD files into a stack
#set file folder by changing 'a', can be 1:3. 1 = fraction_impervious_surface, 2 = landcover, 3 = landcover_change
rastlist <- list.files(path = path, pattern = ".tif$", full.name = TRUE)
rastlist

nlcdstack <- terra::rast(rastlist)
#well, yay. that worked fine! Not sure why longleaf was having an error.
print("stack of files read in as a raster successfully")

#put the buffers in the same projection as the nlcd
mbbs_buffers <- st_transform(mbbs_buffers, crs(nlcdstack))

#crop then mask rasters
bufferstack <- terra::crop(nlcdstack, mbbs_buffers)
print("rasters cropped")
bufferstack <- terra::mask(bufferstack, mbbs_buffers)
print("rasters masked")

#save rasters
i = 1
  filename <- paste0("spatial/nlcd/annual_nlcd_landcover_mbbs_buffers_2025")
  terra::writeRaster(bufferstack[[i]], filename, filetype = "GTiff", overwrite = TRUE)
  print(i)

#eh. that MAYBE worked. not too fussed.


extracted <-terra::extract(x = bufferstack, y = mbbs_buffers, df = TRUE)
print("buffer data extracted")

#make extracted nice
mbbs_buffers_nogeom <- mbbs_buffers_nogeom %>%
  select(ID, route, stop_num)
extracted <- extracted %>%
  left_join(mbbs_buffers_nogeom, by = "ID")


write.csv(extracted, "spatial/nlcd/nlcd_annual_extracted_buffers_landcover_2025.csv", row.names = FALSE)
print("buffer csv saved! all done ^u^")

#huh. 2025's a little different. Not saving as number, it's saving as the values like "Deciduous Forest". 

#let's go ahead and summarize this year's data independently as well. 
npixels <- extracted %>%
  group_by(ID) %>%
  summarize(numpix = n())

landtype_bystop <- extracted %>%
  group_by(ID, route, stop_num, `NLCD Land Cover Class`) %>%
  summarize(count = n(), .groups = 'drop') %>%
  left_join(npixels, by = "ID") %>%
  mutate(percent = (count/numpix)*100,
         year = 2025,
         `NLCD Land Cover Class` = case_when(
           `NLCD Land Cover Class` == "Shrub/Scrub" ~ "Scrub/Shrub",
           TRUE ~ `NLCD Land Cover Class`
         )) %>%
  #add nlcd classification information
  left_join(nlcd_classifications, by = c("NLCD Land Cover Class" = "description"))

write.csv(landtype_bystop, "data/nlcd-landcover/nlcd_annual_summarized_2025.csv", row.names = FALSE)
#then edited columns to match with nlcd_annual_landtype_bystop

all <- read.csv("data/nlcd-landcover/nlcd_annual_landtype_bystop.csv")
twofive <- read.csv("data/nlcd-landcover/nlcd_annual_summarized_2025.csv")

new <- bind_rows(all, twofive) |>
  arrange(ID, year)

write.csv(new, "data/nlcd-landcover/nlcd_annual_landtype_bystop.csv", row.names = FALSE)
