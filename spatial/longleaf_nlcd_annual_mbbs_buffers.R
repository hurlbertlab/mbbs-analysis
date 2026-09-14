#libraries
library(dplyr)
#install.packages("Rcpp", repos = "https://cloud.r-project.org")
#install.packages("units", repos = "https://cloud.r-project.org")
#install.packages("sf", repos = "https://cloud.r-project.org")
#install.packages("terra", repos = "https://cloud.r-project.org")
library(sf) #this is the spatial package
library(ggplot2)
library(tidyr)
library(terra)

#paths
pull_from_path <- "/proj/hurlbertlab/nlcd_landcover/nlcd_annual_products_1999_to_2024/" #didn't rename this, but we do have 2025 data now.
write_to_path <- "/proj/hurlbertlab/ijbgoulden/cropped_nlcds/mbbs_400m_buffers_nlcd_annual/"
file_folder_selection <- c("fraction_impervious_surface", "landcover", "landcover_change")
stop_coords_path <- "/proj/hurlbertlab/ijbgoulden/csv/route_stop_coordinates.csv"

nlcd_file_extension <- ".tif"

#NLCD data (working with annual products) we have years from 1999:2025
year <- 1999:2025
year <- as.character(year)

#load the mbbs buffer
mbbs_buffers_nogeom <- read.csv(stop_coords_path, header = TRUE) %>%
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
a <- 2
print(file_folder_selection[a])
stackpath <- paste0(pull_from_path, file_folder_selection[a])
print(stackpath)
rastlist <- list.files(path = stackpath, pattern = '.tif$', all.files = TRUE, full.name = TRUE)
rastlist #print
nlcdstack <- terra::rast(rastlist)
nlcdstack
print("stack of files read in as a raster successfully")

#put the buffers in the same projection as the nlcd
mbbs_buffers <- st_transform(mbbs_buffers, crs(nlcdstack))

#crop then mask rasters
bufferstack <- terra::crop(nlcdstack, mbbs_buffers)
    print("rasters cropped")
bufferstack <- terra::mask(bufferstack, mbbs_buffers)
    print("rasters masked")

#save rasters
for(i in 1:length(year)) {
    
    filename <- paste0(write_to_path, file_folder_selection[a], "/", year[i])
    terra::writeRaster(bufferstack[[i]], filename, filetype = "GTiff", overwrite = TRUE)
    print(i)
}

print(warnings())

print("buffer rasters saved")

#now, extract the data out of those buffers and save them to a csv
    extracted <-terra::extract(x = bufferstack, y = mbbs_buffers, df = TRUE)
    
    print("buffer data extracted")
    
    #make extracted nice
            mbbs_buffers_nogeom <- mbbs_buffers_nogeom %>%
                                   select(ID, route, stop_num)
    extracted <- extracted %>%
    left_join(mbbs_buffers_nogeom, by = "ID")
    
    
    write.csv(extracted, paste0(write_to_path, "extracted_buffers_", file_folder_selection[a], ".csv"), row.names = FALSE)
    print("buffer csv saved! all done ^u^")