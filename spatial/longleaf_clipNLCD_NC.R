##clip NLCD files to the state of nc using longleaf

library(sf)
library(raster)
library(dplyr)

#get the state boundary we want to clip to of NC
nc <- read_sf("/work/users/i/j/ijbg/nc.shp/nc.shp") 

#load the NLCD from the lab longleaf file
nlcd_2001 <- raster("/proj/hurlbertlab/nlcd_landcover/nlcd_2001_landcover_2011_edition_2014_10_10/nlcd_2001_whole_simplified.tif")
print("raster loaded successfully")

#ensure the nlcd loaded properly by printing a pdf
pdf(file = "/work/users/i/j/ijbg/checknlcd_2001.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 10) # The height of the plot in inches
plot(nlcd_2001)
dev.off()

#transform the nc boundary to match the projection of the NLCD
nc <- st_transform(nc, crs(nlcd_2001))

#ensure projection worked by plotting nc atop the nlcd
pdf(file = "/work/users/i/j/ijbg/checknc_nlcd_2001.pdf", 
    width = 10, # The width of the plot in inches
    height = 10) # The height of the plot in inches
plot(nlcd_2001)
plot(nc, add = TRUE)
dev.off()

#clip the NLCD to NC boundaries - both crop (to bounding box) and mask (make exact to boundaries)
nc_nlcd_2001 <- raster::crop(nlcd_2001, nc)
nc_nlcd_2001 <- raster::mask(nc_nlcd_2001, nc)

#check the clipped raster looks how it should
pdf(file = "/work/users/i/j/ijbg/checkncclipworked.pdf",
    width = 10, # The width of the plot in inches
    height = 10) # The height of the plot in inches
plot(nc_nlcd_2001)
dev.off()

#save the clipped NLCD that's now in the shape of NC
writeRaster(nc_nlcd_2001, "/work/users/i/j/ijbg/nc_nlcd_2001", format = "raster", overwrite = TRUE) #overwrites any other raster file named nc_nlcd_2001

print("Complete :)")
#easy to check that the output file ran the whole thing