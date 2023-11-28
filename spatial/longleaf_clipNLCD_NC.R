##clip NLCD files to the state of nc using longleaf
#loop through all files

library(sf)
library(raster)
library(dplyr)

#get the state boundary we want to clip to of NC
nc <- read_sf("/proj/hurlbertlab/ijbgoulden/nc.shp/nc.shp") 

#list of the names of the nlcd files
filename <- c("NLCD_2001_Land_Cover_L48_20190424", "NLCD_2004_Land_Cover_L48_20190424", "NLCD_2006_Land_Cover_L48_20190424", "NLCD_2008_Land_Cover_L48_20190424", "NLCD_2011_Land_Cover_L48_20190424", "NLCD_2013_Land_Cover_L48_20190424",  "NLCD_2016_Land_Cover_L48_20190424", "NLCD_2019_Land_Cover_L48_20210604", "NLCD_2021_Land_Cover_L48_20230630")

year <- c("2001", "2004", "2006", "2008", "2011", "2013", "2016", "2019", "2021")

#function to make an nlcd raster for the given filename + year
clipnlcd_nc <- function(filename, year, nc) {
  
  #create the full filename
  filename <- paste("/proj/hurlbertlab/nlcd_landcover/", filename,"/",filename,".img", sep = "")
  
  #load in the raster from /proj/hurlbertlab/nlcd_landcover
  nlcd <- raster(filename)
  
  #transform the nc boundary to match the projection of the NLCD
  nc <- st_transform(nc, crs(nlcd))
  
  #clip the NLCD to NC boundaries - both crop (to bounding box) and mask (make exact to boundaries)
  nc_nlcd <- raster::crop(nlcd, nc)
  nc_nlcd <- raster::mask(nc_nlcd, nc)
  
  #check the clipped raster looks how it should
  pdffilename <- paste("/proj/hurlbertlab/ijbgoulden/checkclip",year,".pdf",sep="")
  pdf(file = pdffilename,
      width = 10, # The width of the plot in inches
      height = 10) # The height of the plot in inches
  plot(nc_nlcd)
  dev.off()
  
  #save the clipped NLCD that's now in the shape of NC
  writeRaster(nc_nlcd, paste("/proj/hurlbertlab/ijbgoulden/nc_nlcd_",year,sep=""), format = "GTiff", overwrite = TRUE) #overwrites any other raster file named nc_nlcd_2001
}

for(i in 1:length(filename)){
  clipnlcd_nc(filename[i], year[i], nc)
}

print("Complete :)")