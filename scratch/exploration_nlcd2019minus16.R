
library(terra)
library(dplyr)

mbbs16 <- terra::rast("spatial/nlcd/buffer_nlcd_2016.tif")
mbbs19 <- terra::rast("spatial/nlcd/buffer_nlcd_2019.tif")
color_table <- read.csv("spatial/nlcd_classifications.csv") %>% 
  dplyr::select(code, color)

coltab(mbbs16) <- color_table
coltab(mbbs19) <- color_table

terra::plot(mbbs16)
terra::plot(mbbs19)

mbbsminus = mbbs19 - mbbs16
terra::plot(mbbsminus)
