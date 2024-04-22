#bsft % urbanization by County

library(sf) #this is the spatial package
library(terra) #this is the raster package replacement
library(stringr)
library(dplyr)

#read in whole nc polygon
nc <- st_read("spatial/shapefiles/NC_County_Polygons/North_Carolina_State_and_County_Boundary_Polygons.shp")

#filter to counties we're interested in
nc <- nc %>% filter(County %in% c("Chatham", "Orange", "Durham"))

#save the counties we're interested in.
#st_write(nc, "spatial/shapefiles/NC_County_Polygons/studyarea.shp")

#check that it saved them correctly (yes)
#check <- st_read("spatial/shapefiles/NC_County_Polygons/studyarea.shp")
#plot(check)

#now, I want the nlcd 1999 and nlcd 2023 - or the latest one - for nc so I can clip it to these counties.
path <- "spatial/nlcd/"
nlcdlist <- list.files(path = path, pattern = '.tif$', all.files=TRUE, full.name=TRUE)
stack <- rast(nlcdlist)

#put everything in the same projection
#we're going to do this the other way, with the nc having to change.
nc <- st_transform(nc, crs(stack))
stack <- terra::project(stack, crs(nc))
#okay that took a bit. save that.
#writeRaster(stack, "spatial/nlcd/projected_nc_stack", filetype = "GTiff")
#stack <- rast("spatial/nlcd/projected_nc_stack")
#plot(stack)
#plot(nc)

#crop the nlcd
studyarea_nlcd <- terra::crop(stack, nc)
studyarea_nlcd <- terra::mask(studyarea_nlcd, nc)
studyarea_nlcd <- terra::trim(studyarea_nlcd)
plot(studyarea_nlcd)
#okay. problem solved.

#save, we might want it later
#writeRaster(studyarea_nlcd, "spatial/nlcd/projected_studyarea_stack.tiff", filetype = "GTiff", overwrite = TRUE)

#Coming back to this script later, read in the data.
#studyarea_nlcd <- rast("spatial/nlcd/projected_studyarea_stack.tiff")

#extract the data.
extracted <-  terra::extract(x = studyarea_nlcd, y = nc, df = TRUE)

#left_join back in the counties
nc <- nc %>%
  mutate(ID = row_number())


county_summaries <- extracted %>%
  left_join(nc, by = "ID")

cols_list <- c("county", "npix", "urban2001", "urban2021", "f2001", "f2021")

#make trend table
trend_table <- as.data.frame(matrix(ncol = length(cols_list), nrow = 0))
#name the columns
colnames(trend_table) <- cols_list
#initiate trend table with first row, fill with NAs
trend_table[1:3,] <- NA

df <- trend_table


for(i in 1:3) {
  
  c <- county_summaries %>% 
    filter(ID == i)
  
  df$county[i] <- c$County[1]
  df$npix[i] <- nrow(c)
  df$urban2001[i] <- nrow(c %>% filter(nc_nlcd_2001 %in% 21:24))
  df$urban2021[i] <- nrow(c %>% filter(nc_nlcd_2021 %in% 21:24))
  df$f2001[i] <- nrow(c %>% filter(nc_nlcd_2001 %in% 41:43))
  df$f2021[i] <- nrow(c %>% filter(nc_nlcd_2021 %in% 41:43))
  
}

df <- df %>%
  mutate(pu2001 = (urban2001/npix)*100,
         pu2021 = (urban2021/npix)*100,
         pf2001 = (f2001/npix)*100,
         pf2021 = (f2021/npix)*100,
         difpu = pu2021 - pu2001)

write.csv(df, "scratch/bsft_percent_urbanization_county_df.csv", row.names = FALSE)

#lets try and plot..
barplot(height = t(df[,c("pu2001", "pu2021")]),
        beside = TRUE,
        names.arg = df$county,
        col = c("grey95", "grey"),
        main = "Landcover 2001 vs 2021",
        xlab = "County",
        ylab = "% Landcover",
        ylim = c(0,50))

legend(
  "topright",
  legend = c("2001", "2021"),
  fill = c("grey95", "grey")
)



barplot(height = t(df[,c("pu2001", "pu2021", "pf2001", "pf2021")]),
        beside = TRUE,
        names.arg = df$county,
        col = c("grey95", "grey", "darkgreen", "lightgreen"),
        main = "Landcover 2001 vs 2021",
        xlab = "County",
        ylab = "% Landcover",
        ylim = c(0,100))

legend(
  "topright",
  legend = c("2001 urban", "2021 urban", "2001 forest", "2021 forest"),
  fill = c("grey95", "grey", "darkgreen", "lightgreen")
)

#okay. one last thing it would be really nice to have here would be the comparison of the routes by county. That's what this map makes me want. To show the variation captured on routes.

