library(sf)
library(terra)
library(ggplot2)
library(scales) #for some color selection from ggplot
library(tidyr)
library(dplyr)

##################################
#
# Graphs and supplementary table creation for
# the MBBS data paper
#
#
####################################
devtools::load_all()
mbbs <- create_mbbs_data()
surveys <- mbbs$surveys %>% select(route, year) %>% mutate(route_level = TRUE)
ss <- mbbs$stop_surveys %>% group_by(year, route) %>% summarize(stop_level = TRUE)

allsurvs <- left_join(surveys, ss, by = c("route", "year")) %>%
  arrange(desc(route)) #so they display with chatham 1 up top

matrix <- allsurvs %>% 
  mutate(value = case_when(
    route_level & stop_level ~ 2,
    route_level ~ 1,
    TRUE ~ 3)) %>% 
  select(route, year, value) %>% 
  pivot_wider(names_from = year, values_from = value) %>%
  relocate(`1999`) %>%
  relocate(route) 

matrix[is.na(matrix)] <- 3

matrix_final <- as.matrix(matrix[,2:28])
rownames(matrix_final) <- matrix$route

#pick colors
#colors from: https://colorbrewer2.org/#type=sequential&scheme=YlGnBu&n=6
palette <- colorRampPalette(c("#41b6c4", "#2c7fb8", "#edf8b1")) #GreytoGreen
current.palette = palette(3)


png(filename = "scratch_routeyearmatrix.png", 
    width = 540,
    height = 530,
    units = "px", 
    type = "windows")
heatmap(as.matrix(matrix_final), 
        Colv = NA, 
        Rowv = NA, 
        scale = "none", #map the original values directly
        #col = topo.colors(3)
        col = current.palette,
        #ylab = "Route",
        cexRow = 1.1,
        cexCol = 1.1,
        add.expr = abline(h=.5:34.5, col = "#2c7fb8")
)
title(ylab="Route", line=-33.5, cex.lab=1.2)
#title(main = "Route x Year")

dev.off()

png(filename = "scratch_routeyearlegend.png", 
    width = 530,
    height = 530,
    units = "px", 
    type = "windows")
plot(NULL)
#going to need to fix where this legend plots but yeah :)
legend("topleft",
       title = "Data Availability", 
       legend = c("Stop Level", "Route Only", "No Survey"),
       fill = c("#2c7fb8", "#41b6c4", "#edf8b1"), 
       #col = c("#2c7fb8", "#41b6c4", "#edf8b1"),
       #pch = 15
)
dev.off()



#------------------------------------
#
# Plot a map of the three counties and the routes. Should include a scale bar 
# in the final version.
#
#------------------------------------

# NLCD data
#nlcd <- rast("C:/Users/Ivara/Downloads/devo_1km_east.tif") #this is percent urbanization.
nlcd <- rast("spatial/nlcd/Annual_NLCD_LndCov_2023_CU_C1V0.tif") #awesome, the 2023 works :)
#we will want to resample this later probably to have fewer colors. Just water/urban/grassland/forest at maybe the 400m scale instead of 30m
nlcd_classif <- read.csv("spatial/nlcd_classifications.csv", header = TRUE) %>% mutate(code = as.factor(code))

# reclassify matrix
#reclass_matrix <- matrix(c(
#  # NLCD Code, Your Code
#  11, 1,  # Water
#  41, 2,  # Forest
#  42, 2, 
#  43, 2,
#  71, 3,  # Grassland
#  21, 4,  # Urban
#  22, 4,
#  23, 4,
#  24, 4
#), ncol = 2, byrow = TRUE)


# NC couties:
NC <- read_sf("spatial/shapefiles/NC_County_Polygons/North_Carolina_State_and_County_Boundary_Polygons.shp")
counties <- read_sf("spatial/shapefiles/NC_County_Polygons/North_Carolina_State_and_County_Boundary_Polygons.shp") |>
  filter(County %in% c("Orange", "Durham", "Chatham")) |>
  st_transform(crs(nlcd))
surrounding_counties <- read_sf("spatial/shapefiles/NC_County_Polygons/North_Carolina_State_and_County_Boundary_Polygons.shp") |>
  filter(County %in% c("Alamance", "Caswell", "Person", "Granville", "Wake", "Lee", "Moore", "Randolph")) |>
  st_transform(crs(nlcd))
#counties_vect <- vect(counties)  # Convert sf to SpatVector for terra::mask

# Verify CRS match (optional - for debugging)
#cat("NLCD CRS:", crs(nlcd), "\n")
#cat("Counties CRS:", st_crs(counties)$wkt, "\n")

# Trim NLCD to county boundaries
trimmed_nlcd <- nlcd |>
  crop(counties) |>
  mask(counties) 

resampled_nlcd <- trimmed_nlcd |>
  aggregate(fact = 10, fun = "modal", na.rm = TRUE)
# Aggregate from 30m to 300m using the modal (most frequent) value
# fact = 10 because 300m / 30m = 10
# eh, honestly it just looks worse aggregated. I think actually leave it at the 30x30 resolution
# aggregate(fact = 10, fun = "modal", na.rm = TRUE)

#colors for nlcd
#colors <- read.csv("spatial/")

# routes
surveys <- read.csv("spatial/route_stop_coordinates.csv") |>
  st_as_sf(coords = c('lon', 'lat'), crs = 4269) |>
  st_transform(crs(nlcd)) # Transform surveys to match the NLCD's CRS) 
unique_routes <- unique(surveys$route)  # Assuming column name is 'route'
#get ggplot2 default hex color codes from 1 to 34
colors <- hue_pal()(34)
colors <- c("#F8766D",
            "#00BCD6",
            "#F07E4E",
            "#00C19D",
            "#00BE6E",
            "#00C087",
            "#DD8D00",
            "#45B500",
            "#B2A100",
            "#9FA700",
            "#E46DF5",
            "#F166E8",
            "#E7851E",
            "#FA62D9",
            "#9C8DFF",
            "#FF61C7",
            "#D277FF",
            "#00B3F2",
            "#00ABFC",
            "#6DB100",
            "#29A3FF",
            "#BA82FF",
            "#FD6F87",
            "#FF63B4",
            "#89AC00",
            "#D09400",
            "#00B927",
            "#7398FF",
            "#00C0B2",
            "#00BC51",
            "#00B8E5",
            "#FD6F86",
            "#00BFC4",
            "#FF689E")
names(colors) <- unique_routes

#North American Breeding Bird Survey routes
bbs_routes <- read_sf("Z:/Databases/BBS/GPS_stoplocations/bbsrte_2012_alb/") |>
  #put in same transformation as everything else
  st_transform(crs(NC)) |>
  #crop to NC
  st_crop(NC) |>
  st_transform(crs(nlcd))
plot(st_geometry(bbs_routes))

#set all background to transparent
par(bg = NA)  # or par(bg = "transparent")


{
  plot(
    #resampled_nlcd, 
    resampled_nlcd,
    axes = FALSE,
    bg = "transparent") #maybe just re-code all water as blue.
  plot(st_geometry(counties), add = TRUE, border = "black", lwd = 2)
  plot(st_geometry(surrounding_counties), add = TRUE, border = "black", lwd = 1)
  #add a little transparency overtop
  #plot(st_geometry(counties), add = TRUE, col = adjustcolor("white", alpha = ".1"))
  
  #add the NABBS routes
  plot(st_geometry(bbs_routes), add = TRUE, col = "white", lwd = 5)
  plot(st_geometry(bbs_routes), add = TRUE, 
       col = "purple",
       lwd = 3)
  
  #add the NCMBBS routes
  plot(st_geometry(surveys), add = TRUE, col = "black", pch = 16, cex = 1)
  plot(st_geometry(surveys), add = TRUE, col = "white", pch = 16, cex = .8)
  plot(st_geometry(surveys), add = TRUE, col = "black", pch = 16, cex = .6)
  
}

{
  plot(x = 1, y = 1, xlim = c(1,10), ylim = c(1,10))
  legend("center",
         legend = c("developed"),
         fill = c("red"),
         bty = "n")
}

# Add each route with different color
#for(route in unique_routes) {
#  route_points <- surveys[surveys$route == route, ]
#  plot(st_geometry(route_points), 
#       add = TRUE, 
#       col = colors[route], 
#       pch = 16, 
#      cex = 0.6)
#}
#needs a scale bar
#and to remove the legends
#but yay! moving towards a map ^u^

#inset
plot(st_geometry(NC), border = "grey20") 
plot(st_geometry(counties |> st_transform(crs(NC))), add = TRUE, col = "#68AA63")
dev.off()

##############################
#
# Characterize Observer Turnover
#
#
##############################
library(dplyr)
library(scales)
library(stringr)
surveys <- read.csv("data/mbbs/surveys.csv") |>
  select(route, year, standardized_observers) |>
  #assign a unique number to each unique observer within a route
  #NA observers get value maximum_n_colors + 1
  group_by(route) |>
  mutate(
    # Create a temporary column to force 1999 to the top
    year_order = case_when(
      str_detect(route, "cthm") & year == 2000 ~ 0,
      str_detect(route, "cthm") & year != 2000 ~ 1,
      str_detect(route, "orng") & year == 1999 ~ 0,
      str_detect(route, "orng") & year != 1999 ~ 1,
      str_detect(route, "drhm") ~ ifelse(year == 2002, 0, 1)
  )) |>
  arrange(route, year_order, year, .by_group = TRUE) |>
  # Now handle observer ordering: observers from 1999 first, then consecutive observers
  mutate(
    # Identify which observer is first
    first_observer = first(standardized_observers),
    # Create ordering for observers: 1999 observers first, then others
    observer_order = ifelse(standardized_observers == first_observer, 0, 1)
  ) |>
  arrange(route, year_order, observer_order, standardized_observers, .by_group = TRUE) |>
  #okay! yay! first observer is now always first.
  mutate(observer_id = consecutive_id(standardized_observers),
         route_max_turnover = max(observer_id)) |>
  ungroup() |>
  #re-arrange back to normal
  arrange(route, year) |>
  arrange(desc(route)) |> #so they display with chatham 1 up top
  #hell yeah
  select(-first_observer) |> #too confusing if it stays in
  #hm! Steven suggests arranging by frequency of observer turnover.
  arrange(desc(route_max_turnover), route, year) |>
  mutate(route = paste0(route," (", route_max_turnover, ")")) |>
  mutate(route = case_when(route == "orng-12 (1)" ~ "orng-12 (1 turnover)", 
                           TRUE ~ route))

#how many possible observer changes are there?
n_obs_changes <- surveys |> group_by(route) |> summarize(n = n_distinct(standardized_observers))
#up to 11 unique observers across a route. Okay, instead of assigning each observer their own unique code (gets very visually messy with 113 unique observers, just about impossible to tell some of the observer changes apart), let's assign a new code every time an observer changes.
maximum_n_colors <- max(n_obs_changes$n)
#11 max colors
palette <- palette(hcl.colors(maximum_n_colors, "viridis"))
palette[1] <- "black"
palette[maximum_n_colors+1] <- "#edf8b1"
palette <- c( 
  "#e0f3db",
  "#ccece6",
  "#7bccc4",
  "#66c2a4",
  "#4eb3d3",
  "#ccebc5",
  "#c6dbef",
  "#a8ddb5",
  "#2b8cbe",
  "#0868ac",
  "#084081",
  "white" #NA values
)
rainbow(11)
  

          #palette <- rainbow((length(unique(surveys$standardized_observers))))
          #palette <- palette(hcl.colors(113, "viridis"))
          #palette <- hue_pal()(113)
          #palette <- palette.colors(113)
          #palette[114] <- "#edf8b1"


  
#assign each observer a unique color
#observer_color_map <- data.frame(
#  standardized_observers = unique#(surveys$standardized_observers), 
#  color = rainbow(length(unique(surveys$standardized_observers)))
#  )



#surveys <- surveys |>
# left_join(observer_color_map, by = c("standardized_observers"))

matrix <- surveys %>% 
  mutate(value = observer_id) %>% 
  select(route, year, value) %>% 
  pivot_wider(names_from = year, values_from = value) %>%
  relocate(`1999`) %>%
  relocate(route) 

matrix[is.na(matrix)] <- maximum_n_colors+1 #for missing values

matrix_final <- as.matrix(matrix[,2:28])
rownames(matrix_final) <- matrix$route

png(filename = "scratch_observermatrix.png", 
    width = 540,
    height = 530,
    units = "px", 
    type = "windows")
heatmap(as.matrix(matrix_final), 
        Colv = NA, 
        Rowv = NA, 
        scale = "none", #map the original values directly
        #col = topo.colors(3)
        col = palette,
        #ylab = "Route",
        cexRow = 1.1,
        cexCol = 1.1,
        add.expr = abline(h=.5:34.5, col = "#2c7fb8")
)
title(ylab="Route", line=-33.5, cex.lab=1.2)
dev.off()

png(filename = "scratch_observerlegend.png", 
    width = 530,
    height = 530,
    units = "px", 
    type = "windows")
plot(NULL)
#going to need to fix where this legend plots but yeah :)
legend("topleft",
       title = "Data Availability", 
       legend = c("Stop Level", "Route Only", "No Survey"),
       fill = c("#2c7fb8", "#41b6c4", "#edf8b1"), 
       #col = c("#2c7fb8", "#41b6c4", "#edf8b1"),
       #pch = 15
)
dev.off()

#quantify frequency of multi-observer surveys
quant <- surveys |>
  mutate(mult_obs = str_detect(standardized_observers, ","))
table(quant$mult_obs)
sum(quant$mult_obs == TRUE) / nrow(quant)

