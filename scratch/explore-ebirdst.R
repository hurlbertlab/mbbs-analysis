##Explore ebirdst
#https://ebird.github.io/ebirdst/

#Installation
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("ebird/ebirdst")

#load package
library(ebirdst)
#and the other required packages
library(terra)
library(sf)
library(fields)
library(rnaturalearth)
#ebird stats and trends data is stored in the GEOTIFF format

#request access (access granted immediately)
#https://ebird.org/st/request
set_ebirdst_access_key("vv51lucgatal")
#Restart R after setting the access key

#Examples come from ebirdst documentation on github, modified to work with California Quail

#download species of interest
path <- ebirdst_download(species = "California Quail", tifs_only = FALSE)
#this might take a minute, downloading 51 different files

#load relative abundance raster stack with one layer for each week
abd <- load_raster(path = path, resolution = 'lr')
#load_raster(path,product = c("abundance", "count", "occurrence", "percent-population"),period = c("weekly", "seasonal", "full-year"),metric = NULL,resolution = c("hr", "mr", "lr")

#load the species specific mapping parameters
pars <- load_fac_map_parameters(path)
#custom coordinate reference system
crs <- st_crs(pars$custom_projection)
#legend breaks
breaks <- pars$weekly_bins
#legend labels for top, middle, and bottom
labels <- pars$weekly_labels

#the date that each raster layer corresponds to is stored within the labels, let's take a look
weeks <- parse_raster_dates(abd)
print(weeks)

#let's pick what California quail weekly abundance looked like on November 2nd, 2021
abd <- abd[[44]]

#project raster to species specific coordinates
#the nearest neighbor method preserves cell values across projections
abd_prj <- project(trim(abd), crs$wkt, method = "near")

#get reference data from the rnaturalearth package
wh_states <- ne_states(country = c("United States of America", "New Zealand", "Canada"),
              returnclass = "sf") %>%
  st_transform(crs = crs) %>%
  st_geometry()
#you may need to
#devtools::install_github("ropensci/rnaturalearthhires")

#start plotting
par(mfrow = c(1,1), mar = c(0,0,0,0))

#use raster bounding box to set the spatial extent for the plot
bb <- st_as_sfc(st_bbox(trim(abd_prj)))
plot(bb, col = "white", border = "white")
#add background reference data
plot(wh_states, col = "#cfcfcf", border = NA, add = TRUE)

#plot zeros as light gray
plot(abd_prj, col = "#e6e6e6", maxpixels = nccell(abd_prj), 
     axes = FALSE, legend = FALSE, add = TRUE)

#define the color palette
pal <- abundance_palette(length(breaks) -1, "weekly")
#plot abundance
plot(abd_prj, col = pal, breaks = breaks, maxpixels = ncell(abd_prj), axes = FALSE, legend = FALSE, add = TRUE)

# legend
label_breaks <- seq(0, 1, length.out = length(breaks))
image.plot(zlim = c(0, 1), breaks = label_breaks, col = pal,
           smallplot = c(0.90, 0.93, 0.15, 0.85),
           legend.only = TRUE,
           axis.args = list(at = c(0, 0.5, 1), 
                            labels = round(labels, 2),
                            cex.axis = 0.9, lwd.ticks = 0))


#what if we want to look at just NZ?
# boundary polygon for NZ
nz <- ne_states(iso_a2 = "NZ", returnclass = "sf") %>% 
  # project to same coordinate reference system as the raster data
  st_transform(st_crs(abd_prj))

#crop data to nz
abd_nz <- crop(abd_prj, nz)

#map the cropped data
plot(abd_nz, axes = FALSE)

#or the US?
us <- ne_states(iso_a2 = "US", returnclass = "sf") %>%
  st_transform(st_crs(abd_prj)) 

#crop data to us
abd_us <- crop(abd_prj, us)

#map
plot(abd_us, axes = FALSE)


#Predict habitat associations
#https://ebird.github.io/ebirdst/reference/ebirdst_habitat.html
#define a spatial extent to calculate over 
e <- ebirdst_extent(c(xmin = -124, ymin = 32, xmax = -114, ymax = 42))
#^ spatial extent of california

habitat <- ebirdst_habitat(path = path, ext = e)
print(habitat[habitat$week=="11-02",], n = 24)
?ebirdst_predictors #<-this is the reference dataset we can load in to look at the predictor_labels and find out that ie: "umd_fs_c1 is Evergreen Needleleaf Forest."

#produce a cake plot
#a cake plot is a stacked area chart showing habitat associations in which area indicates the importance of a given land cover class and the position above or below the x-axis indicates the direction of the relationship.
plot(habitat, n_habitat_types = 10) #plot only the 10 most important habitats
#looks like roads and nighttime lights negatively impact California quail occurance quite strongly. 



#Other functions
#need to get a polygon for the species range?
  #stixelize() #generate polygons for eBird Status and Trends
  #load_ranges() Load seasons eBird Status and Trends range polygons
ranges <-  load_ranges(path = path, resolution = "lr")
plot(ranges[6])  

#need to subset the data spatiotemporally?
  #ebirdst_subset()
bb_vec <- c(xmin = -124, ymin = 32, xmax = -114, ymax = 42)
e <- ebirdst_extent(bb_vec, t = c("05-01", "07-30"))
CAbreedingseason <- ebirdst_subset(abd, ext = e)
#and then you can run trends etc. from here
