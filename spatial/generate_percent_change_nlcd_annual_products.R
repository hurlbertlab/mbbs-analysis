###########################
#
# Read in the csvs of the nlcd
# annual data products that have been
# extracted for each mbbs buffer
# and summarize each stop
#
###########################

library(dplyr)
library(stringr)

#read in the three buffers
files <- list.files("spatial/nlcd/", pattern = ".csv$", full.names = TRUE)
frac_impervious_surface <- read.csv(files[1]) 
landcover <- read.csv(files[2])
landcover_change <- read.csv(files[3])

#let's start with landcover, bc it's most similar to what we already know
  #get a count of how many pixels are in each buffer
  npixels <- landcover %>%
    group_by(ID) %>%
    summarize(numpix = n())
  #load in our nlcd classification information
  nlcd_classif <- read.csv("spatial/nlcd_classifications.csv") %>% mutate(code = as.integer(code))

landtype_bystop <- landcover %>%
  #rename columns dynamically
  rename_with(~ str_extract(.x, "[0-9]{4}"), matches("[0-9]{4}")) %>%
  tidyr::pivot_longer(cols = matches("[0-9]{4}"), names_to = "year", values_to = "nlcd_code") %>%
  group_by(ID, route, stop_num, year, nlcd_code) %>%
  summarize(count = n(), .groups = 'drop') %>%
  left_join(npixels, by = "ID") %>%
  mutate(percent = (count/numpix)*100,
         year = as.integer(year)) %>%
  #add nlcd classification information
  left_join(nlcd_classif, by = c("nlcd_code" = "code"))

  #save .csv
write.csv(landtype_bystop, "data/nlcd-landcover/nlcd_annual_landtype_bystop.csv", row.names = FALSE)


