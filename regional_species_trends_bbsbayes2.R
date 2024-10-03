#---------------------------------------
# Get the regional Piedmont trends for all species
# Uses bbsbayes2
#---------------------------------------

#install.packages("bbsBayes2",
#                 repos = c(bbsbayes = 'https://bbsbayes.r-universe.dev',
#                           CRAN = 'https://cloud.r-project.org'))

library(bbsBayes2)
library(mbbs)
library(dplyr)
library(stringr)
#get the functions we need for this script that are stored elsewhere
source("species-trend-estimate-functions.R")

#read in data, using most updated versions of the mbbs. 
mbbs_orange <- mbbs_orange %>% standardize_year(starting_year = 2000)
mbbs_durham <- mbbs_durham %>% standardize_year(starting_year = 2000)
mbbs_chatham <- mbbs_chatham %>% standardize_year(starting_year = 2000)
mbbs <- bind_rows(mbbs_orange, mbbs_chatham, mbbs_durham) %>% #bind all three counties together
  mutate(route_ID = route_num + case_when(
    mbbs_county == "orange" ~ 100L,
    mbbs_county == "durham" ~ 200L,
    mbbs_county == "chatham" ~ 300L)) %>%
  filter(count > 0) %>%
  ungroup() %>%
  #remove the 1999 data, since it's only 1/3 of the routes that were created by then. 
  filter(year > 1999) %>%
  #clean up for ease of use, lots of columns we don't need rn
  dplyr::select(-sub_id, -tax_order, -count_raw, -state, -loc, -locid, -lat, -lon, -protocol, -distance_traveled, -area_covered, -all_obs, -breed_code, -checklist_comments, -source) %>%
  #create a route-standard from 1-34
  group_by(route_ID) %>%
  mutate(route_standard = dplyr::cur_group_id()) %>%
  ungroup()
#help keep the environment clean
rm(mbbs_chatham); rm(mbbs_durham); rm(mbbs_orange) 

#filter to the species with enough sightings to be used in our analysis
mbbs <- filter_to_min_sightings(mbbs, 9, 5)

#get mbbs species list
species_list <- unique(mbbs$common_name)
#filter out black vulture, poorly sampled by this method


#run bbsbayes for the Bird Conservation Region -- Piedmont region for the species in the list,
#trends from 2000->2024.
