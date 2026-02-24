#########################
#
# Take the mbbs dataset
# and apply anything needed
# to create our dataset
#
# eg: mostly removing species that
# are not observed enough to be used
# in the analysis
#
#########################

#load libraries
library(dplyr)
library(tidyr)
library(stringr)
library(beepr)

#get the functions we need for this script that are stored elsewhere
source("2.analysis-functions.R")

#load in survey events and calc. observer quality
mbbs_survey_events <- read.csv("data/mbbs/surveys.csv") %>%
  get_observer_quality() %>%
  select(route, year, primary_observer, standardized_observers, nstops, observer_quality, observer_ID)

#read in the mbbs route data
#this already includes the 0s for when species are not observed.
mbbs <- read.csv("data/mbbs/mbbs_route_counts.csv") %>%
  #remove the 1999 data, since only 1/3 of the routes were created by then
  filter(year > 1999) %>%
  standardize_year(starting_year = 2000) %>%
  #create a route-standard from 1-34
  group_by(route) %>%
  mutate(route_standard = dplyr::cur_group_id()) %>%
  ungroup()


#filter out species that haven't been seen more than the min number of times on the min number of routes.
#9 to include bobwhite!
mbbs <- filter_to_min_sightings(mbbs, 
                                min_sightings_per_route = 9,
                                min_num_routes = 5) %>%
  #filter out waterbirds and hawks
  filter(!common_name %in% excluded_species) %>%
  #create a variable so that has common name from 1:however many species are 
  #included
  group_by(common_name) %>%
  mutate(common_name_standard = dplyr::cur_group_id()) %>%
  ungroup() 

n_distinct(mbbs$common_name) #61

#OK. The mbbs has zeros already added, but we've gone and removed some by filtering the species that have only been seen on minimum n routes. So we'll add in the 0 values for when routes were surveyed but the species that remain in this filtered dataset were not seen.
mbbs <- mbbs %>% complete(
  nesting(year, year_standard, county, route, route_num, route_standard),
  nesting(common_name, sci_name, common_name_standard),
  fill = list(count = 0))  

#check that no issues have occurred - all species should have the same number of occurences in the df 
if(length(unique(table(mbbs$common_name))) == 1) { #all species have the same number of occurances
  print("All species have same number of occurances, good to continue")
  beep()
} else {
  print("ERROR: Adding in the 0 values has led to some species having more occurances than others."); beep(11);}

#now that everything else is ready to go, leftjoin survey_events so we have observer information
mbbs <- add_survey_events(mbbs, mbbs_survey_events)

#save df as our analysis df
write.csv(mbbs, "data/analysis.df.csv", row.names = FALSE)
