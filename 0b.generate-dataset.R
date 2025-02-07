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
source("2.species-trend-estimate-functions.R")

#load in survey events
#we load in a version where we've created observer quality.
load("C:/git/mbbs-analysis/data/mbbs/mbbs_survey_events.rda")
mbbs_survey_events <- mbbs_survey_events %>%
  #add updated route id style
  mutate(route = make_route(mbbs_county, route_num)) %>%
  dplyr::relocate(route, .before = mbbs_county) %>%
  #give primary observer to whoever had the highest max quality
  mutate(primary_observer = case_when(max_qual_observer == 1 ~ obs1,
                                      max_qual_observer == 2 ~ obs2,
                                      max_qual_observer == 3 ~ obs3)) %>%
  group_by(primary_observer) %>%
  mutate(observer_ID = cur_group_id()) %>%
  ungroup() %>%
  #select just the variables we need
  dplyr::select(route, mbbs_county, route_num, year, primary_observer, max_qual_observer, observer_ID, observer_quality)

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

#OK. The mbbs has zeros already added, but we've gone and removed some by filtering the species that have only been seen on minimun n routes. So we'll add in the 0 values for when routes were surveyed but the species that remain in this filtered dataset were not seen.
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
