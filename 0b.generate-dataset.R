#---
#Todo: create dataset

#what this does
#merges mbbs counties to mbbs all
#creates survey.event dataset
#removes species with less than 10 sightings 
#FUTURE: removes observations flaged by the mbbs for like, observer only participated once, too high/too low species counts, other things we talked about flagging etc. 

#TODO: Change from route_num unique format - preserve original route numbers and just group_by both county and route_num when working with the dataset. That's a better way to do things

#TODO: 2025-01-28 This should really be deleted with the new workflow, or is going to be altered a lot in the future with using the new mbbs format dataset, that's already grouped to route etc.
#---

#load mbbs library
library(mbbs)
library(dplyr)
library(stringr)
#get the functions we need for this script that are stored elsewhere
source("species-trend-estimate-functions.R")

#get survey events from temporary load instead of from the current github version
#of the mbbs.
load("C:/git/mbbs-analysis/data/mbbs_survey_events.rda")
mbbs_survey_events <- mbbs_survey_events %>%
  mutate(primary_observer = case_when(max_qual_observer == 1 ~ obs1,
                                      max_qual_observer == 2 ~ obs2,
                                      max_qual_observer == 3 ~ obs3)) %>%
  group_by(primary_observer) %>%
  mutate(observer_ID = cur_group_id()) %>%
  ungroup()
#max_qual obs = 1, gets first observer,
#where max qual obs =2, gets second observer,
#where max qual obs =2 gets third observer

#set up a pipe to make it so that the max_qual_obs (contains a 1,2,3) becomes
#the primary obs


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
#load in mbbs_survey_events from current branch (stop_num)
#load("C:/git/mbbs/data/mbbs_survey_events.rda")

#Data structure changes in 2019 to have stop-level records. We need to group together this data for analysis. We cannot use date here or it causes problems with future analysis. 
mbbs <- mbbs %>%
  group_by(common_name, year, route_ID) %>% 
  mutate(count = sum(count)) %>% 
  distinct(common_name, year, route_ID, count, .keep_all = TRUE) %>% #keep_all is used to retain all other columns in the dataset
  ungroup() #this ungroup is necessary 

#filter out species that haven't been seen more than the min number of times on the min number of routes.
#9 to include bobwhite!
mbbs <- filter_to_min_sightings(mbbs, min_sightings_per_route = 9, min_num_routes = 5) %>%
  #filter out waterbirds and hawks
  filter(!common_name %in% excluded_species) %>%
  #create a variable so that has common name from 1:however many species are 
  #included
  group_by(common_name) %>%
  mutate(common_name_standard = dplyr::cur_group_id()) %>%
  ungroup() 

n_distinct(mbbs$common_name) #ok, 56 species rn make the cut with the borders set at 5 routes and 9 sightings on those routes. Nice!

#save a version of the mbbs before adding 0s
mbbs_nozero <- mbbs
#add in the 0 values for when routes were surveyed but the species that remain in this filtered dataset were not seen.
mbbs <- mbbs %>% complete(
  nesting(year, year_standard, mbbs_county, route_ID, route_num, route_standard),
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
mbbs_nozero <- add_survey_events(mbbs_nozero, mbbs_survey_events)

# Remove Orange route 11 from 2012 due to uncharacteristically high counts from a one-time observer
#! In newer versions of the mbbs (that have to be downloaded from the website due to new data handling processes, this checklist is already removed as a data filtering step.)
mbbs <- mbbs %>% dplyr::filter(!primary_observer %in% "Ali Iyoob") %>%
  group_by(primary_observer) %>%
  mutate(observer_ID = cur_group_id()) %>%
  ungroup()
mbbs_nozero <- mbbs_nozero  %>% dplyr::filter(!primary_observer %in% "Ali Iyoob")

#save copies of analysis df.
write.csv(mbbs, "data/analysis.df.csv", row.names = FALSE)
write.csv(mbbs_nozero, "data/analysis.df.nozero.csv", row.names = FALSE)