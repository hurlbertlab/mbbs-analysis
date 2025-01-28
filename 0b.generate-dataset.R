#---
#Todo: create dataset

#what this does
#merges mbbs counties to mbbs all
#creates survey.event dataset
#removes species with less than 10 sightings 
#FUTURE: removes observations flaged by the mbbs for like, observer only participated once, too high/too low species counts, other things we talked about flagging etc. 

#TODO: Change from route_num unique format - preserve original route numbers and just group_by both county and route_num when working with the dataset. That's a better way to do things

#TODO: 2025-01-28 This should really be deleted with the new workflow
#---

#load mbbs library
library(mbbs)
library(dplyr)
library(stringr)

#bring datasets into the local environment
mbbs_orange <- mbbs_orange
mbbs_durham <- mbbs_durham
mbbs_chatham <- mbbs_chatham

#create mbbs_all
mbbs_all <- bind_rows(mbbs_orange, mbbs_chatham, mbbs_durham)

#Fix potential error sources
#keep route_num, but add a route variable that has a unique route for each county. 
#let's actually REMOVE the county column so it doesn't get confusing, becuase mbbs_county is always correct and the county column from ebird is sometimes not (ie: routes right next to Wake county that ebird sorts into Wake)
mbbs_all <- mbbs_all %>%
  mutate(
    # The route number is not unique within the study
    # (only within a county).
    route_ID = route_num + case_when(
      mbbs_county == "orange" ~ 100L,
      mbbs_county == "durham" ~ 200L,
      mbbs_county == "chatham" ~ 300L,
    )
  )

#survey events is now part of the mbbs dataset, no need to create it here


#Data structure changes in 2019 to have stop-level records. We need to group together this data for analysis at the route level.
mbbs_all <- mbbs_all %>%
  group_by(common_name, year, date, route_ID) %>% 
  mutate(count = sum(count)) %>% 
  distinct(common_name, year, date, route_ID, count, .keep_all = TRUE) %>% #keep_all is used to retain all other columns in the dataset %>%
  ungroup() #just in case. I don't think this is actually needed.


#filter out species that don't show up in enough years
#filter out species that haven't been seen more than the min number of times on the min number of routes. This should become a function that can be called elsewhere. 
occurances <- mbbs_all %>% ungroup() %>% count(common_name, route_ID) %>% arrange(n) 
min_sightings_per_route <- 10
min_num_routes <- 5
allspecies <- unique(mbbs_all$common_name)
temp_occurances <- occurances %>% filter(common_name == "Acadian Flycatcher") #temp for use in for loop
temp_num <- n_distinct(temp_occurances$route_ID) #for use in for loop, really this is also the nrow(temp_occurances) but that's ok. 

#for loop to filter species that haven't been seen enough, the minimum number of times on a minimum number of routes 
for (s in 1:length(allspecies)) {
  
  temp_occurances <- occurances %>% filter(common_name == allspecies[s])
  temp_num <- n_distinct(temp_occurances$route_ID)
  
  if(temp_num >= min_num_routes) { #this species has been seen on the minimum number of routes
    #check that the species has been seen the minimum number of TIMES on those routes
    #so, count the n values over min_sightings_per_route
    temp_num <- sum(temp_occurances$n >= min_sightings_per_route)
    if(temp_num > min_num_routes) {
      #do nothing, the species meet the minimum sighting requirements and should stay in the route
    } else {
      #the species does not meet the minimum sighting requirements and should be removed from analysis
      mbbs_all <- mbbs_all %>% filter(common_name != temp_occurances$common_name[1]) #remove species from datatable
    }
    
  } else { #this species hasn't been seen on the minimum number of routes required for analysis
    mbbs_all <- mbbs_all %>% filter(common_name != temp_occurances$common_name[1]) #remove species from datatable
  }
}

n_distinct(mbbs_all$common_name) #ok, 58 species rn make the cut with the borders set at 5 routes and 10 sightings on those routes. Nice!


#now apply the species and observer filters...
#remove routes with problems
#orange rt 11 in 2012


#save dataset 
write.csv(mbbs_all, "data/route_analysis_df.csv", row.names = F)
#save backup dataset
write.csv(mbbs_all, file = sprintf(
  "data/versioncontrol/analysis.df_%s.csv",
  format(Sys.Date(), "%Y%m%d")
))
