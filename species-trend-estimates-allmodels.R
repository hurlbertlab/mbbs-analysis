#file to have all the trend estimate modeling on one file, ideally just adding everything to one nice datasheet as I go

#get the functions we need for this script that are stored elsewhere
source("species-trend-estimate-functions.R")

#libraries
library(beepr)
library(dplyr)
library(tidyr)
library(lme4)
library(beepr)
library(MASS)
library(geepack)
library(mbbs)
library(broom) #extracts coefficient values out of models, works with geepack

#prevent scientific notation to make a trend table easier to read
options(scipen=999)

#read in data
mbbs <- read.csv("data/analysis.df.csv", header = T)
survey_events <- mbbs_survey_events

#unneeded mbbs columns
remove <- c("sub_id", "tax_order", "count_raw", "state", "loc", "locid", "lat", "lon", "protocol", "distance_traveled", "area_covered","all_obs", "breed_code")

#read in data, using most updated versions of the mbbs. 
mbbs_orange <- mbbs_orange
mbbs_durham <- mbbs_durham
mbbs_chatham <- mbbs_chatham
mbbs <- bind_rows(mbbs_orange, mbbs_chatham, mbbs_durham) %>% #bind all three counties together
  mutate(route_ID = route_num + case_when(
    mbbs_county == "orange" ~ 100L,
    mbbs_county == "durham" ~ 200L,
    mbbs_county == "chatham" ~ 300L)) %>%
  filter(count > 0) %>%
  ungroup() %>%
  dplyr::select(-sub_id, -tax_order, -count_raw, -state, -loc, -locid, -lat, -lon, -protocol, -distance_traveled, -area_covered, -all_obs, -breed_code, -checklist_comments, -source)
#help keep the environment clean
rm(mbbs_chatham); rm(mbbs_durham); rm(mbbs_orange) 

#Data structure changes in 2019 to have stop-level records. We need to group together this data for analysis. We cannot use date here or it causes problems with future analysis. 
mbbs <- mbbs %>%
  group_by(common_name, year, route_ID) %>% 
  mutate(count = sum(count)) %>% 
  distinct(common_name, year, route_ID, count, .keep_all = TRUE) %>% #keep_all is used to retain all other columns in the dataset
ungroup() #this ungroup is necessary 

#filter out species that haven't been seen more than the min number of times on the min number of routes.
mbbs <- filter_to_min_sightings(mbbs, min_sightings_per_route = 10, min_num_routes = 5)

n_distinct(mbbs$common_name) #ok, 58 species rn make the cut with the borders set at 5 routes and 10 sightings on those routes. Nice!

#add in the 0 values for when routes were surveyed but the species that remain in this filtered dataset were not seen.
 mbbs <- mbbs %>% complete(
  nesting(year, mbbs_county, route_ID, route_num),
  nesting(common_name, sci_name),
  fill = list(count = 0))  

#check that no issues have occurred - all species should have the same number of occurences in the df 
 if(length(unique(table(mbbs$common_name))) == 1) { #all species have the same number of occurances
   print("All species have same number of occurances, good to continue")
   beep()
 } else {
     print("ERROR: Adding in the 0 values has led to some species having more occurances than others."); beep(11); beep(11)}
 
#now that everything else is ready to go, leftjoin survey_events so we have observer information
mbbs <- mbbs %>% 
  left_join(mbbs_survey_events, by = c("mbbs_county", "year", "route_num")) %>%
  dplyr::select(-observers.x) %>%
  rename(observers = observers.y)
  
# Remove Orange route 11 from 2012 due to uncharacteristically high counts from a one-time observer
mbbs <- mbbs %>% filter(primary_observer != "Ali Iyoob")

#leftjoin for landcover information
#read in nlcd data, filter to just what we're interested in. Otherwise it's a many-to-many join relationship. Right now, just the % developed land. Workflow similar to "calc_freq_remove_rows()" from the generate_percent_change_+_map.. code, but altered for this use.
nlcd <- read.csv("data/landtype_byroute.csv", header = TRUE) %>%
  #filter to nlcd class of interest, here, my classification of all 'developed'
  filter(ijbg_class == "developed") %>%
  #group_by, because calc_freq_remove_rows requested an already grouped dataset
  group_by(mbbs_county, year, route_num, ijbg_class, totpix) %>%
  transmute(frequency = sum(frequency)) %>%
  distinct() %>% #remove now extraneous rows
  mutate(percent_developed = (frequency/totpix) * 100,
         year = as.integer(year)) %>%
  ungroup() 

  
  #need to now combine all the % developed land into one row for each county/route/year
##heads up, nlcd data is missing years, u need to assign it the last value, only change when a new yr that has new data happens. Must be a way to do that easily without writing a whole complicated function..
mbbs <- mbbs %>%
  left_join(nlcd, by = c("mbbs_county", "year", "route_num")) %>%
  group_by(route_ID, common_name) %>%
  arrange(common_name, route_ID, year) %>%
  relocate(percent_developed, .before = common_name) %>%
  fill(percent_developed, .direction = "downup")
#check <- mbbs %>% filter(route_ID == 106)

#------------------------------------------------------------------------------

#set up for modeling
species_list <- unique(mbbs$common_name)
filtered_mbbs <- mbbs %>% filter(common_name == species_list[1])

#create trend table to store results in
cols <- c("species")
trend_table <- as.data.frame(matrix(ncol = length(cols), nrow = 0))
colnames(trend_table) <- cols
#initiate the trend table, fill with NAs
  trend_table[1:length(species_list),] <- NA
#add in the species
  for(s in 1:length(species_list)) {
    
    trend_table$species[s] <- species_list[s]
    
  }

  
  formula_simple <- count ~ year + percent_developed
  formula_randomeffects <- count ~ year + percent_developed + (1|primary_observer)
  
  #nah babe, just make each model have it's own table and left_join them to the trend_table based on "species"
  #"pois_estimate", "pois_error", "pois_significant", "pois_percdev_estimate", "pois_percdev_significant", "gee_estimate", "gee_error", "gee_significant", "gee_percdev_estimate", "gee_percdev_significant", "gee_observer_estimate"
#------------------------------------------------------------------------------
#poisson model
#------------------------------------------------------------------------------

#issues with glmer poisson - I'm getting a "model failed to converge" when I include the random effect of observers, "Model is nearly unidentifiable: very large eigenvalue" - this is using primary_observer (58 possible random variations)
  #sample_model <- glmer(count ~ year + percent_developed + (1 | observer_ID), data = filtered_mbbs, family = "poisson")
  
  for(s in 1:length(species_list)) {
    
    current_species <- species_list[s]
    
    filtered_mbbs <- mbbs %>% filter(common_name == current_species)
    
    model <- glm(formula_simple, family = "poisson", data = filtered_mbbs)
    
    tidied <- tidy(model)
    
    #add to trend table
    trend_table <- 
      trend_table %>%
      mutate(
        pois_estimate = ifelse(species == current_species, tidied$estimate[1], pois_estimate)
      )
    trend_table[trend_table$species == current_species,]$pois_error
  }

sample_model <- glm(count ~ year + percent_developed, data = filtered_mbbs, family = "poisson")
temp <- tidy(sample_model)

yeartemp <- temp %>% filter(term == "year")
yeartemp$estimate
  
  model <- glm(count ~ year + (1|route_ID) + (1|observer_ID), data = filtered.mbbs, family="poisson")

#------------------------------------------------------------------------------
#negative binomial model
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
#GEE model  
#------------------------------------------------------------------------------


