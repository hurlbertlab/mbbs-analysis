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
  ungroup() 

#Data structure changes in 2019 to have stop-level records. We need to group together this data for analysis. We cannot use date here or it causes problems with future analysis. 
mbbs <- mbbs %>%
  group_by(common_name, year, route_ID) %>% 
  mutate(count = sum(count)) %>% 
  distinct(common_name, year, route_ID, count, .keep_all = TRUE) %>% #keep_all is used to retain all other columns in the dataset
ungroup() #um, just in case. I don't think this is actually needed.

#filter out species that haven't been seen more than the min number of times on the min number of routes.
mbbs <- filter_to_min_sightings(mbbs, min_sightings_per_route = 10, min_num_routes = 5)

n_distinct(mbbs$common_name) #ok, 58 species rn make the cut with the borders set at 5 routes and 10 sightings on those routes. Nice!

#add in the 0 values for when routes were surveyed but the species that remain in this filtered dataset were not seen.
 mbbs <- mbbs %>% complete(
  nesting(year, mbbs_county, route_ID, route_num),
  nesting(common_name, sci_name),
  fill = list(count = 0))  

#now that everything else is ready to go, leftjoin survey_events so we have observer information
mbbs <- mbbs %>% 
  left_join(mbbs_survey_events, by = c("mbbs_county", "year", "route_num"))
  
# Remove Orange route 11 from 2012 due to uncharacteristically high counts from a one-time observer
mbbs <- mbbs %>% filter(primary_observer != "Ali Iyoob")

#leftjoin for landcover information
nlcd <- read.csv("data/landtype_byroute.csv", header = TRUE)
##heads up, nlcd data is missing years, u need to assign it the last value, only change when a new yr that has new data happens. Must be a way to do that easily without writing a whole complicated function..
mbbs <- mbbs %>%
  left_join()

#------------------------------------------------------------------------------
#poisson model
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
#negative binomial model
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
#GEE model
#------------------------------------------------------------------------------


