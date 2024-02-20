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

#save a version of the mbbs before adding 0s
  mbbs_nozero <- mbbs
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
mbbs <- add_survey_events(mbbs, survey_events)
mbbs_nozero <- add_survey_events(mbbs_nozero, survey_events)
  
# Remove Orange route 11 from 2012 due to uncharacteristically high counts from a one-time observer
#current giving an error where it sets mbbs to have 0 obs, so, that needs a fix
#mbbs <- mbbs %>% dplyr::filter(primary_observer != "Ali Iyoob")
#mbbs_nozero <- mbbs_nozero  %>% dplyr::filter(primary_observer != "Ali Iyoob")
#check <- mbbs %>% filter(!primary_observer %in% "Ali Iyoob")

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
add_nlcd <- function(mbbs, nlcd) {
  mbbs <- mbbs %>%
    left_join(nlcd, by = c("mbbs_county", "year", "route_num")) %>%
    group_by(route_ID, common_name) %>%
    arrange(common_name, route_ID, year) %>%
    relocate(percent_developed, .before = common_name) %>%
    fill(percent_developed, .direction = "downup")
  #check <- mbbs %>% filter(route_ID == 106)
  return(mbbs)
}
mbbs <- add_nlcd(mbbs, nlcd)
mbbs_nozero <- add_nlcd(mbbs_nozero, nlcd)

#------------------------------------------------------------------------------

#set up for modeling
species_list <- unique(mbbs$common_name)
filtered_mbbs <- mbbs %>% filter(common_name == species_list[1])

#create trend table to store results in
cols_list <- c("common_name")
trend_table <- make_trend_table(cols_list)
  #add in the species
  for(s in 1:length(species_list)) {
    
    trend_table[s,1] <- species_list[s]
    
  }

mbbs <- mbbs %>% mutate(route_ID = as.factor(route_ID))
mbbs_nozero <- mbbs_nozero %>% mutate(route_ID = as.factor(route_ID))

#formulas to plug into the models
  formula_basic <- count ~ year
  formula_simple <- count ~ year + percent_developed
  formula_randomeffects <- count ~ year  + (1|route_ID)
  formula_rankedobserver <- count ~ year + observer_quality
  
#------------------------------------------------------------------------------
#poisson model
#------------------------------------------------------------------------------

#issues with glmer poisson - I'm getting a "model failed to converge" when I include the random effect of observers, "Model is nearly unidentifiable: very large eigenvalue" - this is using primary_observer (58 possible random variations)
  #sample_model <- glmer(count ~ year + percent_developed + (1 | observer_ID), data = filtered_mbbs, family = "poisson")
  
#a poisson model is not meant for a version that has the added in zeros. so we will use the mbbs without added zeros
  
  pois_table <- make_trend_table(c("common_name","pois_estimate", "pois_trend", "pois_error", "pois_significant", "pois_percdev_estimate", "pois_percdev_trend", "pois_percdev_significant"))
  
  for(s in 1:length(species_list)) {
    
    current_species <- species_list[s]
    
    filtered_mbbs <- mbbs_nozero %>% filter(common_name == current_species)
    
    model <- glmer(formula_randomeffects, family = "poisson", data = filtered_mbbs)
    
    #tidied <- tidy(model)
    
    #add to trend table
    pois_table[s,1] <- current_species
    pois_table$pois_estimate[s] <- summary(model)$coefficients[2,1]
    #pois_table$pois_estimate[s] <- tidied$estimate[2]
    #pois_table$pois_trend[s] <- exp(pois_table$pois_estimate[s]) -1
    #pois_table$pois_error[s] <- tidied$std.error[2]
    #pois_table$pois_significant[s] <- case_when(tidied$p.value[2] <= 0.05 ~ 1,
#                                                tidied$p.value[2] > 0.05 ~ 0)
    #pois_table$pois_percdev_estimate[s] <- tidied$estimate[3]
    #pois_table$pois_percdev_trend[s] <- exp(pois_table$pois_percdev_estimate[s]) -1
    # pois_table$pois_percdev_significant[s] <- case_when(tidied$p.value[3] <= 0.05 ~ 1,
    #                                                     tidied$p.value[3] > 0.05 ~ 0)
  }

  trend_table <- left_join(trend_table, pois_table, by = "common_name")

#------------------------------------------------------------------------------
#negative binomial model
#------------------------------------------------------------------------------
  
  #I think not necessary, but reserve the space.
  negb_table <- make_trend_table(c("common_name","negb_estimate", "negb_trend", "negb_error", "negb_significant", "negb_percdev_estimate", "negb_percdev_trend", "negb_percdev_significant"))
  
#------------------------------------------------------------------------------
#GEE model  
#------------------------------------------------------------------------------
  #GEE models from GEEpack assume that things are listed in order of cluster
  #cluster is the route. that's fine, we can sort in order
  mbbs <- mbbs %>% arrange(route_ID, year, common_name)  #also by year and common_name just to improve readability of the datatable

    gee_table <- make_trend_table(c("common_name", "gee_estimate", "gee_trend","gee_error", "gee_significant", "gee_percdev_estimate", "gee_percdev_significant", "gee_observer_estimate", "gee_Waldtest"))
    
    #add in the species
    for(s in 1:length(species_list)) {
      
      gee_table[s,1] <- species_list[s]
      
    }
    
    for(s in 1:length(species_list)) {
      
      current_species <- species_list[s]
      
      filtered_mbbs <- mbbs %>% filter(common_name == current_species)
      
      model <- geeglm(formula_simple,
                      family = poisson,
                      id = route_ID,
                      data = filtered_mbbs)
      
      tidied <- tidy(model)
      
      gee_table$common_name[s] <- current_species
      gee_table$gee_estimate[s] <- tidied$estimate[2]
      gee_table$gee_trend[s] <- exp(gee_table$gee_estimate[s]) -1
      gee_table$gee_error[s] <- tidied$std.error[2]
      gee_table$gee_significant[s] <- case_when(tidied$p.value[2] <= 0.05 ~ 1,
                                             tidied$p.value[2] > 0.05 ~ 0)
      gee_table$gee_percdev_estimate[s] <- tidied$estimate[3]
      gee_table$gee_percdev_significant[s] <- case_when(tidied$p.value[3] <= 0.05 ~ 1,
                                                        tidied$p.value[3] > 0.05 ~ 0)
      gee_table$gee_observer_estimate[s] <- NA
      gee_table$gee_Waldtest[s] <- summary(model)$coefficients[2,3]
    }

    trend_table <- left_join(trend_table, gee_table, by = "common_name")
    
    
#------------------------------------------
    plot(trend_table$pois_estimate, trend_table$gee_estimate)
    abline(a=0,b=1)
    text(trend_table$pois_estimate, trend_table$gee_estimate, labels = trend_table$common_name, pos = 4, cex = 0.8, col = "darkblue")    
    
    plot(trend_table$pois_percdev_estimate, trend_table$gee_percdev_estimate)
    abline(a=0,b=1)
    
    plot(trend_table$pois_error.x, trend_table$pois_error.y)
    abline(a=0,b=1)
    