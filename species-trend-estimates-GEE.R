library(dplyr)
library(lme4)
library(beepr)
library(MASS)
library(geepack)
library(mbbs)
library(broom) #extracts coefficient values out of models, works with geepack

#prevent scientific notation to make a trend table easier to read, if we have one at the end
options(scipen=999)

#read in data
mbbs <- read.csv("data/analysis.df.csv", header = T)
survey.events <- read.csv("data/survey.events.csv", header = T)

#read in data 
mbbs_orange <- mbbs_orange
mbbs_durham <- mbbs_durham
mbbs_chatham <- mbbs_chatham
mbbs <- bind_rows(mbbs_orange, mbbs_chatham, mbbs_durham) %>%
  left_join(mbbs_survey_events, by = c("mbbs_county", "year", "route_num")) %>%
  mutate(route_ID = route_num + case_when(
    mbbs_county == "orange" ~ 100L,
    mbbs_county == "durham" ~ 200L,
    mbbs_county == "chatham" ~ 300L)) %>%
  filter(count > 0) %>%
  ungroup()

#Data structure changes in 2019 to have stop-level records. We need to group together this data for analysis.
mbbs <- mbbs %>%
  group_by(common_name, year, date, route_ID) %>% 
  mutate(count = sum(count)) %>% 
  distinct(common_name, year, date, route_ID, count, .keep_all = TRUE) #keep_all is used to retain all other columns in the dataset %>%
  ungroup() #um, just in case. I don't think this is actually needed.
  
#filter out species that haven't been seen more than the min number of times on the min number of routes. This should become a function that can be called elsewhere. 
occurances <- mbbs %>% ungroup() %>% count(common_name, route_ID) %>% arrange(n) 
min_sightings_per_route <- 10
min_num_routes <- 5
allspecies <- unique(mbbs$common_name)
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
        mbbs <- mbbs %>% filter(common_name != temp_occurances$common_name[1]) #remove species from datatable
      }
      
    } else { #this species hasn't been seen on the minimum number of routes required for analysis
      mbbs <- mbbs %>% filter(common_name != temp_occurances$common_name[1]) #remove species from datatable
    }
  }

n_distinct(mbbs$common_name) #ok, 58 species rn make the cut with the borders set at 5 routes and 10 sightings on those routes. Nice!

#GEE models from GEEpack assume that things are listed in order of cluster
#cluster is the route. that's fine, we can sort in order
mbbs <- mbbs %>% arrange(route_ID, year, common_name)  #also by year and common_name just to improve readability of the datatable
  
#for testing.....
mbbs_test_increase <- mbbs %>% filter(common_name == "Summer Tanager") # should be increasing
mbbs_test_decrease <- mbbs %>% filter(common_name == "Chimney Swift") # should be decreasing
#LOL. Okay, don't, do it that way with the (year):common_name. Something's wrong bc I *know* EVERY species does not have a negative trend. lol, let's make a trend table and run these one by one ok.


#create trend table to store results in
cols <- c("species", "logtrend", "trend", "error", "pvalue", "significant", "Waldtest")
trend_table <- as.data.frame(matrix(ncol = length(cols), nrow = 0))
colnames(trend_table) <- cols
#get species list to filter through
species_list <- unique(mbbs$common_name)
#initaite trend table
trend_table[1:length(species_list),] <- NA


#create variables
current_species <- "NULL BIRD"
filtered_mbbs <- mbbs %>% filter(common_name == "Eastern Bluebird")

#formula we want to run
formula = count ~ year 

#for lop to filter to species, then add species + trend + pvalue + R2 to trend table
  for(s in 1:length(species_list)) {
    
    current_species <- species_list[s]
    
    filtered_mbbs <- mbbs %>% filter(common_name == current_species)
    
    model <- geeglm(formula,
                    family = poisson,
                    id = route_ID,
                    data = filtered_mbbs)
    
    
    trend_table$species[s] <- current_species
    trend_table$logtrend[s] <- summary(model)$coefficients[2,1]
    trend_table$trend[s] <- exp(trend_table$logtrend[s]) - 1
    trend_table$error[s] <- summary(model)$coefficients[2,2]
    trend_table$pvalue[s] <- summary(model)$coefficients[2,4]
    trend_table$Waldtest[s] <- summary(model)$coefficients[2,3]
    trend_table$significant[s] <- case_when(trend_table$pvalue[s] <= 0.05 ~ 1,
                                            trend_table$pvalue[s] > 0.05 ~ 0)
  }


#export trend table
write.csv(trend_table, "data/trend-table-GEE.csv", row.names = F)
