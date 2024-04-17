#file to have all the trend estimate modeling on one file, ideally just adding everything to one nice datasheet as I go

#get the functions we need for this script that are stored elsewhere
source("species-trend-estimate-functions.R")

#libraries
library(beepr) #beeps
library(dplyr) #data manip
library(tidyr) #data manip
library(lme4) #modeling
library(MASS) #modeling
library(geepack) #GEE modeling
library(mbbs) #data comes from here
library(broom) #extracts coefficient values out of models, works with geepack

#prevent scientific notation to make a trend table easier to read
options(scipen=999)

#read in data - not in use right now. Data workflow where I'm making an analysis.df needs a reassessment
#mbbs <- read.csv("data/analysis.df.csv", header = T)
#survey_events <- mbbs_survey_events

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
  #clean up for ease of use, lots of columns we don't need rn
  dplyr::select(-sub_id, -tax_order, -count_raw, -state, -loc, -locid, -lat, -lon, -protocol, -distance_traveled, -area_covered, -all_obs, -breed_code, -checklist_comments, -source)
#help keep the environment clean
rm(mbbs_chatham); rm(mbbs_durham); rm(mbbs_orange) 
#load in mbbs_survey_events from current branch (stop_num)
load("C:/git/mbbs/data/mbbs_survey_events.rda")

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
     print("ERROR: Adding in the 0 values has led to some species having more occurances than others."); beep(11);}
 
#now that everything else is ready to go, leftjoin survey_events so we have observer information
mbbs <- add_survey_events(mbbs, mbbs_survey_events)
mbbs_nozero <- add_survey_events(mbbs_nozero, mbbs_survey_events)
  
# Remove Orange route 11 from 2012 due to uncharacteristically high counts from a one-time observer
mbbs <- mbbs %>% dplyr::filter(!primary_observer %in% "Ali Iyoob")
mbbs_nozero <- mbbs_nozero  %>% dplyr::filter(!primary_observer %in% "Ali Iyoob")


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

#read in landfire information
landfire <- read.csv("data/landfire_byroute.csv", header = TRUE) %>%
  #right now, I'm only concerned here with means. So I want to just have one bit of information for each route, we don't need to keep all the different percent landtypes. If we wanted to do that, we could pivot wider based on landtype and get the % frequency for each category.
  distinct(mbbs_county, route_num, lf_mean_route, lf_median_route, lf_year, lf_q3_route, lf_difmean_22_16) 

landfire$mbbs_county <- str_to_lower(landfire$mbbs_county)

  
  #need to now combine all the % developed land into one row for each county/route/year
##heads up, nlcd data is missing years, assigns it the last value, only change when a new yr that has new data happens. 
add_nlcd <- function(mbbs, nlcd) {
  mbbs <- mbbs %>%
    left_join(nlcd, by = c("mbbs_county", "year", "route_num")) %>%
    group_by(route_ID, common_name) %>%
    arrange(common_name, route_ID, year) %>%
    tidyr::fill(percent_developed, .direction = "downup")
  #check <- mbbs %>% filter(route_ID == 106)
  return(mbbs)
}
mbbs <- add_nlcd(mbbs, nlcd)
mbbs_nozero <- add_nlcd(mbbs_nozero, nlcd)

add_landfire <- function(mbbs, landfire) {
  mbbs <- mbbs %>% 
    left_join(landfire, by = c("mbbs_county", "route_num", "year" = "lf_year")) %>%
    group_by(route_ID, common_name) %>%
    arrange(common_name, route_ID, year) %>%
    tidyr::fill(lf_mean_route, lf_median_route, lf_q3_route, lf_difmean_22_16, .direction = "downup")
}

mbbs <- add_landfire(mbbs,landfire)
mbbs_nozero <- add_landfire(mbbs_nozero, landfire)

#------------------------------------------------------------------------------

#set up for modeling
species_list <- unique(mbbs$common_name)
filtered_mbbs <- mbbs %>% filter(common_name == species_list[1])

#create trend table to store results in
cols_list <- c("common_name")
trend_table <- make_trend_table(cols_list, species_list)

#make route_ID a factor
mbbs <- mbbs %>% mutate(route_ID = as.factor(route_ID))
mbbs_nozero <- mbbs_nozero %>% mutate(route_ID = as.factor(route_ID))

#add in UAI
uai <- read.csv("data/UAIs_NCetall.csv") %>%
  filter(City == "Charlotte_US")

mbbs <- mbbs %>%
  left_join(uai, by = c("common_name" = "Species"))

#formulas to plug into the models
  f_base <- count ~ year  
  f_pd <- update(f_base, ~ . + percent_developed)
  f_obs <- update(f_base, ~ . + observer_quality)
  f_pdobs <- update(f_base, ~ . + percent_developed + observer_quality)
  f_wlandfire <- update(f_pdobs, ~. + lf_mean_route)

#------------------------------------------------------------------------------
#GEE model  
#------------------------------------------------------------------------------
  #GEE models from GEEpack assume that things are listed in order of cluster
  #cluster is the route. that's fine, we can sort in order
  mbbs <- mbbs %>% arrange(route_ID, year, common_name)  #also by year and common_name just to improve readability of the datatable

    gee_table <- make_trend_table(cols_list = c("common_name", "gee_estimate", "gee_trend","gee_error", "gee_significant", "gee_percdev_estimate", "gee_percdev_significant", "gee_observer_estimate", "gee_Waldtest"),
                                  rows_list = species_list)
  
  #let's try another way of having the trend table. 
  pivot_tidied <- function(model, current_species) {
    
    #tidy up the model
    tidied <- tidy(model) %>%
      pivot_longer(cols = term) %>% 
      mutate(common_name = current_species) %>% #add species column
      relocate(c(common_name, value), .before = estimate)
    
    return(tidied)
    
  }
  
  
  run_gee <- function(formula, mbbs, species_list) {
    
    #make first rows of trend table based off the first species
    current_species <- species_list[1]
    filtered_mbbs <- mbbs %>% filter(common_name == current_species)
    model <- geeglm(formula,
                    family = poisson,
                    id = route_ID,
                    data = filtered_mbbs,
                    corstr = "ar1")
    gee_table <- pivot_tidied(model, current_species)
    
    #do the same thing to the rest of the species list, and rbind those rows together
    for(s in 2:length(species_list)) {
      
      current_species <- species_list[s]
      
      filtered_mbbs <- mbbs %>% filter(common_name == current_species)
      
      model <- geeglm(formula,
                      family = poisson,
                      id = route_ID,
                      data = filtered_mbbs,
                      corstr = "ar1")
      #rbind to gee_table
      gee_table <- rbind(gee_table, pivot_tidied(model, current_species))
      
    }
    
    return(gee_table)
  }
  
  output <- run_gee(formula = 
                     update(f_base, ~ .+ observer_quality),
                   mbbs, species_list) %>%
    #remove intercept information
    filter(!value %in% "(Intercept)") %>%
    #add trend into (exponentiate the esimate)
    mutate(trend = exp(estimate)-1) %>%
    #pivot_wider
    pivot_wider(names_from = value, values_from = c(estimate, std.error, statistic, p.value, trend)) %>%
    dplyr::select(-name)%>%
    left_join(uai, by = c("common_name" = "Species"))
  
  #---------------------------------bsft---------------------------------------
  output$trend_year <- output$trend_year*100
  output$std.error_year <- output$std.error_year*100
  
  output <- output %>% arrange(trend_year)
  
  hist(output$trend_year, breaks = 15)
  
  colors <- ifelse(output$p.value_year >= 0.05, "skyblue", "white")
  
  mid <- barplot(output$trend_year)
  barplot(height = output$trend_year,
         # names.arg = output$common_name,
         #percent_change - 2 * standard_deviation, percent_change + 2 * standard_deviation),  # Adjust ylim to include error bars,
          #main = "Species Yearly Trends",
          xlab = "Bird Species",
         ylab = "% Yearly Change",
         ylim = c(-7,7),
         col = "skyblue", #if you want based on significance use colors variable above
         border = "black")
  #arrows(x0 = mid, y0 = output$trend_year + output$std.error_year,
  #       x1 = mid, y1 = output$trend_year - output$std.error_year,
  #       angle = 90, code =3, length = 0.01)
  
  
  #okay. um. some start to visuals for this presentation..
  
  #meantime. let's do migratory distance and diet etc. predictions. left_join traits
  traits <- read.csv("data/NC_species_traits.csv", header = TRUE)
  output <- output %>%
    left_join(traits, by = c("common_name" = "english_common_name"))
  
  #hey, Ivara, this is the wrong way to analyze this data. Rather than fitting a linear line to a category vs trend, this is a t-test style analysis that requires box plots of differences. let's do that and THEN make calls, ok? uai is a continous variable and can be fit with a line, these categorical variables are not and you should treat them like the cat data.
  fit <- lm(trend_year ~ Breeding_Biome, data = output)
  summary(fit)
  #____________________________________________________________________________
  
  
  output_base <- run_gee(formula = f_base,
                    mbbs, species_list) %>%
    #remove intercept information
    filter(!value %in% "(Intercept)") %>%
    #add trend into (exponentiate the esimate)
    mutate(trend = exp(estimate)-1) %>%
    #pivot_wider
    pivot_wider(names_from = value, values_from = c(estimate, std.error, statistic, p.value, trend)) %>%
    dplyr::select(-name)%>%
    left_join(uai, by = c("common_name" = "Species"))
  
  #estimate and trend are 1:1 matched. !!! Something is probably wrong with trend, I don't think it should all be in the .30s when the estimates are -06:0.08
  plot(output$trend_year, output$estimate_year)
  
  fit <- lm(trend_year ~ UAI, data = output)
  plot(y=output$trend_year, x=output$UAI)
  plot(trend_year ~ UAI, data = output)
  abline(fit)
  
  plot(count ~ year, data = pw)
  fit <- lm(count ~ year, data = pw)
  abline(fit)
  
  plot(x=output_cor$trend_year,y= output_cor$estimate_percent_developed)
    
#------------------------------------------  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
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
    