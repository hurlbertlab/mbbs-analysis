#####################
#
# Take the stop-level data we have on birds
# and run an analysis of the effects of landcover
# on species population trends
# at the quarter-route level WHERE
# we are looking at a lag effect eg.
# % change in count predicted by
# a % change in urbanization.
# this SHOULD incorporate our measure of observer_quality
#
######################

library(dplyr)
library(rstan)
library(stringr)
unloadNamespace("rethinking")
options(scipen=999)

source("2.species-trend-estimate-functions.R")

#read in data we need
dev <- read.csv("data/nlcd-landcover/nlcd_annual_running_max_developed.csv") %>%
  #get percent developed by route-quarter
  mutate(quarter_route = as.integer(case_when(stop_num > 15 ~ 4,
                                              stop_num > 10 ~ 3,
                                              stop_num > 5 ~ 2,
                                              stop_num > 0 ~ 1))) %>%
  group_by(route, quarter_route, year) %>%
  #summarize bc we only need to keep 1 entry per route quarter
  summarize(rmax_dev_quarter = mean(running_max_perc_developed)) %>%
  ungroup() %>%
  #now, we're going to take an extra step and center this data. What this is going to do is help keep everything interpretable in the model. So, rather than our intercepts being at 0% urbanization, our intercepts will be at mean_urbanization
  mutate(mean_dev = mean(rmax_dev_quarter),
         sd_dev = sd(rmax_dev_quarter),
         centered_rmax_dev = rmax_dev_quarter - mean_dev,
         z_score_rmax_dev = ((rmax_dev_quarter - mean_dev)/ sd_dev))
#so mean urbanization is about a quarter urbanized.
#if we use the centered rmax dev, the interpretation of the intercept is the intercept at the mean urban % (a quarter urbanized)
#if we use the z score rmax dev, which has been standardized, 
#the intercept represents the expected bird counts at the average urbanization (a quarter urbanized)
#a 1 unit change in the z score is 1 SD above the mean. the SD is 21.1% change. So 

forest <- read.csv("data/nlcd-landcover/nlcd_annual_sum_forest.csv") %>%
  #summarize bc we only need 1 entry per route quarter
  group_by(route, quarter_route, year, perc_forest_quarter) %>%
  summarize() %>%
  ungroup

max_nlcd_year <- max(dev$year)

stopdata <- read.csv("data/mbbs/mbbs_stops_counts.csv") %>%
  #make unique quarter route notifier
  mutate(quarter = case_when(stop_num > 15 ~ 4,
                             stop_num > 10 ~ 3,
                             stop_num > 5 ~ 2,
                             stop_num > 0 ~ 1),
         quarter_route = paste0(route,"-",quarter)) %>%
  group_by(quarter_route) %>%
  mutate(q_rt_standard = cur_group_id()) %>%
  ungroup() %>%
  #need to sum the counts to the quarter-route, right now by each individual stop, which is a different analysis unit from the quarter-route
  group_by(year, quarter_route, common_name, sci_name, q_rt_standard, route, quarter) %>%
  summarize(q_rt_count = sum(count)) %>%
  ungroup() %>%
  #keep only the data that's up to the year we have nlcd data for
  filter(year <= max_nlcd_year) %>%
  #standardize year
  standardize_year(starting_year = 2012) %>% #current stopdata max year is 2023 (huh! pull the 2024 data, might as well use it! actually this might be happening b/c we don't have 2024 landcover data..... anyway! we'll standardize on year 2012 (abt halfway btwn 2000 and 2023))
  #maybe year should be GENUINELY standardized. that would make the intercept lean towards the years where we have the most data.
  ####let's genuinely standardize year. like, the same way we z_score development.
  mutate(z_score_year = (year-mean(year))/sd(year)) %>%
  #mean year is 2014.36
  #sd year is 7.87
  #let's pull out the species that are unscientific, waterbirds, etc.
  filter(!common_name %in% excluded_species) %>%
  #let's also remove species that don't meet our minimum bound observations 
  #set right now at 20 quarter routes
  #this excludes species that are not seen enough to make any sort of confident estimate on their trends, although one benefit of the bayes model is that the number of datapoints you need is 0, the slopes we fit are also going to SPAN 0 and be insigificant. 
  #this represents species that just do not commonly breed in the area and that we ought not make assumptions about anyway bc this isn't their usual breeding location.
  filter_to_min_qrts(min_quarter_routes = 20) %>%
  #let's left_join in the landcover data
  left_join(dev, by = c("route", "quarter" = "quarter_route", "year")) %>%
  left_join(forest, by = c("route", "quarter" = "quarter_route", "year")) %>%
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  #time for the new stuff. For a change for change analysis, rather than each data point being the count and the urbanization%, each datapoint needs to be a lag count and lab urbanization percent. let's also have a years_btwn variable thats how long the latest lag is. let's sort the data first.
  group_by(common_name, quarter_route) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    change_count = q_rt_count - lag(q_rt_count),
    change_dev = rmax_dev_quarter - lag(rmax_dev_quarter),
    years_btwn = year - lag(year), 
    #switching things up, we also want to create a ratio between years, then take the log of that ratio and divide it by the years_btwn
    ratio_count = case_when(
      q_rt_count == 0 & lag(q_rt_count) == 0 ~ 0,
      TRUE ~ q_rt_count/lag(q_rt_count) #likely to actually NOT use bc Inf errors when previous year the count on the quarter route was 0 (p common occurance as we know), as if we removed those it takes out like almost every population increase. Nah
    ),
    log_rc_div_yb = log(ratio_count)/years_btwn, #likely to depreciate
  ) %>%
  #add a flag if the species is experiencing exponential declines that are going to cause problems
  ungroup() %>%
  group_by(common_name) %>%
  mutate(pvalue_changecount_by_year = summary(lm(change_count~year))$coefficients[2,4],
         r_sq = summary(lm(change_count~year))$r.squared,
         flag = ifelse(pvalue_changecount_by_year > .05, NA, "FLAG")) %>%
  ungroup() %>%
  #remove the NA years (first record of each quarter route) 
  filter(is.na(change_count) == FALSE) %>%
  #hmmmm and remake year_standard while we're here? now we've changed it. rn year standard is based on the mean year.
  standardize_year(starting_year = 2012) %>% 
  ####let's genuinely standardize year. like, the same way we z_score development.
  mutate(z_score_year = (year-mean(year))/sd(year)) 
  #yeah, and bc we've removed like a year's worth of data, that changes the z_score years. hm, maybe we should do year a different way. Check in abt this

  #check for species where we should be hesitant to work with the data because there IS an effect of year on the change in count eg. there's exponential declines to the degree it affects the scale of change in counts at the quarter route level
  flagged_sp <- stopdata %>% 
    filter(flag == "FLAG", #was it flagged for a significant change_count ~ year relationship?
           r_sq > 0.01) #if it was flagged, did it actually explain ANY variation in the data?
  
  #assert that no species are flagged.
  assertthat::assert_that(nrow(flagged_sp) == 0)
  #great, if it passes we can move on :)
  stopdata <- stopdata %>% 
    dplyr::select(-flag, -r_sq, -pvalue_changecount_by_year)
  
#where to save stan code and fit
save_to <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.06.06_change_per_change_woyear"
#if the output folder doesn't exist, create it
if (!dir.exists(save_to)) {dir.create(save_to)}

#stan model specified in landcover_qrt_trends.stan, let R know where to find it
stan_model_file <- "2.landcover_change_per_change.stan"
#compile the stan model
stan_model <- stan_model(file = stan_model_file)
beepr::beep()
print("model compiled")

#save the model text
file.copy(stan_model_file, save_to, overwrite = TRUE)
print("model saved")

#loop through the species
species_list <- stopdata %>% 
  distinct(common_name)
#testing
#species_list <- species_list[1:5,]

#blankdata set everything will be saved to
fit_summaries <- as.data.frame(NA)
posterior_samples <-  as.data.frame(NA)

for (i in species_list$common_name) {
  #filter to one species
  one_species <- stopdata %>% 
    filter(common_name == i)
  #print species name
  print(i)
  
  #set up the data to feed into the model
  datstan <- list(
    N = nrow(one_species), #number of observations
    Nqrt = length(unique(one_species$q_rt_standard)), #number of unique quarter routes
    qrt = one_species$q_rt_standard, #qrt index for each observation
    change_dev = one_species$change_dev, #change in percent developed for each observation since the last year
    dev = one_species$rmax_dev_quarter, #running max developed
#    R = one_species$log_rc_div_yb #log transformed ratio of counts incorporating gap length between survey years
#    year = one_species$year_standard, #year for each observation, standard year = 2012. Implicitly captures the years_btwn variable so we won't worry about like, adding that. 
    change_C = one_species$change_count #count data for each observation, change since the last year
  )
  print("datstan set")
  
  
  #fit the model to the data
  fit <- sampling(stan_model,
                  data = datstan,
                  chains = 4,
                  cores = 4, 
                  iter = 2000,
                  warmup = 1000)
  beepr::beep()
  print(paste0("model fit for: ",i))
  
  #save the output
  fit_temp <- as.data.frame(summary(fit)$summary) %>%
    mutate(rownames = rownames(.)) %>%
    relocate(rownames, .before = mean) %>%
    #filter out the z-score intercept calculations
    filter(str_detect(rownames, "a_z") == FALSE) %>%
    #exponentiate (not sure we need this!)
    mutate(exp_mean = exp(mean),
           exp_minconf = exp(`2.5%`),
           exp_maxconf = exp(`97.5%`),
           q_rt_standard = as.numeric(ifelse(
             str_detect(rownames, "a\\[\\d+\\]"),
             str_extract(rownames, "\\d+"),
             NA)),
           slope = ifelse(str_detect(rownames, "b"), 
                          str_extract(rownames, "year|dev"),
                          NA),
           common_name = i)
  #bind rows
  fit_summaries <- bind_rows(fit_summaries, fit_temp)
  #save
  write.csv(fit_summaries, paste0(save_to,"/fit_summaries.csv"), row.names = FALSE)
  
  
  #extract posterior samples and save those also
  temp_posterior <- as.data.frame(fit) %>%
    select(starts_with("b_")) %>%
    mutate(row_id = row_number(),
           common_name = i)
  #bind rows
  posterior_samples <- bind_rows(posterior_samples, temp_posterior) %>%
    dplyr::select(b_dev_change, b_dev_base, row_id, common_name)
  #save
  write.csv(posterior_samples, paste0(save_to,"/posterior_samples.csv"), row.names = FALSE)
  paste("datasets saved")
  timestamp()
}

