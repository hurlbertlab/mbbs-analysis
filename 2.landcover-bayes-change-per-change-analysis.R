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
    years_btwn = year - lag(year)
  ) %>%
  ungroup()
  

