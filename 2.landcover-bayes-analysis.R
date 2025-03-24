#############################
#
# Take the stop-level data we have on birds
# and run an analysis of the effects of landcover
# on species population trends
# at the quarter-route level. 
#
#############################

library(dplyr)
library(rstan)

#read in data we need
stopdata <- read.csv("data/mbbs/mbbs_stops_counts.csv")
dev <- read.csv("data/nlcd-landcover/nlcd_annual_running_max_developed.csv") %>%
  #get percent developed by route-quarter
  mutate(quarter_route = case_when(stop_num > 15 ~ 4,
                                   stop_num > 10 ~ 3,
                                   stop_num > 5 ~ 2,
                                   stop_num > 0 ~ 1)) %>%
  group_by(route, quarter_route, year) %>%
  #summarize bc we only need to keep 1 entry per route quarter
  summarize(rmax_dev_quarter = mean(running_max_perc_developed)) %>%
  ungroup()

forest <- read.csv("data/nlcd-landcover/nlcd_annual_sum_forest.csv") %>%
  #summarize bc we only need 1 entry per route quarter
  group_by(route, quarter_route, year, perc_forest_quarter) %>%
  summarize() %>%
  ungroup

#Things don't have to be left-joined, bc we will datstan everything together.

#where to save stan code and fit
save_to <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.03.24_designing"
#if the output folder doesn't exist, create it
if (!dir.exists(save_to)) {dir.create(save_to)}
