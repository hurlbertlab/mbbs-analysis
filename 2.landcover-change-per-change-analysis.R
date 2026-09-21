#####################
#
# Take the stop-level data we have on birds
# and run an analysis of the effects of landcover
# on species population trends
# at the quarter-route level WHERE
# we are looking at a lag effect eg.
# % change in count predicted by
# a % change in urbanization. and a % change in forest
# and splitting out the negative and positive forest effects
# controlling for the effect of observer we will also add a variable of
# change in observer quality.
# 
######################

library(dplyr)
library(rstan)
rstan_options(auto_write = TRUE)
library(stringr)
unloadNamespace("rethinking")
options(scipen=999)
options(mc.cores = parallel::detectCores())

source("2.analysis-functions.R")

#set model fun, options = one_year OR full_lag OR two_year OR three_year
model_run = "full_lag" #we'll use full lag bc it includes trait calculations. Why did I need a different model for the full lag vs 1 year anyway? Not clear to me those needed to be handled any differently.
#ope, it's because of quarter routes. The full lag doesn't need a control for quarter routes b/c each qr was represented only once. So... now I do want to modify the one-year model to inlcude species traits directly rather than then passing to a second model. hokay!

#read in data we need
barren <- read.csv("data/nlcd-landcover/nlcd_annual_barren.csv") 
dev <- read.csv("data/nlcd-landcover/nlcd_annual_running_max_developed.csv") %>%
  left_join(barren, by = c("route", "stop_num", "year")) %>%
  #get percent developed by route-quarter
  mutate(quarter_route = as.integer(case_when(stop_num > 15 ~ 4,
                                              stop_num > 10 ~ 3,
                                              stop_num > 5 ~ 2,
                                              stop_num > 0 ~ 1)),
         perc_barren = ifelse(is.na(perc_barren), 0, perc_barren)) %>%
  group_by(route, quarter_route, year) %>%
  #summarize bc we only need to keep 1 entry per route quarter
  summarize(rmax_dev_quarter = mean(running_max_perc_developed),
            perc_barren = mean(perc_barren)) %>%
  ungroup() %>%
  #create a variable where dev has barren ground added on
   mutate(rmax_dev_plus_barren = rmax_dev_quarter + perc_barren) #%>%
  ##Following deprecated b/c we don't use baseline development at all anymore in this model
      #   #now, we're going to take an extra step and center this data. What this is going to do is help keep everything interpretable in the model. So, rather than our intercepts being at 0% urbanization, our intercepts will be at mean_urbanization
      #   mutate(mean_dev = mean(rmax_dev_quarter),
      #          sd_dev = sd(rmax_dev_quarter),
      #          centered_rmax_dev = rmax_dev_quarter - mean_dev,
      #          z_score_rmax_dev = ((rmax_dev_quarter - mean_dev)/ sd_dev))
      # #NOTE: currently using rmax_dev_plus_barren in the model, and have depreciated use of z_score_rmax_dev
      # #so mean urbanization is about a quarter urbanized.
      # #if we use the centered rmax dev, the interpretation of the intercept is the intercept at the mean urban % (a quarter urbanized)
      # #if we use the z score rmax dev, which has been standardized, 
      # #the intercept represents the expected bird counts at the average urbanization (a quarter urbanized)
      # #a 1 unit change in the z score is 1 SD above the mean. the SD is 21.1% change. 


forest <- read.csv("data/nlcd-landcover/nlcd_annual_sum_forest.csv") %>%
  #summarize bc we only need 1 entry per route quarter
  group_by(route, quarter_route, year, perc_forest_quarter) %>%
  summarize() %>%
  ungroup()

grassland <- read.csv("data/nlcd-landcover/nlcd_annual_sum_grassland.csv") %>%
  group_by(route, quarter_route, year, perc_grassland_quarter) %>%
  summarize() %>%
  ungroup()

max_nlcd_year <- max(dev$year)

#observer information:
load("data/mbbs/mbbs_survey_events.rda")
obs <- mbbs_survey_events %>%
  dplyr::select(route, primary_observer, observer_ID, year, observer_quality)

#to stopdata we need to:
# - filter to just the 1:1 year changes
# - filter to the 5 year change groups, based on the first year of surveys available. 
#         - actually, for the 5 year change groups, can I pick a different group of 5 years for each route? Like pick the 5 year gap starting in a place that maximizes the number of 5 year gaps for that route? I think that could be okay?..
# - do just the full time period group.
# - also hey um, some of these years between are wack. There should not be a quarter-route that has '21' years between?? so there's some amount of problem solving to be done here. AH. Okay there could be a 21 year lag because I'm depending on the availability of stop-level data, of which there is no guarantee there is one. 

stopdata <- read.csv("data/mbbs/mbbs_stops_counts.csv") %>%
  ##########
  # testing
  #filter(common_name %in% c("Acadian Flycatcher", "Wood Thrush", "Northern Bobwhite", "Indigo Bunting", "Northern Cardinal")) %>%
  ############
  #make unique quarter route identifier
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
  #add observer information
  left_join(obs, by = c("year", "route")) %>%
  #let's pull out the species that are unscientific, waterbirds, etc.
  filter(!common_name %in% excluded_species) %>%
  #let's also remove species that don't meet our minimum bound observations 
  #set right now at 10 quarter routes with at least 2 observations
  #this excludes species that are not seen enough to make any sort of confident estimate on their trends, although one benefit of the bayes model is that the number of datapoints you need is 0, the slopes we fit are also going to SPAN 0 and be insigificant. 
  #this represents species that just do not commonly breed in the area and that we ought not make assumptions about anyway bc this isn't their usual breeding location.
  filter_to_min_qrts(min_quarter_routes = 10,
                     min_obs_per_route = 2) %>%
  #now we only have species of interest, create a species_id 
  group_by(common_name) %>%
  mutate(sp_id = cur_group_id()) %>%
  ungroup() %>%
  #let's left_join in the landcover data
  left_join(dev, by = c("route", "quarter" = "quarter_route", "year")) %>%
  left_join(forest, by = c("route", "quarter" = "quarter_route", "year")) %>%
  left_join(grassland, by = c("route", "quarter" = "quarter_route", "year")) |>
#and, we know there are quarter routes where a species is just never seen across the whole dataset. These are quarter routes where like, we just can't say anything about this species. They're not there and are never there. We want to remove those sp-qrt combinations from consideration
  (\(x) {
    x |>
      anti_join(
        #remove sp-qrt combinations where a species is never present
       ( x |>
          group_by(common_name, quarter_route) |>
          summarize(n_observations = sum(q_rt_count), .groups = "drop") |>
          filter(n_observations == 0)
         ),
        by = c("common_name", "quarter_route")
      )
  })()

add_lags <- stopdata |>
  group_by(common_name, quarter_route) |>
  arrange(year, .by_group = TRUE) |>
  mutate(mean_t0tm1 = (q_rt_count + lag(q_rt_count))/2,
         mean_t0tm1tm2 = (q_rt_count + lag(q_rt_count) + lag(q_rt_count, 2))/3,
         mean_t1t2 = (lead(q_rt_count) + lead(q_rt_count, 2))/2,
         mean_t1t2t3 = (lead(q_rt_count) + lead(q_rt_count, 2) + lead(q_rt_count, 3))/3,
         y_0m1 = year - lag(year), #needs to be 1 for two-year analysis
         y_0m1m2 = year - lag(year, 2), #needs to be 2 for three-year analysis
         y_12 = lead(year) - year, #needs to be 1 for two year analysis
         y_123 = lead(year, 2) - year, #needs to be 2 for three-year analysis
         #regardless of the lag in my calculations of abundances.... so it's changes in abundances to changes in landcover of a single year, then what's the response of birds over the next 2/3 years, that landcover change is always going to be t1 - t0. 
         change_dev = rmax_dev_plus_barren - lag(rmax_dev_plus_barren),
         #also take change forest
         change_forest = perc_forest_quarter - lag(perc_forest_quarter),
         change_grassland = perc_grassland_quarter - lag(perc_grassland_quarter),
         #calculate if observer changed as well
         change_obs = case_when(observer_ID == lag(observer_ID) ~ 0,
                                observer_ID != lag(observer_ID) ~ 1),
         #calculate change in observer quality
         change_obs_qual = observer_quality - lag(observer_quality)
  )

two_year <- add_lags |>
  filter(y_0m1 == 1, 
         y_12 == 1) |>
  mutate(change_count = mean_t1t2 - mean_t0tm1) |>
  filter(!is.na(change_count)) |>
  group_by(quarter_route) |>
  mutate(q_rt_standard = cur_group_id()) |>
  ungroup() |>
  group_by(common_name, quarter_route) |>
  mutate(spqrt_standard = cur_group_id()) |>
  ungroup()

three_year <- add_lags |>
  filter(y_0m1m2 == 2,
         y_123 == 2) |>
  mutate(change_count = mean_t1t2t3 - mean_t0tm1tm2) |>
  filter(!is.na(change_count)) |>
  group_by(quarter_route) |>
  mutate(q_rt_standard = cur_group_id()) |>
  ungroup() |>
  group_by(common_name, quarter_route) |>
  mutate(spqrt_standard = cur_group_id()) |>
  ungroup()

  
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#time for the new stuff unique to each time period. For a change for change analysis, rather than each data point being the count and the urbanization%, each datapoint needs to be a lag count and lab urbanization percent. let's also have a years_btwn variable thats how long the latest lag is. let's sort the data first.
one_year <- stopdata |>
  ##testing
  #filter(route == "cthm-01" | route == "cthm-04") |>
  #filter(common_name == "Acadian Flycatcher") |>
  ##alright, and this is a dataset where one of the routes has some gaps in it. 
  group_by(common_name, quarter_route) |>
  arrange(year, .by_group = TRUE) |>
  mutate(change_count = q_rt_count - lag(q_rt_count), #okay, that worked as expected actually, and only calculated lags within each quarter_route
         years_btwn = year - lag(year)) |>
  mutate(
    #when the count is 0 two years in a row, we assume species is not present on the quarter route and need to remove that data point from consideration, as theres no chance for the population to change. If species returns, eg, time series is 0,0,1 - that 0,1 datapoint is still fine. species 'came back' to the quarter route.
    flag_0_to_0 = pmax(q_rt_count, lag(q_rt_count)), #DEPRECIATED
    #change_dev = rmax_dev_quarter - lag(rmax_dev_quarter),
    change_dev = rmax_dev_plus_barren - lag(rmax_dev_plus_barren),
    #also take change forest
    change_forest = perc_forest_quarter - lag(perc_forest_quarter),
    change_grassland = perc_grassland_quarter - lag(perc_grassland_quarter),
    #calculate if observer changed as well
    change_obs = case_when(observer_ID == lag(observer_ID) ~ 0,
                           observer_ID != lag(observer_ID) ~ 1),
    #calculate change in observer quality
    change_obs_qual = observer_quality - lag(observer_quality),
    # commented out section here depricated b/c most of these go to eg. negative infinity. Can't divide things by 0s
    #switching things up, we also want to create a ratio between years, then take the log of that ratio and divide it by the years_btwn
    #ratio_count = case_when(
    #  q_rt_count == 0 & lag(q_rt_count) == 0 ~ 0,
    #  TRUE ~ q_rt_count/lag(q_rt_count) #likely to actually NOT use bc Inf errors when previous year the count on the quarter route was 0 (p common occurance as we know), as if we removed those it takes out like almost every population increase. Nah
    #),
    #log_rc_div_yb = log(ratio_count)/years_btwn, #likely to depreciate
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
#UN-DEPRECIATED: remove the years where the population had no chance to change in response to any underlying landcover change (population was 0 both years, so these 0 population changes are different from when species is present eg. count = 2 and count = 2 and population doesn't change between years). 
filter(flag_0_to_0 != 0) |>
  filter(years_btwn == 1) #filter to the one year changes #27,990.
  #the least represented species has only 39 observations

representation <- one_year |>
  group_by(common_name) |>
  summarize(n = n())

#hist(representation$n)
#min(representation$n)
#max(representation$n)

#so, function that calculates the maximum number of 5 years gaps based on each starting year.
#years_list <- c(1999, 2000, 2002, 2003, 2004, 2005, 2010) #so here, it's best to start in 2000 (3 #matches) rather than 1999 (2 matches)
#years_list = data.frame(year = years_list)
#  for(i in 1:nrow(years_list)) {
#    starting_value = years_list$year[i]
#    matches <- seq(from = starting_value, to = max(years_list$year), by = 5)
#    
#    years_list$n_matches[i] = sum(years_list$year %in% matches == TRUE)
#  }
#
#years_list <- years_list |>
#  filter(n_matches == 2)
#
#pick the starting year with the most matches 
#and if there's a tie in n_matches then pick the earliest start year.
  #so like
#starting_year <- years_list |>
#  filter(n_matches == max(n_matches))
#starting_year <- starting_year |>
#  mutate(earliest = case_when(
#    n() == 1 ~ NA,
#    n() > 1 ~ min(year))) |>
#  filter(year == earliest)
#you'll have to check out some edge cases here where like, due to data availability there's good data earlier and good data later but on seperate 5 yr schedules that don't otherwise overlap but I think that sounds fine..
#for my longer time frame, it's not going to be 27 years bc not all routes have that amount of time. BUT BUT BUT I could do my last round based off the longest possible lag time for year route-quarter, and add that lag length as a predictor variable or like, check the distribution of it? Check the distribution of it to decide what to do there. 

  #quick calculation of how often the mean count of a sp. at a quarter route is each number
  n_q_rt_count = stopdata %>%
    group_by(q_rt_count) %>%
    summarize(n = n()) %>%
    filter(!q_rt_count == 0) %>% #remove the 0 counts
    mutate(percent = n/sum(n),
           cum_percent = cumsum(percent))

##### calculate the dataframe for the full lag model
full_lag <- stopdata |>
  ##testing
  #filter(route == "cthm-01" | route == "cthm-04") |>
  #filter(common_name == "Acadian Flycatcher") |>
  ##alright, and this is a dataset where one of the routes has some gaps in it. 
  group_by(common_name, quarter_route) |>
  arrange(year, .by_group = TRUE) |>
  mutate(earliest_year = case_when(year == min(year) ~ "earliest",
                                   year == max(year) ~ "latest",
                                   TRUE ~ "0"),
         mean_0m1 = (q_rt_count + lag(q_rt_count))/2) |>
  mutate(mean_0m1 = case_when(
    earliest_year == "earliest" ~ q_rt_count, #earliest year just gets that count of that year, b/c of data availability we can't always have two starting years next to each other
    TRUE ~ mean_0m1)) |> #but for the latest year we average the count of the two latest years.
  filter(earliest_year != "0") |>
  mutate(change_count = mean_0m1 - lag(mean_0m1),
         years_btwn = year - lag(year)) |> #should subtract earliest from latest - tested and works as expected.
  # now do all the landscape changes
  mutate(
    #change_dev = rmax_dev_quarter - lag(rmax_dev_quarter),
    change_dev = rmax_dev_plus_barren - lag(rmax_dev_plus_barren),
    #also take change forest
    change_forest = perc_forest_quarter - lag(perc_forest_quarter),
    change_grassland = perc_grassland_quarter - lag(perc_grassland_quarter),
    #calculate if observer changed as well
    change_obs = case_when(observer_ID == lag(observer_ID) ~ 0,
                           observer_ID != lag(observer_ID) ~ 1),
    #calculate change in observer quality
    change_obs_qual = observer_quality - lag(observer_quality),
    flag_0_to_0 = ifelse((q_rt_count + lag(q_rt_count) == 0), TRUE, FALSE)
  ) %>%
  #remove the NA years (first record of each quarter route) 
  filter(is.na(change_count) == FALSE) |>
  #and, we should also remove full lags that are just too short to be the long-term effects that we're trying to get at. let's cut out quarter routes with less than a 10 year lag.
  filter(years_btwn >= 10) |>
  #remove routes where the species is not present in the start or the end
  filter(flag_0_to_0 == FALSE) |>
  ungroup()
  
  #hist(full_lag$change_count)
  #table(full_lag$years_btwn)
  #hist(full_lag$change_dev)
  #table(full_lag$common_name)
  
#working with just the one_year, so here one_year becomes stopdata
  if(model_run == "one_year") {
    stopdata <- one_year
  } else if(model_run == "full_lag") {
    stopdata <- full_lag
  } else if(model_run == "two_year") {
    stopdata <- two_year
  } else if(model_run == "three_year") {
    stopdata <- three_year
  }
  
#add species traits to stopdata as they might be needed
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!not on longleaf yet
  #UAI
  uai <- read.csv("data/species-traits/UAI-NateCleg-etall.csv") %>%
    dplyr::filter(City == "Charlotte_US") %>%
    #fix House Wren -> Northern House Wren which has had a taxonomy change since this data was published
    dplyr::mutate(Species = 
                    case_when(
                      Species == "House Wren" ~ "Northern House Wren",
                      TRUE ~ Species
                    )) %>%
    dplyr::select(-X, -SE)
  
  #ebird-habitat data
  #habitat_select <- read.csv("data/species-traits/ebird-habitat-association/forest-grass-habitat-associations.csv")
  habitat_select <- read.csv("data/species-traits/species_list.csv") %>%
    dplyr::select(common_name, ebird_code, ebirdst_association_forest, ebirdst_association_grassland)
  
  
  #want to have something that tells us how many samples we have from each species as well, since they're no longer equal
  sample_size <- stopdata %>% 
    group_by(common_name, sp_id) %>%
    summarize(sample_size = n()) %>%
    mutate(pch_scale = log(sample_size)+.5) %>%
    ungroup()
  
  #add all to species_list
  traits <- sample_size |>
    left_join(habitat_select, by = "common_name") |>
    left_join(uai, by = c("common_name" = "Species")) |>
    #scale all the variables so we can compare across them
    mutate(
      scale_UAI = ((UAI - mean(UAI))/sd(UAI)),
      scale_eaforest = ((ebirdst_association_forest - mean(ebirdst_association_forest))/sd(ebirdst_association_forest)),
      scale_eagrassland = ((ebirdst_association_grassland - mean(ebirdst_association_grassland))/sd(ebirdst_association_grassland))
    ) |>
    ungroup()
#!!!!!!!!!!!!!!!!!!!!!!!!not on longleaf yet
  
  #also need to save the species id and spqrt conversion
  spqrt_info <- stopdata |>
    distinct(common_name, sp_id, spqrt_standard)
  
  #check for species where we should be hesitant to work with the data because there IS an effect of year on the change in count eg. there's exponential declines to the degree it affects the scale of change in counts at the quarter route level
#  flagged_sp <- stopdata %>% 
#    filter(flag == "FLAG", #was it flagged for a significant change_count ~ year relationship?
#           r_sq > 0.01) #if it was flagged, did it actually explain ANY variation in the data?
  
  #assert that no species are flagged.
#  assertthat::assert_that(nrow(flagged_sp) == 0)
  #great, if it passes we can move on :)
#  stopdata <- stopdata %>% 
#    dplyr::select(-flag, -r_sq, -pvalue_changecount_by_year)
  #Always passes :)
  
  
  #if we wanted to remove routes where a species is never seen, but keep the other routes..
  #btw pretty sure this is broken. seems to just remove 0 counts even though they have a change in count from the previous year.
  #stopdata_0sprts_removed <- stopdata %>%
  #  group_by(common_name, q_rt_standard) %>%
  #  filter(sum(q_rt_count) > 0) %>% <- probably broken here.
  #  ungroup() #44733 observations
  #!!!!!!!!!!!!for this run
  #stopdata <- stopdata_0sprts_removed

  
  #if we want to randomly subsample a given number of observations from each species based on the number of samples we take in the rm0to0 group...
  #sample_size <- read.csv("Z:/Goulden/mbbs-analysis/model_landcover/2025.09.09_cpc_allspin1_rm0to0_halfnormalsig_sp/sample_size.csv")
  #subsampled_stopdata <- NULL
  #for(n in 1:nrow(sample_size)) {
  #  sp <- sample_size$common_name[n]
  #  temp <- stopdata %>%
  #    #filter to one species
  #    filter(common_name == sample_size$common_name[n]) %>%
  #    #randomly subsample
  #    slice_sample(n = sample_size$sample_size[n])
  #  
  #  #add back to df
  #  subsampled_stopdata <- bind_rows(subsampled_stopdata, temp)
  #}
  ##assert that the sizes of the subsample match for a test species.
  #assertthat::assert_that(nrow(subsampled_stopdata %>% filter(common_name == "Northern Bobwhite")) == sample_size$sample_size[sample_size$common_name == "Northern Bobwhite"])
  ##!!!!!!!!!!!!for this run
  #stopdata <- subsampled_stopdata
  
  #we're going to run the same model for both our urban (dev + barren) and for our forest variables - breaking out the various effects of change in the amount of urbanization, positive increases in forest cover, and negative decreases in forest cover. Forest cover and urbanization change are not 1:1 correlated so these are indeed different from each other. 
  landcover <- c("dev+barren", "forest_positive", "forest_negative"
                 #"grassland_positive", "grassland_negative"
                 ) #for now, let's just focus on the dev + forests like we need to for ESA
  #for testing
  #landcover <- c("dev+barren")
  #for running the grassland model only
  #landcover <- c("grassland_positive", "grassland_negative")
  
#where to save stan code and fit
save_to <- "Z:/Goulden/mbbs-analysis/model_landcover/2026.09.21.full-lag-rm0spqrts-uaiforest/"
#save_to <- "model/ch2/2026.07.28_full_lag_uaiONLY_fixbetas_keep00/"
#if the output folder doesn't exist, create it
if (!dir.exists(save_to)) {dir.create(save_to)}
#for use in descriptive plots, also save the df there
#  write.csv(stopdata, paste0(save_to, "/stopdata.csv"))
#save the sample size
write.csv(sample_size, paste0(save_to, "sample_size.csv"), row.names = FALSE)

#stan model specified in landcover_qrt_trends.stan, let R know where to find it
if(model_run %in% c("one_year", "two_year", "three_year")) {
  stan_model_file <- "2.landcover_change_per_change.stan"
} else if(model_run == "full_lag") {
  stan_model_file <- "2.landcover_cpc_full_lag.stan"
}
#compile the stan model
stan_model <- rstan::stan_model(file = stan_model_file)
beepr::beep()
print("model compiled")

#save the model text
file.copy(stan_model_file, save_to, overwrite = TRUE)
print("model saved")
#save a species list
if(model_run == "full_lag") {
  species_list <- stopdata |> dplyr::distinct(common_name, sp_id) 
} else {
  species_list <- stopdata %>% dplyr::distinct(common_name, sp_id, spqrt_standard) 
}
write.csv(species_list, paste0(save_to, "species_list.csv"), row.names = FALSE)
write.csv(traits, paste0(save_to, "traits.csv"), row.names = FALSE)
#save the spqrt conversion
write.csv(spqrt_info, paste0(save_to, "sprqt_info.csv"), row.names = FALSE)

####LOOP through forest and developed landcover change models
for(a in 1:length(landcover)) {
  
  #blankdata set everything will be saved to
  fit_summaries <- as.data.frame(NULL)
  posterior_samples <-  as.data.frame(NULL)
  
  #set up data for use in this loop w/o affecting our background stopdata df
  loopdata <- stopdata |>
    ungroup()
    
    #pick the relevant landcover variables depending on the model running this time
    if(landcover[a] == "forest_all") {
      change_selected_land <- loopdata$change_forest
      base_selected_land <- loopdata$perc_forest_quarter
    } else if (landcover[a] == "dev+barren") {
      change_selected_land <- loopdata$change_dev
      base_selected_land <- loopdata$rmax_dev_plus_barren
    } else if (landcover[a] == "forest_positive") {
      if(model_run == "full_lag") {
        loopdata <- loopdata %>%
          filter(change_forest >= 0) %>%
          group_by(q_rt_standard) %>%
          mutate(q_rt_standard = cur_group_id()) %>%
          ungroup()
      } else {
        loopdata <- loopdata %>%
          filter(change_forest >= 0) %>%
          group_by(spqrt_standard) %>%
          mutate(spqrt_standard = cur_group_id()) %>%
          ungroup() 
        spqrt_info <- loopdata |>
          distinct(common_name, sp_id, spqrt_standard)
        write.csv(spqrt_info, paste0(save_to, "forest_positive_sprqt_info.csv"), row.names = FALSE)
      }
      change_selected_land <- loopdata$change_forest
      base_selected_land <- loopdata$perc_forest_quarter
    } else if (landcover[a] == "forest_negative") {
      if(model_run == "full_lag") {
        loopdata <- loopdata %>%
          filter(change_forest >= 0) %>%
          group_by(q_rt_standard) %>%
          mutate(q_rt_standard = cur_group_id()) %>%
          ungroup()
      } else {
      loopdata <- loopdata %>%
        filter(change_forest <= 0)  %>%
        group_by(spqrt_standard) %>%
        mutate(spqrt_standard = cur_group_id()) %>%
        ungroup()
      spqrt_info <- loopdata |>
        distinct(common_name, sp_id, spqrt_standard)
      write.csv(spqrt_info, paste0(save_to, "forest_negative_sprqt_info.csv"), row.names = FALSE)
      }
      change_selected_land <- loopdata$change_forest
      base_selected_land <- loopdata$perc_forest_quarter
    } else if (landcover[a] == "grassland_positive") {
      loopdata <- loopdata %>%
        filter(change_grassland >= 0) %>%
        group_by(q_rt_standard) %>%
        mutate(q_rt_standard = cur_group_id()) %>%
        ungroup()
      change_selected_land <- loopdata$change_grassland
      base_selected_land <- loopdata$perc_grassland_quarter
    } else if (landcover[a] == "grassland_negative") {
      loopdata <- loopdata %>%
        filter(change_grassland <= 0) %>%
        group_by(q_rt_standard) %>%
        mutate(q_rt_standard = cur_group_id()) %>%
        ungroup()
      change_selected_land <- loopdata$change_grassland
      base_selected_land <- loopdata$perc_grassland_quarter
    }
    
    
    #set up the data to feed into the model
  if(model_run %in% c("one_year", "two_year", "three_year")) {
    datstan <- list(
      N = nrow(loopdata), #number of observations
      Nspqrt = length(unique(loopdata$spqrt_standard)),
      spqrt = loopdata$spqrt_standard,
      #Nqrt = length(unique(loopdata$q_rt_standard)), #number of unique quarter routes
      #qrt = loopdata$q_rt_standard, #qrt index for each observation
      Nsp = length(unique(loopdata$sp_id)), 
      sp = loopdata$sp_id,
      change_landcover = (change_selected_land/100), #change in percent developed or forest for each observation since the last year
      #base_landcover = base_selected_land, #running max developed or perc forest,
      change_obs = loopdata$change_obs, #if the observer changed between years
      #    R = loopdata$log_rc_div_yb #log transformed ratio of counts incorporating gap length between survey years
      #    year = loopdata$year_standard, #year for each observation, standard year = 2012. Implicitly captures the years_btwn variable so we won't worry about like, adding that. 
      change_C = loopdata$change_count, #count data for each observation, change since the last year
      forest_association = traits$scale_eaforest,
      #grassland_association = traits$scale_eagrassland,
      uai = traits$scale_UAI
    )
  } else if(model_run == "full_lag") {
    datstan <- list(
      N = nrow(loopdata), #number of observations
      Nsp = length(unique(loopdata$sp_id)), 
      sp = loopdata$sp_id,
      change_landcover = (change_selected_land/100), #change in percent developed or forest for each observation since the last year. Divide by 100 because it's on a pretty different scale from everything else right now, and at heart it is a percentage.
      #base_landcover = base_selected_land, #running max developed or perc forest,
      change_obs = loopdata$change_obs, #if the observer changed between years
      #    R = loopdata$log_rc_div_yb #log transformed ratio of counts incorporating gap length between survey years
      #    year = loopdata$year_standard, #year for each observation, standard year = 2012. Implicitly captures the years_btwn variable so we won't worry about like, adding that. 
      change_C = loopdata$change_count, #count data for each observation, change since the last year
      forest_association = traits$scale_eaforest,
      uai = traits$scale_UAI
    )

  }
    
    print("datstan set")
    
    timestamp()
    #fit the model to the data
    fit <- sampling(stan_model,
                    data = datstan,
                    chains = 4,
                    cores = 4, 
                    iter = 1000, #should be 10k in a full model
                    warmup = 200) #2k in a full model
    beepr::beep()
    print(paste0("model fit for: ", landcover[a]))
    timestamp()
    
    #save the output
    fit_temp <- as.data.frame(summary(fit)$summary) %>%
      mutate(rownames = rownames(.)) %>%
      relocate(rownames, .before = mean) %>%
      #filter out the z-score intercept calculations
      filter(str_detect(rownames, "a_z") == FALSE) %>%
      #filter out this if present
      filter(!str_detect(rownames, "spqrt_intercept")) %>%
      #exponentiate (not sure we need this!)
      #need new things in this, don't need the exp do need to extract the sp_id and the q_rt_standard and to left_join in the species_list to get the common names.
      mutate(
             sp_id = as.numeric(ifelse(
               str_detect(.$rownames, "b_landcover_change"),
               str_extract(.$rownames, "[0-9]([0-9])?"),
               NA)),
             spqrt_standard = as.numeric(ifelse(
               str_detect(.$rownames, "a_spqrt"),
               str_extract(.$rownames, "[0-9]([0-9])?([0-9])?([0-9])?"),
               NA)),
             slope = ifelse(str_detect(rownames, "b_"), 
                            paste0(str_extract(rownames, "year|dev|forest|landcover"),", ", landcover[a]),
                            NA),
             raw = ifelse(str_detect(rownames, "raw"), TRUE, NA),
             flag_rhat = ifelse(round(.$Rhat, 2) == 1, FALSE, TRUE),
             flag_neff = ifelse(.$n_eff > 2000, FALSE, TRUE)
             ) %>%
      #left_join(species_list, by = "sp_id") %>%
      #remove the intermediate step data
      filter(is.na(raw))  %>%
      #rename numeric columns
      rename_with(~ paste0("conf_", .), .cols = matches("^[0-9]")) %>%
      #remove %s in column names
      rename_with(~ str_remove(., "%"), .cols = everything())
    
    #bind rows
    fit_summaries <- bind_rows(fit_summaries, fit_temp)
    #save
    write.csv(fit_summaries, paste0(save_to, landcover[a], "_fit_summaries.csv"), row.names = FALSE)
    print("saved fit summary")
    
    
    #extract posterior samples and save those also
    # let's not bother with this atm while testing.
    temp_posterior <- as.data.frame(fit) %>%
      select(!starts_with("a")) %>%
      select(!contains("raw")) %>%
      select(!contains("spqrt")) %>%
      select(!contains("lp_")) |>
      mutate(row_id = row_number()) |>
      mutate(landcover = landcover[a]) |>
      #and we don't need 32,000 samples. let's take the first 5k
      dplyr::filter(row_id < 5001)
 
   # bind rows
    posterior_samples <- bind_rows(posterior_samples, temp_posterior) #%>%
      #dplyr::select(b_landcover_change, 
      #              #b_landcover_base,
      #              row_id, 
      #              landcover)
    #save
    write.csv(posterior_samples, paste0(save_to, landcover[a], "_posterior_samples.csv"), row.names = FALSE)
    print("datasets saved")
    
    #calculate prop posterior >0
    nrow_posterior <- nrow(temp_posterior)
    prop_posterior <- temp_posterior |>
      summarize(across(everything(),
                       (prop_gt_0 = ~sum(. > 0)/nrow_posterior))) |>
      tidyr::pivot_longer(cols = c(contains("b_landcover"), contains("kappa")), 
                          names_to = "variable") |>
      dplyr::select(variable, value) |>
      rename(prop_posterior_gt_0 = value) |>
      mutate(common_name_standard = as.integer(str_extract(variable, "[0-9]([0-9])?")),
             kappa = str_extract(variable, "_[a-z]+")) |>
      dplyr::select(-variable) 
    write.csv(prop_posterior, paste0(save_to, landcover[a], "_prop_posterior_gt0.csv"), row.names = FALSE)
    timestamp()
  } #end landcover loop



