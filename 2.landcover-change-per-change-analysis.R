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
library(stringr)
unloadNamespace("rethinking")
options(scipen=999)
options(mc.cores = parallel::detectCores())

source("2.species-trend-estimate-functions.R")

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
  #set right now at 20 quarter routes
  #this excludes species that are not seen enough to make any sort of confident estimate on their trends, although one benefit of the bayes model is that the number of datapoints you need is 0, the slopes we fit are also going to SPAN 0 and be insigificant. 
  #this represents species that just do not commonly breed in the area and that we ought not make assumptions about anyway bc this isn't their usual breeding location.
  filter_to_min_qrts(min_quarter_routes = 20) %>%
  #now we only have species of interest, create a species_id 
  group_by(common_name) %>%
  mutate(sp_id = cur_group_id()) %>%
  ungroup() %>%
  #let's left_join in the landcover data
  left_join(dev, by = c("route", "quarter" = "quarter_route", "year")) %>%
  left_join(forest, by = c("route", "quarter" = "quarter_route", "year")) %>%
  left_join(grassland, by = c("route", "quarter" = "quarter_route", "year")) %>%
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  #time for the new stuff. For a change for change analysis, rather than each data point being the count and the urbanization%, each datapoint needs to be a lag count and lab urbanization percent. let's also have a years_btwn variable thats how long the latest lag is. let's sort the data first.
  group_by(common_name, quarter_route) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    change_count = q_rt_count - lag(q_rt_count),
    #when the count is 0 two years in a row, we assume species is not present on the quarter route and need to remove that data point from consideration, as theres no chance for the population to change. If species returns, eg, time series is 0,0,1 - that 0,1 datapoint is still fine. species 'came back' to the quarter route.
    #flag_0_to_0 = pmax(q_rt_count, lag(q_rt_count)), #DEPRECIATED
    #change_dev = rmax_dev_quarter - lag(rmax_dev_quarter),
    change_dev = rmax_dev_plus_barren - lag(rmax_dev_plus_barren),
    #also take change forest
    change_forest = perc_forest_quarter - lag(perc_forest_quarter),
    change_grassland = perc_grassland_quarter - lag(perc_grassland_quarter),
    years_btwn = year - lag(year),
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
  filter(is.na(change_count) == FALSE) #%>%
  #DEPRECIATED: remove the years where the population had no chance to change in response to any underlying landcover change (population was 0 both years, so these 0 population changes are different from when species is present eg. count = 2 and count = 2 and population doesn't change between years). 
  #filter(flag_0_to_0 != 0) #67584 before, 29280 after.

  #quick calculation of how often the mean count of a sp. at a quarter route is each number
  n_q_rt_count = stopdata %>%
    group_by(q_rt_count) %>%
    summarize(n = n()) %>%
    filter(!q_rt_count == 0) %>% #remove the 0 counts
    mutate(percent = n/sum(n),
           cum_percent = cumsum(percent))

  #check for species where we should be hesitant to work with the data because there IS an effect of year on the change in count eg. there's exponential declines to the degree it affects the scale of change in counts at the quarter route level
  flagged_sp <- stopdata %>% 
    filter(flag == "FLAG", #was it flagged for a significant change_count ~ year relationship?
           r_sq > 0.01) #if it was flagged, did it actually explain ANY variation in the data?
  
  #assert that no species are flagged.
  assertthat::assert_that(nrow(flagged_sp) == 0)
  #great, if it passes we can move on :)
  stopdata <- stopdata %>% 
    dplyr::select(-flag, -r_sq, -pvalue_changecount_by_year)
  
  #if we wanted to remove routes where a species is never seen, but keep the other routes..
  stopdata_0sprts_removed <- stopdata %>%
    group_by(common_name, q_rt_standard) %>%
    filter(sum(q_rt_count) > 0) %>%
    ungroup() #44733 observations
  #!!!!!!!!!!!!for this run
  stopdata <- stopdata_0sprts_removed
  
  #want to have something that tells us how many samples we have from each species as well, since they're no longer equal
  sample_size <- stopdata %>% 
    group_by(common_name, sp_id) %>%
    summarize(sample_size = n()) %>%
    mutate(pch_scale = log(sample_size)+.5) %>%
    ungroup()
  
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
  landcover <- c("dev+barren", "forest_positive", "forest_negative", "grassland_positive", "grassland_negative")
  #for testing
  #landcover <- c("dev+barren")
  #for running the grassland model only
  #landcover <- c("grassland_positive", "grassland_negative")
  
#where to save stan code and fit
save_to <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.09.18_cpc_rm0sprt_alllandcovers/"
#if the output folder doesn't exist, create it
if (!dir.exists(save_to)) {dir.create(save_to)}
#for use in descriptive plots, also save the df there
#  write.csv(stopdata, paste0(save_to, "/stopdata.csv"))
#save the sample size
write.csv(sample_size, paste0(save_to, "sample_size.csv"), row.names = FALSE)

#stan model specified in landcover_qrt_trends.stan, let R know where to find it
stan_model_file <- "2.landcover_change_per_change.stan"
#compile the stan model
stan_model <- stan_model(file = stan_model_file)
beepr::beep()
print("model compiled")

#save the model text
file.copy(stan_model_file, save_to, overwrite = TRUE)
print("model saved")
#save a species list
species_list <- stopdata %>% dplyr::distinct(common_name, sp_id)
write.csv(species_list, paste0(save_to, "species_list.csv"), row.names = FALSE)

####LOOP through forest and developed landcover change models
for(a in 1:length(landcover)) {
  
  #blankdata set everything will be saved to
  fit_summaries <- as.data.frame(NULL)
  posterior_samples <-  as.data.frame(NULL)
  
  #set up data for use in this loop w/o affecting our background stopdata df
  loopdata <- stopdata
    
    #pick the relevant landcover variables depending on the model running this time
    if(landcover[a] == "forest_all") {
      change_selected_land <- loopdata$change_forest
      base_selected_land <- loopdata$perc_forest_quarter
    } else if (landcover[a] == "dev+barren") {
      change_selected_land <- loopdata$change_dev
      base_selected_land <- loopdata$rmax_dev_plus_barren
    } else if (landcover[a] == "forest_positive") {
      loopdata <- loopdata %>%
        filter(change_forest >= 0) %>%
        group_by(q_rt_standard) %>%
        mutate(q_rt_standard = cur_group_id()) %>%
        ungroup()
      change_selected_land <- loopdata$change_forest
      base_selected_land <- loopdata$perc_forest_quarter
    } else if (landcover[a] == "forest_negative") {
      loopdata <- loopdata %>%
        filter(change_forest <= 0)  %>%
        group_by(q_rt_standard) %>%
        mutate(q_rt_standard = cur_group_id()) %>%
        ungroup()
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
    datstan <- list(
      N = nrow(loopdata), #number of observations
      Nqrt = length(unique(loopdata$q_rt_standard)), #number of unique quarter routes
      qrt = loopdata$q_rt_standard, #qrt index for each observation
      Nsp = length(unique(loopdata$sp_id)), 
      sp = loopdata$sp_id,
      change_landcover = change_selected_land, #change in percent developed or forest for each observation since the last year
      #base_landcover = base_selected_land, #running max developed or perc forest,
      change_obs = loopdata$change_obs, #if the observer changed between years
  #    R = loopdata$log_rc_div_yb #log transformed ratio of counts incorporating gap length between survey years
  #    year = loopdata$year_standard, #year for each observation, standard year = 2012. Implicitly captures the years_btwn variable so we won't worry about like, adding that. 
      change_C = loopdata$change_count #count data for each observation, change since the last year
    )
    print("datstan set")
    
    timestamp()
    #fit the model to the data
    fit <- sampling(stan_model,
                    data = datstan,
                    chains = 4,
                    cores = 4, 
                    iter = 10000, #should be 10k in a full model
                    warmup = 2000) #2k in a full model
    beepr::beep()
    print(paste0("model fit for: ", landcover[a]))
    timestamp()
    
    #save the output
    fit_temp <- as.data.frame(summary(fit)$summary) %>%
      mutate(rownames = rownames(.)) %>%
      relocate(rownames, .before = mean) %>%
      #filter out the z-score intercept calculations
      filter(str_detect(rownames, "a_z") == FALSE) %>%
      #exponentiate (not sure we need this!)
      #need new things in this, don't need the exp do need to extract the sp_id and the q_rt_standard and to left_join in the species_list to get the common names.
      mutate(
             sp_id = as.numeric(ifelse(
               str_detect(.$rownames, "a_sp|b_landcover_change"),
               str_extract(.$rownames, "[0-9]([0-9])?"),
               NA)),
             q_rt_standard = as.numeric(ifelse(
               str_detect(.$rownames, "a_qrt"),
               str_extract(.$rownames, "[0-9]([0-9])?([0-9])?"),
               NA)),
             slope = ifelse(str_detect(rownames, "b_"), 
                            paste0(str_extract(rownames, "year|dev|forest|landcover"),", ", landcover[a]),
                            NA),
             raw = ifelse(str_detect(rownames, "raw"), TRUE, NA),
             flag_rhat = ifelse(round(.$Rhat, 2) == 1, FALSE, TRUE),
             flag_neff = ifelse(.$n_eff > 2000, FALSE, TRUE)
             ) %>%
      left_join(species_list, by = "sp_id") %>%
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
    temp_posterior <- as.data.frame(fit) %>%
      select(starts_with("b_")) %>%
      select(!contains("raw")) %>%
      mutate(row_id = row_number(),
             landcover = landcover[a]) 
    #bind rows
    posterior_samples <- bind_rows(posterior_samples, temp_posterior) #%>%
    #  dplyr::select(b_landcover_change, 
    #                #b_landcover_base,
    #                row_id, 
    #                landcover)
    #save
    write.csv(posterior_samples, paste0(save_to, landcover[a], "_posterior_samples.csv"), row.names = FALSE)
    print("datasets saved")
    timestamp()
  } #end landcover loop



