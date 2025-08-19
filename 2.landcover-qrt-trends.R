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
library(stringr)
unloadNamespace("rethinking")

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
  left_join(forest, by = c("route", "quarter" = "quarter_route", "year"))

## step to pull out only the species that have enough non-zero data included.
#so, actually going to skip this step! and fit the model even for the species that we have very low data on. One of the benefits of bayes models is that the number of datapoints you need is 0... and if it turns out we ought to exclude these anyway, then we'll do that, but no need to cut ourselves off early.

#where to save stan code and fit
save_to <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.04.01_iale_uncenter_betas"
#if the output folder doesn't exist, create it
if (!dir.exists(save_to)) {dir.create(save_to)}

#stan model specified in landcover_qrt_trends.stan, let R know where to find it
stan_model_file <- "2.landcover_qrt_trends.stan"
#compile the stan model
stan_model <- stan_model(file = stan_model_file)
beepr::beep()
print("model compiled")
#save the model text
file.copy(stan_model_file, save_to, overwrite = TRUE)
print("model saved")

#loop through the species
species_list <- unique(stopdata$common_name)
#testing
#species_list <- species_list[1:2]

#blankdata set everything will be saved to
fit_summaries <- as.data.frame(NA)
posterior_samples <-  as.data.frame(NA)

for (i in species_list) {
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
    perc_dev = one_species$z_score_rmax_dev, #percent developed for each observation
    year = one_species$year_standard, #year for each observation
    C = one_species$q_rt_count #count data for each observation
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
          common_name = i) %>%
    rename_with(~ paste0("conf_", .), .cols = matches("^[0-9]")) %>%
    #remove %s in column names
    rename_with(~ str_remove(., "%"), .cols = everything())
  
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
      dplyr::select(b_year, b_dev, row_id, common_name)
    #save
    write.csv(posterior_samples, paste0(save_to,"/posterior_samples.csv"), row.names = FALSE)
    paste("datasets saved")
    timestamp()
}


###########################################
##########################################
###########################################



#What we also want is those beta values at the end, and to pull a sample from all those betas so we don't have to re-load the .rds for EVERY SPECIES but we can still get the samples. I want a dataframe where I sample both b_year and b_dev 4k times. And then I want to add on each species as we go.

#cor(stopdata$rmax_dev_quarter, stopdata$perc_forest_quarter)
#correlation is right at -.7 so, we should not use both of them in the model.

#plot the means without any information added
c <- one_species %>%
  group_by(q_rt_standard) %>%
  summarize(mean_count = mean(q_rt_count))
plot(c$q_rt_standard, c$mean_count,
     ylim = c(0,12),
     pch = 19) #this is our raw average count.
  #add the posterior means
#after fitting the model we want to plot the posterior means on top, should all shrink towards the overall mean because of the effect of pooling. Add posterior intervals as well.
fit_plot <- fit_final %>% 
  filter(is.na(q_rt_standard) == FALSE)
  points(fit_plot$q_rt_standard, fit_plot$exp_mean, col = "purple", pch = 19)
  segments(fit_plot$q_rt_standard,
           fit_plot$exp_minconf,
           fit_plot$q_rt_standard,
           fit_plot$exp_maxconf,
           col = "purple")
  