#################################
#
# Mostly used for testing now, but code 
# with the simple observer only multilevel
# mbbs model.
# adding regional trend to figure out how to implement sampling from it
#
#################################

library(beepr) #beeps
library(dplyr) #data manip
library(tidyr) #data manip
library(rstan) #stan
library(stringr)
library(StanHeaders) #stan helper

#prevent scientific notation to make a trend table easier to read
options(scipen=999)

#source functions
source("2.species-trend-estimate-functions.R")

#read in analysis file 
mbbs <- read.csv("data/analysis.df.csv", header = TRUE) 

    #routes with errors
    temp_rm <- mbbs %>% filter(route == "drhm-04", year == 2010)
    mbbs <- anti_join(mbbs, temp_rm) %>%
      #add species route
      mutate(sprt = paste0(route,common_name)) %>%
      group_by(sprt) %>%
      mutate(sprt_standard = cur_group_id()) %>%
      ungroup() %>%
      #add regional trend
      left_join(read.csv("data/bbs-regional/species-list-usgs-regional-trend.csv"), by = "common_name")# %>%
      #dplyr::mutate(usgs_sd = (.$usgs_97.5CI - .$usgs_2.5CI)/(2 * qnorm(0.95)))
    
    #..for some reason my mutate to do this within the funciton isn't working? humph. not really sure why it's not. 
    mbbs$usgs_sd = (mbbs$usgs_97.5CI - mbbs$usgs_2.5CI)/(2*qnorm(.95))


#create testing dataset also
filtered_mbbs <- make_testing_df(mbbs, obs_only = TRUE)

#change to filtered_mbbs for testing, mbbs for the real thing
mbbs_dataset <- filtered_mbbs
#where to save stan code and fit
save_to <- "Z:/Goulden/mbbs-analysis/model/2025.08.27_obsonly_withregional_testing_mu/"
#if the output folder doesn't exist, create it
if (!dir.exists(save_to)) {dir.create(save_to)}

#write the list of species + their species code
mbbs_dataset %>%
  dplyr::select(common_name_standard, common_name) %>%
  distinct() %>%
  write.csv(paste0(save_to, "beta_to_common_name.csv"), row.names = FALSE)

#get distinct regional trends
regional_trend <- mbbs_dataset %>%
  dplyr::select(common_name_standard, usgs_trend_estimate, usgs_sd) %>%
  distinct()

#We no longer have to make sp_sprt because we can add new intercepts for each species and route instead.
        #make sp_sprt, we make it separate because it's shorter and requires some manuvering
        #sp_sprt_base <- mbbs_dataset %>% 
        #  select(common_name_standard, sprt_standard) %>%
        #  distinct()

#make observer quality seperate
#obs_q_base <- mbbs_dataset %>%
#  distinct(observer_ID,observer_quality) %>%
#  select(observer_quality)

#create datalist for stan
datstan <- list(
  N = nrow(mbbs_dataset), #number of observations
  Nsp = length(unique(mbbs_dataset$common_name_standard)), #n species
  Nrt = length(unique(mbbs_dataset$route_standard)), #n routes
  Nyr = length(unique(mbbs_dataset$year_standard)), #n years
  sp = mbbs_dataset$common_name_standard, #species indices
  rt = mbbs_dataset$route_standard, #route indicies
  year = mbbs_dataset$year_standard, #year indices
  regional_trend_mean = regional_trend$usgs_trend_estimate,
  regional_trend_sd = regional_trend$usgs_sd,
  observer_quality = mbbs_dataset$observer_quality, 
#  observer_quality = obs_q_base$observer_quality, #measure of observer quality, NOT CENTERED and maybe should be? Right now there are still negative and positive observer qualities, but these are ''centered'' within routes. Actually I think this is fine non-centered, because the interpretation is that the observer observes 'quality' species of birds more or less than any other observer who's run the route. Only way it could not be fine is bc it's based on each individual route, but the observer is actually judged cross-routes.
  #but on the other hand, is this a variable that is going to be interpreted? no. And centering takes this from relating to the number of species, to relating observers to each other. Also numerically it may make stan's job easier. 
  #with centering and scaling, obs_q becomes interpretable as how different each observer is from the mean observer - this is very easy to interpret, especially when we get to the final output and have a slope for observer_quality
  C = mbbs_dataset$count #count data
)

#specify stan model code
stan_model_file <- "2.ch1_observer_only_model.stan"

#compile the stan model
stan_model <- stan_model(file = stan_model_file)
beepr::beep()
#save the model text
file.copy(stan_model_file, save_to, overwrite = TRUE)
print("model saved :)")


#fit the model to the data
#options(mc.cores = parallel::detectCores())
fit <- sampling(stan_model, 
                data = datstan, 
                chains = 4,
                cores = 4, 
                iter = 4000, 
                warmup = 2000
)
#expect 23 minutes to run this model with the full data.
beepr::beep()
#3 minutes with the testing data.
#hmmm yeah. With the new method of transforming the alphas the testing data now takes 7.5 minutes.
#getting ESS warnings. uhmmmm. Okay but it's still actually sampling the a's better than the last version I ran in terms of neff
#and the betas come out the exact same so success there. Like, I'm fussing with the intercepts but it's yeah, it's not changing the slopes from what I can tell. 
#!!!!!!!!!!!!!!! Actually, not true! b[3] is different, goes from being non-significant with a negative mean, to a small but significantly positive mean. But I mean, that could also be a result of low sampling?
#but yes the other ones are all still the same.

#when I remove the kappas, ESS no longer gives a warning, and rhats improved further :D

#Save the fit
saveRDS(fit, paste0(save_to, "stanfit.rds"))

#get the summary, save as well
fit_summary <- summary(fit) 
rownames <- row.names(fit_summary$summary)
fit_final <- as.data.frame(fit_summary$summary)
fit_final$rownames <- rownames  
fit_final <- fit_final %>%
  relocate(rownames, .before = mean) %>%
  #rename numeric columns
  rename_with(~ paste0("conf_", .), .cols = matches("^[0-9]")) %>%
  #remove %s in column names
  rename_with(~ str_remove(., "%"), .cols = everything())
#Save the summary
write.csv(fit_final, paste0(save_to,"fit_summary.csv"), row.names = FALSE)


#Done, with everything saved :) for further work, assessing the quality of the fit, plotting, etc. see later files



























