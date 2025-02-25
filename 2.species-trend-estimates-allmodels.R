#file to have all the trend estimate modeling on one file, ideally just adding everything to one nice datasheet as I go

#get the functions we need for this script that are stored elsewhere
#some variables of importance as well eg. excluded species
source("2.species-trend-estimate-functions.R")
source("2.ste-datstan-lists.R")

#libraries
library(beepr) #beeps
library(dplyr) #data manip
library(tidyr) #data manip
#library(lme4) #modeling
#library(MASS) #modeling
#library(geepack) #GEE modeling
#library(mbbs) #data comes from here
#library(broom) #extracts coefficient values out of models, works with geepack (not using geepack rn)
library(rstan) #stan
library(StanHeaders) #stan helper
#library(rethinking) #alt. way to model with bayes, from Statistical Rethinking course. Can't use for final analysis bc the ulam helper function has some presets that don't fit my needs (eg. 87% standard deviation calculations)

#prevent scientific notation to make a trend table easier to read
options(scipen=999)

#------------------------------------------------------------------------------
# Bayes model, based initially on Link and Sauer 2001
  # https://academic.oup.com/auk/article/128/1/87/5149447
# Modeling help from review of DiCecco Pop Trends code
  # https://github.com/gdicecco/poptrends_envchange/tree/master
#------------------------------------------------------------------------------
#--------------------------------------------------------------------------
# Stan
#-------------------------------------------------------------------------

#read in analysis file 
mbbs <- read.csv("data/analysis.df.csv", header = TRUE) 

  #routes with errors
  temp_rm <- mbbs %>% filter(route == "drhm-04", year == 2010)
  mbbs <- anti_join(mbbs, temp_rm)

#add traits
mbbs_traits <- add_all_traits(mbbs) %>%
  mutate(sprt = paste0(route,common_name)) %>%
  group_by(sprt) %>%
  mutate(sprt_standard = cur_group_id()) %>%
  ungroup()

#filtered mbbs, 5 species for testing purposes.
filtered_mbbs <- make_testing_df(mbbs_traits)

#change to filtered_mbbs for testing, mbbs_traits for the real thing
mbbs_dataset <- filtered_mbbs
#where to save stan code and fit
save_to <- "Z:/Goulden/mbbs-analysis/model/2025.02.25_testing_regional_dif/"
#if the output folder doesn't exist, create it
if (!dir.exists(save_to)) {dir.create(save_to)}

#write the list of species + their species code
beta_to_common_name <- mbbs_dataset %>%
  dplyr::select(common_name_standard, common_name) %>%
  distinct() 
  write.csv(beta_to_common_name, paste0(save_to, "beta_to_common_name.csv"), row.names = FALSE)

#get traits so it's one trait per species, so we can put this in datlist
#this is in the same order as the mbbs_dataset. Still, check intuition works?
traits <- mbbs_dataset %>%
  dplyr::select(common_name_standard, shannonE_diet:sprt_standard) %>%
  distinct(common_name_standard, .keep_all = TRUE)

#make sp_sprt, we make it separate because it's shorter and requires some manuvering
sp_sprt_base <- mbbs_dataset %>% 
  select(common_name_standard, sprt_standard, route) %>%
  distinct()
  #while we're here, this ought to be part of beta_to_common_name.
  beta_to_common_name %>% 
    left_join(sp_sprt_base) %>%
    write.csv(paste0(save_to, "beta_to_common_name.csv"), row.names = FALSE) #now alphas can also be translated.

#make observer quality seperate
obs_q_base <- mbbs_dataset %>%
  distinct(observer_ID,observer_quality)

#prepare the data list for Stan
datstan <- list(
  N = nrow(mbbs_dataset), #number of observations
  Nsp = length(unique(mbbs_dataset$common_name_standard)), #n species
  Nrt = length(unique(mbbs_dataset$route_standard)), #n routes
  Nsprt = length(unique(mbbs_dataset$sprt_standard)), #n species route combos
  Nyr = length(unique(mbbs_dataset$year_standard)), #n years
  sp = mbbs_dataset$common_name_standard, #species indices for each observation
  rt = mbbs_dataset$route_standard, #route indices for each observation
#  sp_a = unique(mbbs_dataset$common_name_standard), #the unique species ids
  #sprt = mbbs_dataset$sprt_standard, #species route indices
  sp_sprt = sp_sprt_base$common_name_standard, #species for each species route
  year = mbbs_dataset$year_standard, #year indices
  observer_quality = obs_q_base$observer_quality, #measure of observer quality, NOT CENTERED and maybe should be? Right now there are still negative and positive observer qualities, but these are ''centered'' within routes. Actually I think this is fine non-centered, because the interpretation is that the observer observes 'quality' species of birds more or less than any other observer who's run the route. Only way it could not be fine is bc it's based on each individual route, but the observer is actually judged cross-routes.
  Nobs = length(unique(mbbs_dataset$observer_ID)), #n observers
  obs = mbbs_dataset$observer_ID, #observer index
  #trait_diet = mbbs_dataset$shannonE_diet, #! NOT CENTERED YET
  #trait_climate = mbbs_dataset$climate_vol_2.1, #! NOT CENTERED YET
  #trait_habitat = mbbs_dataset$habitat_ssi, #! NOT CENTERED YET
  t_climate_pos = traits$climate_position, #! NOT CENTERED YET
  t_habitat_selection = traits$habitat_selection, #! NOT CENTERED YET
  t_regional = traits$usgs_trend_estimate,
  sp_t = traits$common_name_standard, #species for each trait
  C = mbbs_dataset$count, #count data
  
)

#specify the stan model code
stan_model_file <- "2.active_development_model.stan"
stan_model_file <- "2.trends_predict_regional_local_difference_model.stan"

#compile the stan model
stan_model <- stan_model(file = stan_model_file)
beepr::beep()
#save the model text
file.copy(stan_model_file, save_to, overwrite = TRUE)



#save stan model text if it compiled without errors
#cmdstanr::write_stan_file(code = stan_model, dir = save_to)

#fit the model to the data
#options(mc.cores = parallel::detectCores())
fit <- sampling(stan_model, 
                data = datstan, 
                chains = 4,
                cores = 4, 
                iter = 2000, 
                warmup = 1000
                )
beepr::beep()

#view results
#Accessing the contents of a stanfit object:
#https://cran.r-project.org/web/packages/rstan/vignettes/stanfit-objects.html

#Save the fit
saveRDS(fit, paste0(save_to, "stanfit.rds"))

#get the summary, save as well
fit_summary <- summary(fit)
rownames <- row.names(fit_summary$summary)
fit_final <- as.data.frame(fit_summary$summary)
fit_final$rownames <- rownames  
fit_final <- fit_final %>%
  relocate(rownames, .before = mean)
#Save the summary
write.csv(fit_final, paste0(save_to,"fit_summary.csv"), row.names = FALSE)


View(fit_final) ##DO NOT HAVE THIS ON LONGLEAF


### NEXT STEP: Predict difference between regional and local trends!

#make new variables
  #pull posterior samples for the betas from the stanfit.
  #load in stanfit if needed...
  #fit <- readRDS(paste0(save_to, "stanfit.rds"))
  
  posterior <- as.data.frame(fit) %>%
    dplyr::select("b[1]":"b[61]") #or select("b[1]:gamma_b") and then -gamma_b
  #keep only the betas
  #pivot 
  #^ why do we do the as.data.frame and don't have to like, extract anything?


  #bootstrap a regional trend to use to predict the difference between beta and regional with traits
  num_bootstrap <- nrow(posterior)
  bootstrap_regional <- mbbs_dataset %>%
    dplyr::select(common_name, common_name_standard, usgs_trend_estimate:usgs_startyear) %>%
    group_by(common_name) %>%
    distinct() %>%
    ungroup() %>%
    mutate(sd = ((usgs_97.5CI - usgs_2.5CI)/(2*1.96))) %>%
    group_by(common_name, common_name_standard) %>%
    dplyr::summarize(
      bootstrapped_regional = list(rnorm(num_bootstrap, mean = usgs_trend_estimate, sd = sd))
    ) %>%
    unnest(bootstrapped_regional) %>%
    ungroup()
  
  datstan_dif <- list(
    #unique for calculating the difference between regional and local trends
    Nboot = nrow(bootstrap_regional), #number of bootstrap obs overall
    Nboot_sample = num_bootstrap, #number of regional and b samples per species
    boot_regional = bootstrap_regional$bootstrapped_regional, #regional trend for each bootstrapped observation
    boot_sp = bootstrap_regional$common_name_standard #species index for each bootstrapped observation
  )
  

#####################

#linear model to check inutition
  #need one record for every species
  mbbs_test <- mbbs_traits %>%
    distinct(common_name, .keep_all = TRUE) %>%
    mutate(common_name_standard = as.character(common_name_standard))
  
  #get trend from fit_final
  #ideally this ought to be from the model we just ran, but for testing purposes:
load_from <- "Z:/Goulden/mbbs-analysis/model/2025.02.11_allspecies_traits_index_fixed/"
  fit_final <- read.csv(paste0(load_from, "fit_summary.csv")) 
  data_test = fit_final %>%
    mutate(b = stringr::str_detect(fit_final$X, "b\\["),
           withinbracket = stringr::str_extract(fit_final$X, "(?<=\\[)([^]]+)(?=\\])")) %>%
    filter(b == TRUE) %>%
    #add in species traits
    left_join(mbbs_test, by = c("withinbracket" = "common_name_standard"))
  
  #run simple glm
  m <- glm(mean ~ habitat_selection + usgs_trend_estimate + climate_position, data = data_test, family = gaussian)

  summary(m) #so, yeah - at the Least regional should come out as significant. From a linear model I've run just on trends + regional_trends, ought to explain about 60% of the variation.
  


#extract posterior samples
#post <- extract(fit)