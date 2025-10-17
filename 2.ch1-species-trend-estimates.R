########################################
#
# Modeling for chapter one species trends
# models all the species together and predicts 
# b*year slope using species traits
# to ask about their influence on winners/losers
# at the local level here in the Triangle
# traits are:
# diet category
# temperature niche position
# habitat selectivity
# regional species trend
#
########################################

#get the functions we need for this script that are stored elsewhere
#some variables of importance as well eg. excluded species
source("2.species-trend-estimate-functions.R")

#libraries
library(beepr) #beeps
library(dplyr) #data manip
library(tidyr) #data manip
library(rstan) #stan
library(stringr) #data manip
library(StanHeaders) #stan helper

#prevent scientific notation to make a trend table easier to read
options(scipen=999)

#---------------------------------------------------------------------------
# Bayes model, based initially on Link and Sauer 2001 - it has changed a decent bit since then tbh
  # https://academic.oup.com/auk/article/128/1/87/5149447
# Modeling help from review of DiCecco Pop Trends code
  # https://github.com/gdicecco/poptrends_envchange/tree/master
#------------------------------------------------------------------------------

#read in analysis file, this has already been filtered to remove species without the minimum number of sightings or that are excluded species
mbbs <- read.csv("data/analysis.df.csv", header = TRUE) 

  #routes with errors
  temp_rm <- mbbs %>% filter(route == "drhm-04", year == 2010)
  
  #do everything we need to do! 
  mbbs <- mbbs %>%
    #remove route with error for missing surveyor data
    anti_join(temp_rm) %>%
    #remove eastern whip-por-will because it lacks ssi habitat selectivity data
    filter(!common_name == "Eastern Whip-poor-will") %>%
    #remove hummingbird because it's the only one of it's diet group
    filter(!common_name == "Ruby-throated Hummingbird") %>% 
    group_by(common_name) %>%
    mutate(common_name_standard = dplyr::cur_group_id()) %>%
    ungroup() %>%
    arrange(year, route_standard, common_name_standard) %>%
    #add traits
    #habitat niche
    left_join(y = (read.csv("data/species-traits/gdicecco-avian-range-shifts/habitat_niche_ssi_true_zeroes.csv") %>%
                     dplyr::select(english_common_name, ssi)),
              by = c("common_name" = "english_common_name")) %>%
    dplyr::rename(habitat_ssi = ssi) %>%
    #temperature niche position
    left_join(y = 
                (read.csv("data/species-traits/climate_position.csv") %>%
                   dplyr::select(common_name, z_tempwq)),
              by = "common_name") %>%
    #diet category
    left_join(y = 
                read.csv("data/species-traits/avonet_filtered.csv") %>%
                dplyr::select(common_name, Trophic.Niche) %>%
                dplyr::rename(avonet_diet = Trophic.Niche), 
              by = "common_name") %>%
    group_by(avonet_diet) %>%
    mutate(diet_category_standard = cur_group_id()) %>%
    ungroup() %>%
    #regional trend
    left_join(read.csv("data/bbs-regional/species-list-usgs-regional-trend.csv"), by = "common_name") %>%
    dplyr::mutate(usgs_sd = (.$usgs_97.5CI - .$usgs_2.5CI)/(2 * qnorm(0.95))) %>%
    #remove extra columns
    dplyr::select(-usgs_note) %>%
    #center variables as possible :)
    mutate(scale_habitat_ssi = (habitat_ssi - mean(habitat_ssi)/sd(habitat_ssi)),
           scale_ztempwq = (z_tempwq - mean(z_tempwq))/sd(z_tempwq),
           scale_obs_quality = (observer_quality - mean(observer_quality))/sd(observer_quality))
  
# 
# #add traits
# mbbs_traits <- add_all_traits(mbbs) %>%
#   mutate(sprt = paste0(route,common_name)) %>%
#   group_by(sprt) %>%
#   mutate(sprt_standard = cur_group_id()) %>%
#   ungroup()

#filtered mbbs, 5 species for testing purposes.
filtered_mbbs <- make_testing_df(mbbs)

#change to filtered_mbbs for testing, mbbs for the real thing
mbbs_dataset <- filtered_mbbs
#where to save stan code and fit
save_to <- "Z:/Goulden/mbbs-analysis/model/2025.08.29_newch1model1_log_trytovectorize/"
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
  dplyr::select(common_name_standard, habitat_ssi:scale_ztempwq) %>%
  distinct(common_name_standard, .keep_all = TRUE) 
  write.csv(traits, paste0(save_to, "species_traits.csv"), row.names = FALSE)

#prepare the data list for Stan
datstan <- list(
  N = nrow(mbbs_dataset), #number of observations
  Nsp = length(unique(mbbs_dataset$common_name_standard)), #n species
  Nrt = length(unique(mbbs_dataset$route_standard)), #n routes
  Nyr = length(unique(mbbs_dataset$year_standard)), #n years
  Ndc = length(unique(mbbs_dataset$diet_category_standard)), #n diets
  sp = mbbs_dataset$common_name_standard, #species indices for each observation
  rt = mbbs_dataset$route_standard, #route indices for each observation
  year = mbbs_dataset$year_standard, #year indices
  observer_quality = mbbs_dataset$scale_obs_quality, #measure of observer quality, 
  #NOW CENTERNED
  #used to not be CENTERED and maybe should be? Right now there are still negative and positive observer qualities, but these are ''centered'' within routes. Actually I think this is fine non-centered, because the interpretation is that the observer observes 'quality' species of birds more or less than any other observer who's run the route. Only way it could not be fine is bc it's based on each individual route, but the observer is actually judged cross-routes.
  t_temp_pos = traits$scale_ztempwq,
  t_habitat_selection = traits$scale_habitat_ssi,
  t_diet_cat = traits$diet_category_standard,
  regional_trend_mean = traits$usgs_trend_estimate,
  regional_trend_sd = traits$usgs_sd,
  C = mbbs_dataset$count #count data
)

#specify the stan model code
stan_model_file <- "2.ch1_active_development_model.stan"

#compile the stan model
stan_model <- stan_model(file = stan_model_file)
beepr::beep()
#save the model text
file.copy(stan_model_file, save_to, overwrite = TRUE)

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
  relocate(rownames, .before = mean) %>%
  #rename numeric columns
  rename_with(~ paste0("conf_", .), .cols = matches("^[0-9]")) %>%
  #remove %s in column names
  rename_with(~ str_remove(., "%"), .cols = everything())
#Save the summary
write.csv(fit_final, paste0(save_to,"fit_summary.csv"), row.names = FALSE)

#last model run, 3051 seconds, 22 second warm up time
#adding
#vector[N] eta = a + a_sp[sp] + a_rt[rt] + b[sp] .* year + c_obs * observer_quality;
#C ~ neg_binomial_2_log(eta, overdispersion_param);
#new model run:
# 591.515 seconds
# 22 second warmup time, like the exact same. No difference

#####################

#effort numbers for chapter 1
route_years <- mbbs %>% distinct(year, route)
nrow(route_years)

abundant_sp <- mbbs %>% 
  group_by(common_name) %>%
  summarize(total_count = sum(count)) %>%
  arrange(desc(total_count))
abundant_sp[1:5,]
abundant_sp[55:59,]
nrow(abundant_sp)
sum(abundant_sp$total_count)

####################













### NEXT STEP: Predict difference between regional and local trends!

#make new variables
  #pull posterior samples for the betas from the stanfit.
  #load in stanfit if needed...
  #fit <- readRDS(paste0(save_to, "stanfit.rds"))
  
  #posterior <- extract(fit, pars = "b")
  posterior <- as.data.frame(fit) %>%
    dplyr::select("b[1]":"b[61]") %>% #or select("b[1]:gamma_b") and then -gamma_b
  #keep only the betas
    pivot_longer(cols = "b[1]":"b[61]", names_to = "beta", values_to = "posterior") %>%
    mutate(common_name_standard_p = stringr::str_extract(beta, "(?<=\\[)([^]]+)(?=\\])"),
           common_name_standard_p = as.integer(common_name_standard_p)) %>%
    arrange(common_name_standard_p) %>%
    #regional is on a dif scale, let's multiply to that same scale.
    mutate(posterior = posterior*100) #great.
  #pivot 
  #^ why do we do the as.data.frame and don't have to like, extract anything?


  #bootstrap a regional trend to use to predict the difference between beta and regional with traits
  num_bootstrap <- nrow(posterior %>% dplyr::filter(common_name_standard == 1))
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
    ungroup() %>%
    arrange(common_name_standard)
  
  #notably, we will NOT leftjoin these. just check they're the same length
  assertthat::assert_that(
    nrow(bootstrap_regional) == nrow(posterior)
  )
  #and we've already sorted them the same so we're OK to bind columns
  assertthat::assert_that(
    sum(dif_data$common_name_standard == dif_data$common_name_standard_p) == 244000
  ) #checks that they're sorted the same, all the common names standards match for each sample.
  
  
  dif_data <- bind_cols(bootstrap_regional, posterior)  %>%
    mutate(D = bootstrapped_regional - posterior) 
  
  #For traits as well... we should be bootstrapping samples from the habitat selection and climate_position standard deviations. It's information we have and that we can add to this model. It matters if species have a wide esimate for habitat selection or not, and same with climate_position. We can, within this stan model, ''recreate'' these variables and build in that uncertainty.
  
  datstan_dif <- list(
    #unique for calculating the difference between regional and local trends
    Nsp = length(unique(dif_data$common_name)),
    N = nrow(dif_data), #number of bootstrap obs overall
    Nsp_sample = num_bootstrap, #number of regional and b samples per species
    sample_regional = dif_data$bootstrapped_regional, #regional trend for each bootstrapped observation
    sp = dif_data$common_name_standard, #species index for each bootstrapped observation
    sample_local = dif_data$posterior,
    D = dif_data$D,
    #traits!
    t_climate_pos = traits$climate_position, #! NOT CENTERED YET
    t_habitat_selection = traits$habitat_selection, #! NOT CENTERED YET
    sp_t = traits$common_name_standard #species for each trait
  )
  

#linear model to check inutition
  
  dif_intuit <- dif_data %>%
    left_join(traits, by = "common_name_standard")
  
  dif_model <- glm(D ~ habitat_selection + climate_position, data = dif_intuit)
    plot(dif_intuit$habitat_selection, dif_intuit$D)
      # Extract coefficients from the model
      coefs <- coef(dif_model)
      
      # Calculate the mean of climate_position to hold it constant
      mean_climate_position <- mean(dif_intuit$climate_position, na.rm = TRUE)
      
      # Add the regression line for habitat_selection, holding climate_position at its mean
      abline(a = coefs[1] + coefs[3] * mean_climate_position, b = coefs[2], col = "red")
    
    plot(dif_intuit$climate_position, dif_intuit$D)
      # Calculate the mean of habitat_selection to hold it constant
      mean_habitat_selection <- mean(dif_intuit$habitat_selection, na.rm = TRUE)
      abline(a = coefs[1] + coefs[2] * mean_habitat_selection, b = coefs[3], col = "red")
  
  summary(dif_model)
  
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
  
