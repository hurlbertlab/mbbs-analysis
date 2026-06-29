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
source("2.analysis-functions.R")

project_directory <- "/proj/hurlbertlab/ijbgoulden/mbbs-analysis/"

#where to save stan code and fit
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
save_to <- paste0(project_directory,"STAN_outputs/2026.06.26_ch1_rmNOBO_2024rt/")
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
print(save_to)
#if the output folder doesn't exist, create it
if (!dir.exists(save_to)) {dir.create(save_to)}

#What type of model run is this?
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
run_type = "sensitivity_analysis"
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  #options are:
  #full_run with all species
  #testing_run with 6 species
  #sensitivity_analysis with outlier species removed (e.g. Northern Bobwhite)


#libraries
#library(beepr) #beeps
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
  
  #do everything we need to do! 
  mbbs <- mbbs %>%
    #remove eastern whip-por-will because it lacks ssi habitat selectivity data
    filter(!common_name == "Eastern Whip-poor-will") %>%
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
    #percent insectivory
    left_join(y = (read.csv("data/species-traits/fraction_diet_arthropods.csv") %>%
                              dplyr::select(common_name, Final_Fraction_Diet_Wt)),
              by = c("common_name")) %>%
    #regional trend
    left_join(read.csv("data/bbs-regional/species-list-usgs-regional-trend-2024.csv"), by = "common_name") %>%
    dplyr::mutate(usgs_sd = (.$usgs_97.5CI - .$usgs_2.5CI)/(2 * qnorm(0.95))) %>%
    #regional trend is on a different scale from our result, so we'll re-scale by dividing by 100 (current 2.01 = 2% change, want .02 = 2% change in pop per year)
    dplyr::mutate(usgs_trend_estimate = usgs_trend_estimate/100,
                  usgs_sd = usgs_sd/100) %>%
    #remove extra columns
    dplyr::select(-usgs_note) %>%
    #center variables as possible :)
    mutate(scale_habitat_ssi = (habitat_ssi - mean(habitat_ssi))/sd(habitat_ssi),
           scale_ztempwq = (z_tempwq - mean(z_tempwq))/sd(z_tempwq),
           scale_obs_quality = (observer_quality - mean(observer_quality))/sd(observer_quality),
           scale_insect_perc = ((Final_Fraction_Diet_Wt - mean(Final_Fraction_Diet_Wt))/sd(Final_Fraction_Diet_Wt)),
           scale_usgs_trend = (usgs_trend_estimate - mean(usgs_trend_estimate))/sd(usgs_trend_estimate),
           scale_usgs_sd = usgs_sd/sd(usgs_trend_estimate)
    )
# 
# #add traits
# mbbs_traits <- add_all_traits(mbbs) %>%
#   mutate(sprt = paste0(route,common_name)) %>%
#   group_by(sprt) %>%
#   mutate(sprt_standard = cur_group_id()) %>%
#   ungroup()


#set the mbbs_dataset based on the run type (full_run, filtered_run, or sensitivity_analysis)
if(run_type == "full_run") {
  
  mbbs_dataset <- mbbs
  
} else if(run_type == "filtered_run") {
  
  mbbs_dataset <- make_testing_df(mbbs,
                                  filter_species_to = c("Wood Thrush", "Acadian Flycatcher", "Northern Bobwhite", "White-eyed Vireo", "Tufted Titmouse"))
  #5 species for testing purposes
  
} else if(run_type == "sensitivity_analysis") {
  
  #get species to remove
  #species_to_remove <- read.csv("data/species-traits/ch1-sensitivity-analysis/species_to_remove.csv")
  #Just going to remove bobwhite, it's x2 more extreme than any other species, clearly in a class of it's own in terms of declines, and is exerting a lot of influence
  #over both the Temperature Niche and Percent Insectivory slopes.
  
  mbbs_dataset <- make_testing_df(mbbs,
                                  remove_species = "Northern Bobwhite")
  
} else {
  
  paste("ERROR, run type not specified")
  return(NULL)
  
}

#write the list of species + their species code
beta_to_common_name <- mbbs_dataset %>%
  dplyr::select(common_name_standard, common_name) %>%
  distinct() 
  write.csv(beta_to_common_name, paste0(save_to, "beta_to_common_name.csv"), row.names = FALSE)

#get traits so it's one trait per species, so we can put this in datlist
#this is in the same order as the mbbs_dataset. Still, check intuition works?
traits <- mbbs_dataset %>%
  dplyr::select(common_name_standard, habitat_ssi:scale_usgs_sd) %>%
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
  t_diet = traits$scale_insect_perc,
  regional_trend_mean = traits$scale_usgs_trend,
  regional_trend_sd = traits$scale_usgs_sd,
  #regional_trend_mean = traits$usgs_trend_estimate,
  #regional_trend_sd = traits$usgs_sd,
  C = mbbs_dataset$count #count data
)

#specify the stan model code, whether this is a run of our base model or model with an interaction term
stan_model_file <- paste0(project_directory, "2.ch1_m1.stan")
stan_m2_file <- paste0(project_directory, "2.ch1_m2_noregional.stan")
stan_m3_file <- paste0(project_directory, "2.ch1_m3_pooling.stan")

print("about to start to compile model")

#UNCOMMENT THE MODEL YOU ARE ABOUT TO USE
#RECOMMENT AFTER RUN STARTS TO PREVENT ERRORS LATER

#compile and save the stan model MODEL 1
stan_model <- stan_model(file = stan_model_file)
file.copy(stan_model_file, save_to, overwrite = TRUE)

#compile and save stan model MODEL 2
#stan_model <- stan_model(file = stan_m2_file)
#file.copy(stan_m2_file, save_to, overwrite = TRUE)

#compile and save stan model MODEL 3
#stan_model <- stan_model(file = stan_m3_file)
#file.copy(stan_m3_file, save_to, overwrite = TRUE)

print("model compiled")

timestamp()
print("start model fit")
#fit the model to the data
#options(mc.cores = parallel::detectCores())
fit <- sampling(stan_model, 
                data = datstan, 
                chains = 4,
                cores = 4, 
                iter = 10000, #10,000 
                warmup = 2000 #2,000
                )
print("model fit success")

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
  rename_with(~ str_remove(., "%"), .cols = everything()) %>%
  filter(!stringr::str_detect(.$rownames, "intercept"))
#Save the summary
write.csv(fit_final, paste0(save_to,"fit_summary.csv"), row.names = FALSE)

print("saved fit summary")
timestamp()

post <- rstan::extract(fit, inc_warmup = FALSE) %>%
        as.data.frame() %>%
        dplyr::select(starts_with("b"), starts_with("k"), starts_with("g"), starts_with("sig")) %>%
  mutate(row_id = row_number()) %>%
  dplyr::filter(row_id <= 5000)

write.csv(post, paste0(save_to, "posterior_draws.csv"), row.names = FALSE)
print("saved posterior samples")

#Save the traceplots of the fit
#save_stan_traceplot_pdf(fit,
#                    file = paste0((save_to), "traceplots.pdf"),
#                    pars = NULL,
#                    n_per_page = 8,
#                    remove_pars = "eta\\[[0-9]+\\]|lp__|a_sprt(_raw)?\\[[0-9]+\\]")
#that remove pars is just not working and it is saving EVERY variable. Commenting out bc in prev. iterations we've confirmed
#no traceplot issues.
                    
#examine pairs plot
#print("trying to save pairs plot")
#pdf(file = paste0(save_to, "pairsplot.pdf")
#print(pairs(fit))
#dev.off()
#print("pairs plot saved")

#Save the fit
#saveRDS(fit, paste0(save_to, "stanfit.rds"))
#print("saved full fit")
#print("Done!")
