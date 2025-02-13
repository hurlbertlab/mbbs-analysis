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
save_to <- "Z:/Goulden/mbbs-analysis/model/2025.02.11_Changing_indexing/"
#if the output folder doesn't exist, create it
if (!dir.exists(save_to)) {dir.create(save_to)}

#write the list of species + their species code
mbbs_dataset %>%
  dplyr::select(common_name_standard, common_name) %>%
  distinct() %>%
  write.csv(paste0(save_to, "beta_to_common_name.csv"), row.names = FALSE)

#get traits so it's one trait per species, so we can put this in datlist
#this is in the same order as the mbbs_dataset. Still, check intuition works?
traits <- mbbs_dataset %>%
  dplyr::select(common_name_standard, shannonE_diet:sprt_standard) %>%
  distinct(common_name_standard, .keep_all = TRUE)

#make sp_sprt, we make it separate because it's shorter and requires some manuvering
sp_sprt_base <- mbbs_dataset %>% 
  select(common_name_standard, sprt_standard) %>%
  distinct()


#prepare the data list for Stan
datstan <- list(
  N = nrow(mbbs_dataset), #number of observations
  Nsp = length(unique(mbbs_dataset$common_name_standard)), #n species
  Nsprt = length(unique(mbbs_dataset$sprt_standard)), #n species route combos
  Nyr = length(unique(mbbs_dataset$year_standard)), #n years
  sp = mbbs_dataset$common_name_standard, #species indices
  sprt = mbbs_dataset$sprt_standard, #species route indices
  sp_sprt = sp_sprt_base$common_name_standard, #species for each species route
  year = mbbs_dataset$year_standard, #year indices
  observer_quality = mbbs_dataset$observer_quality, #measure of observer quality, NOT CENTERED and maybe should be? Right now there are still negative and positive observer qualities, but these are ''centered'' within routes. Actually I think this is fine non-centered, because the interpretation is that the observer observes 'quality' species of birds more or less than any other observer who's run the route. Only way it could not be fine is bc it's based on each individual route, but the observer is actually judged cross-routes.
  #observer_ID = mbbs_dataset$observer_ID, #observer index
  #O = length(unique(mbbs_dataset$observer_ID)), #n observers
  #trait_diet = mbbs_dataset$shannonE_diet, #! NOT CENTERED YET
  #trait_climate = mbbs_dataset$climate_vol_2.1, #! NOT CENTERED YET
  #trait_habitat = mbbs_dataset$habitat_ssi, #! NOT CENTERED YET
  t_climate_pos = traits$climate_position, #! NOT CENTERED YET
  t_habitat_selection = traits$habitat_selection, #! NOT CENTERED YET
  t_regional = traits$usgs_trend_estimate,
  sp_t = traits$common_name_standard, #species for each trait
  C = mbbs_dataset$count #count data
)

#specify the stan model code
stan_model_code <- "
data {
  int<lower=0> N; // number of observations or rows
  int<lower=1> Nsp; // number of species
  int<lower=1> Nsprt; // number of species+route combinations
  int<lower=1> Nyr; //number of years
  array[N] int<lower=1, upper=Nsp> sp; //species id for each observation
  array[N] int<lower=1, upper = Nsprt> sprt; //species+route combo for each observation
  array[Nsprt] int<lower=1, upper=Nsp> sp_sprt; //species id for each species+route combo
  //note: the 'for each x' that 'x' is what the array length is.
  array[N] int<lower=1, upper=Nyr> year; //year for each observation
  vector[N] observer_quality; //there is an observer_quality for each observation..but not really! 
//...........................................
//when I back back in observer intercepts or w/e...
// int<lower=1> Nobs; //number of observers
// array[N] int<lower=1, upper=Nobs> obs; //there is an observer for every observation
// vector[obs] observer_quality; //there is an observer_quality for every observer
//...........................................
  array[N] int<lower=0> C; // there is a count (my y variable!) for every row, and it is an unbounded integer that is at least 0.
//.................okay, now for the predictor variables.....................
  array[Nsp] int<lower=1, upper = Nsp> sp_t; //species ID to associate with each species trait
  vector[Nsp] t_regional; //regional trait value for every species 
  vector[Nsp] t_climate_pos; //climate position value for every species 
  vector[Nsp] t_habitat_selection; //ndvi habitat selection for every species
  //here, you give it the LENGTH of the vector (Nsp), eg. same as the number of species. Later, in the model section, you give it the SPECIES (sp)
  
}

parameters {
  vector[Nsp] b; //species trend, fit one for each species
  vector[Nsprt] a; //species trend along a specific route, fit one for each sp+rt combo
  vector[Nsp] a_bar; // the intercept eg. initial count at yr 0, fit one per species
  real<lower=0> sigma_a; //standard deviation in a
  real gamma_b; //intercept for species trends. calculated across species, and we only want one value, so this is not a vector.
  real kappa_regional; //effect of t_regional on betas. real b/c we only want one.
  real kappa_climate_pos; //effect of t_climate_pos on betas. real b/c we only want one.
  real kappa_habitat_selection; //effect of t_habitat_selection on betas. real b/c we only want one.
  real<lower=0> sig_b; //deviation from explanatory power of the traits on predicting the trends. Represents residual variance / measure of scatter. ...In some ways, R2??
  
}

model {

// Non-vectorized, so slower than it could be. Let's ignore speed and work on content.
   for (n in 1:N) {
     C[n] ~ poisson_log(a[sprt[n]] + b[sp[n]] * year[n] + observer_quality[n]);
   }
// eg... for every row/observation in the data.
// The count is a function of the poisson distribution log(lambda), and lamda modeled by (literally subbed in, didn't bother with a lambda intermediary step) the species trend along a species+route combo, the b*year overall trend, and observer quality.

  a ~ normal(a_bar[sp_sprt], sigma_a); //sp_sprt maps species+route combos to species
  a_bar ~ normal(1, 0.5); //bc a_bar is a vector of sp_sprt, fits one for each sp.
  sigma_a ~ exponential(1);
  
  b ~ normal(gamma_b + 
             kappa_regional*t_regional[sp_t] +
             kappa_climate_pos*t_climate_pos[sp_t] +
             kappa_habitat_selection*t_habitat_selection[sp_t],
             sig_b);
  gamma_b ~ normal(0, 0.2);
  sig_b ~ exponential(1);
//.............BY COMMENTING OUT KAPPAS, DEFAULT UNIFORM PRIORS ARE USED.................
//  kappa_regional ~ normal(0, .02); 
//  kappa_climate_pos ~ normal(0, .2); 
//  kappa_habitat_selection ~ normal(0, .2);
//.............^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^..................
}
"

#compile the stan model
stan_model <- stan_model(model_code = stan_model_code)
beepr::beep()

#save stan model text if it compiled without errors
cmdstanr::write_stan_file(code = stan_model_code, dir = save_to)

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
#####################

#linear model to check inutition
  #need one record for every species
  mbbs_test <- mbbs_traits %>%
    distinct(common_name, .keep_all = TRUE) %>%
    mutate(common_name_standard = as.character(common_name_standard))
  
  #get trend from fit_final
  #ideally this ought to be from the model we just ran, but for testing purposes:
  fit_final <- read.csv("data/STAN_output_localtraits2025.01.28.csv") 
  data_test = fit_final %>%
    mutate(b = stringr::str_detect(fit_final$rownames, "b\\["),
           withinbracket = stringr::str_extract(fit_final$rownames, "(?<=\\[)([^]]+)(?=\\])")) %>%
    filter(b == TRUE) %>%
    #add in species traits
    left_join(mbbs_test, by = c("withinbracket" = "common_name_standard"))
  
  #run simple glm
  m <- glm(mean ~ habitat_selection + usgs_trend_estimate + climate_position, data = data_test, family = gaussian)

  summary(m) #so, yeah - at the Least regional should come out as significant. From a linear model I've run just on trends + regional_trends, ought to explain about 60% of the variation.
  


#extract posterior samples
#post <- extract(fit)