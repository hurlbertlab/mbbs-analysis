#################################
#
# Mostly used for testing now, but code 
# with the simple observer only multilevel
# mbbs model.
#
#################################

library(beepr) #beeps
library(dplyr) #data manip
library(tidyr) #data manip
library(rstan) #stan
library(StanHeaders) #stan helper

#prevent scientific notation to make a trend table easier to read
options(scipen=999)

#source functions
source("2.species-trend-estimate-functions.R")

#read in analysis file 
mbbs <- read.csv("data/analysis.df.csv", header = TRUE) %>%
  #add species route
  mutate(sprt = paste0(route,common_name)) %>%
  group_by(sprt) %>%
  mutate(sprt_standard = cur_group_id()) %>%
  ungroup()

    #routes with errors
    temp_rm <- mbbs %>% filter(route == "drhm-04", year == 2010)
    mbbs <- anti_join(mbbs, temp_rm)

#create testing dataset also
filtered_mbbs <- make_testing_df(mbbs)

#change to filtered_mbbs for testing, mbbs for the real thing
mbbs_dataset <- filtered_mbbs
#where to save stan code and fit
save_to <- "Z:/Goulden/mbbs-analysis/model/2025.05.29_simple_bayes_observeronly_zscorea+obsqonly/"
#if the output folder doesn't exist, create it
if (!dir.exists(save_to)) {dir.create(save_to)}

#write the list of species + their species code
mbbs_dataset %>%
  dplyr::select(common_name_standard, common_name) %>%
  distinct() %>%
  write.csv(paste0(save_to, "beta_to_common_name.csv"), row.names = FALSE)

#make sp_sprt, we make it separate because it's shorter and requires some manuvering
sp_sprt_base <- mbbs_dataset %>% 
  select(common_name_standard, sprt_standard) %>%
  distinct()

#make observer quality seperate
#obs_q_base <- mbbs_dataset %>%
#  distinct(observer_ID,observer_quality) %>%
#  select(observer_quality)

#create datalist for stan
datstan <- list(
  N = nrow(mbbs_dataset), #number of observations
  Nsp = length(unique(mbbs_dataset$common_name_standard)), #n species
  Nsprt = length(unique(mbbs_dataset$sprt_standard)), #n species route combos
  Nyr = length(unique(mbbs_dataset$year_standard)), #n years
  sp = mbbs_dataset$common_name_standard, #species indices
  sprt = mbbs_dataset$sprt_standard, #species route indices
  sp_sprt = sp_sprt_base$common_name_standard, #species for each species route
  year = mbbs_dataset$year_standard, #year indices
  observer_quality = mbbs_dataset$observer_quality, 
#  observer_quality = obs_q_base$observer_quality, #measure of observer quality, NOT CENTERED and maybe should be? Right now there are still negative and positive observer qualities, but these are ''centered'' within routes. Actually I think this is fine non-centered, because the interpretation is that the observer observes 'quality' species of birds more or less than any other observer who's run the route. Only way it could not be fine is bc it's based on each individual route, but the observer is actually judged cross-routes.
  #but on the other hand, is this a variable that is going to be interpreted? no. And centering takes this from relating to the number of species, to relating observers to each other. Also numerically it may make stan's job easier. 
  #with centering and scaling, obs_q becomes interpretable as how different each observer is from the mean observer - this is very easy to interpret, especially when we get to the final output and have a slope for observer_quality
#  Nobs = length(unique(mbbs_dataset$observer_ID)), #n observers
#  obs = mbbs_dataset$observer_ID, #observer index
  C = mbbs_dataset$count #count data
)

#specify stan model code
stan_model_code <- "
data {
  int<lower=0> N; // number of observations or rows
  int<lower=1> Nsp; // number of species
  int<lower=1> Nsprt; // number of species+route combinations
  int<lower=1> Nyr; //number of years
  array[N] int<lower=1, upper=Nsp> sp; //species id for each observation
  array[N] int<lower=1, upper = Nsprt> sprt; //species+route combo for each observation
  //note: the 'for each x' that 'x' is what the array length is.
  array[Nsprt] int<lower=1, upper=Nsp> sp_sprt; //species id for each species+route combo
  array[N] int<lower=1, upper=Nyr> year; //year for each observation
  vector[N] observer_quality; //there is an observer_quality for each observation..but not really! This is version w/o observer ID
//when I back back in observer intercepts or w/e...
//  int<lower=1> Nobs; //number of observers
//  array[N] int<lower=1, upper=Nobs> obs; //there is an observer for every observation
//  vector[Nobs] observer_quality; //there is an observer_quality for every observer
//...........................................
  array[N] int<lower=0> C; // there is a count (my y variable!) for every row, and it is an unbounded integer that is at least 0.
}

parameters {
  vector[Nsp] b; //species trend, fit one for each species
  vector[Nsprt] a_z; //intercept for each species+rt combo, z-score
  vector[Nsp] a_bar; // 'average q-route'; along each sp+rt combo. fit one for each species eg. this is allowed to vary by species. 
  real<lower=0> sigma_a; //standard deviation in a/variance from average route+species
  real gamma_b; //intercept for species trends. calculated across species, and we only want one value, so this is not a vector.
  real<lower=0> sig_b; //deviation from explanatory power of the traits on predicting the trends. Represents residual variance / measure of scatter. ...In some ways, R2??
//  vector[Nobs] c; //effect of observer, fit one for each observer
//  real gamma_c; //fit one intercept across observers. Let's keep this simple, bc we don't need to super complicate the role observers play
//  real kappa_obs; //fit one effect of observer quality
//  real <lower=0> sig_c; //deviation btwn observed offset for count and score I gave each observer
  
}

transformed parameters {

  vector[Nsprt] a = a_bar[Nsp] + a_z*sigma_a; //how to transform a, which is the species trend along a specific route, fit one for each sp+rt combo

}

model {

// Non-vectorized, so slower than it could be. Let's ignore speed and work on content.
   for (n in 1:N) {
     C[n] ~ poisson_log(a[sprt[n]] + b[sp[n]] * year[n] + observer_quality[n]);
   }

  a_z ~ normal(0,1); //partially pool the intercepts
  a_bar ~ normal(1, 0.5); //bc a_bar is a vector of sp_sprt, fits one for each sp.
  sigma_a ~ exponential(1);
  
  b ~ normal(gamma_b, sig_b); //without traits
  gamma_b ~ normal(0, 0.2);
  sig_b ~ exponential(1);
  
  //c ~ normal(gamma_c + kappa_obs*observer_quality, sig_c); //observer quality may need some indexing? 
  //getting rid of this, going to only use observer quality instead of fitting all the observers with their own kappa
  
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
#expect 23 minutes to run this model with the full data.
beepr::beep()
#3 minutes with the testing data.
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



























