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

#read in analysis file 
mbbs <- read.csv("data/analysis.df.csv", header = TRUE) %>%
  dplyr::select(-weather, -vehicles, -notes, -hab_hm, -hab_p, -hab_o, -hab_b, -hab_other, -S, -N, - month, -day, -species_comments)

#create testing dataset also
filtered_mbbs <- mbbs %>% filter(common_name == "Wood Thrush" | common_name == "Acadian Flycatcher") %>%
  #Recreate IDs for common name
  group_by(common_name) %>%
  mutate(common_name_standard = cur_group_id()) %>%
  ungroup() %>%
  #Recreate IDs for primary observer
  group_by(primary_observer) %>%
  mutate(observer_ID = cur_group_id()) %>%
  ungroup() 

#change to filtered_mbbs for testing, mbbs for the real thing
mbbs_dataset <- mbbs
#where to save stan code and fit
save_to <- "Z:/Goulden/mbbs-analysis/model/simple_bayes_observer_only/"
#if the output folder doesn't exist, create it
if (!dir.exists(save_to)) {dir.create(save_to)}

#write the list of species + their species code
mbbs_dataset %>%
  dplyr::select(common_name_standard, common_name) %>%
  distinct() %>%
  write.csv(paste0(save_to, "beta_to_common_name.csv"), row.names = FALSE)

#create datalist for stan
datstan <- list(
  N = nrow(mbbs_dataset), #number of observations
  S = length(unique(mbbs_dataset$common_name_standard)), #n species
  R = length(unique(mbbs_dataset$route_standard)), #n routes
  Y = length(unique(mbbs_dataset$year_standard)), #n years
  species = mbbs_dataset$common_name_standard, #species indices
  route = mbbs_dataset$route_standard, #route indicies
  year = mbbs_dataset$year_standard, #year indices
  observer_quality = mbbs_dataset$observer_quality, #measure of observer quality, NOT CENTERED and maybe should be? Right now there are still negative and positive observer qualities, but these are ''centered'' within routes. Actually I think this is fine non-centered, because the interpretation is that the observer observes 'quality' species of birds more or less than any other observer who's run the route. Only way it could not be fine is bc it's based on each individual route, but the observer is actually judged cross-routes.
  C = mbbs_dataset$count #count data
)

#specify stan model code
stan_model_code <- "
data {
  int<lower=0> N; // number of rows
  int<lower=1> S; // number of species
  int<lower=1> R; // number of routes
  int<lower=1> Y; // number of years
  array[N] int<lower=1, upper=S> species; // there is a species for every row and it's an integer between 1 and S
  array[N] int<lower=1, upper=R> route;  // there is a route for every row and it's an integer between 1 and R
  array[N] int<lower=1, upper=Y> year;  // there is an integer for every year and it's an integer between 1 and Y
 vector[N] observer_quality;  // there is an observer quality for every row and it is a real number, because it is continuous it can be a vector instead of an array
  array[N] int<lower=0> C;  // there is a count (my y variable!) for every row and it is an unbounded integer that is at least 0.
}

parameters {
  vector[S] b; //species trend
  matrix[R, S] a; //species trend along a specific route
  vector[S] a_bar; //the intercept eg. initial count at yr 0 along a route, is allowed to vary by species
  real<lower=0> sigma_a; //standard deviation in a
  real gamma_b; //intercept 
  real<lower=0> sig_b;
  
}

model {
  a_bar ~ normal(1, 0.5);
  sigma_a ~ exponential(1);
  b ~ normal(gamma_b, sig_b); //without traits
  gamma_b ~ normal(0, 0.2);
  sig_b ~ exponential(1);
  to_vector(a) ~ normal(a_bar[S], sigma_a); //uses a species specific a_bar
  

// Non-vectorized, so slower than it could be. Let's ignore speed and work on content.
   for (n in 1:N) {
     C[n] ~ poisson_log(a[route[n], species[n]] + b[species[n]] * year[n] + observer_quality[n]);
   }
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
                warmup = 1000, 
                sample_file = save_to
)
#expect 23 minutes to run this model with the full data.
beepr::beep()

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


#Done, with everything saved :) for further work, assessing the quality of the fit, plotting, etc. see later files



























