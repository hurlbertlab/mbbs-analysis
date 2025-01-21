#file to have all the trend estimate modeling on one file, ideally just adding everything to one nice datasheet as I go

#get the functions we need for this script that are stored elsewhere
source("species-trend-estimate-functions.R")

#libraries
library(beepr) #beeps
library(dplyr) #data manip
library(tidyr) #data manip
library(lme4) #modeling
library(MASS) #modeling
library(geepack) #GEE modeling
library(mbbs) #data comes from here
library(broom) #extracts coefficient values out of models, works with geepack
library(rstan) #stan
library(StanHeaders) #stan helper
#library(rethinking) #alt. way to model with bayes, from Statistical Rethinking course. Can't use for final analysis bc the ulam helper function has some presets that don't fit my needs (eg. 87% standard deviation calculations)


#prevent scientific notation to make a trend table easier to read
options(scipen=999)

#read in data - not in use right now. Data workflow where I'm making an analysis.df needs a reassessment
#mbbs <- read.csv("data/analysis.df.csv", header = T)

#get survey events from temporary load instead of from the current github version
#of the mbbs.
load("C:/git/mbbs-analysis/data/mbbs_survey_events.rda")
mbbs_survey_events <- mbbs_survey_events %>%
  mutate(primary_observer = case_when(max_qual_observer == 1 ~ obs1,
                                      max_qual_observer == 2 ~ obs2,
                                      max_qual_observer == 3 ~ obs3)) %>%
  group_by(primary_observer) %>%
  mutate(observer_ID = cur_group_id()) %>%
  ungroup()
#max_qual obs = 1, gets first observer,
    #where max qual obs =2, gets second observer,
    #where max qual obs =2 gets third observer

#set up a pipe to make it so that the max_qual_obs (contains a 1,2,3) becomes
#the primary obs
  

#read in data, using most updated versions of the mbbs. 
mbbs_orange <- mbbs_orange %>% standardize_year(starting_year = 2000)
mbbs_durham <- mbbs_durham %>% standardize_year(starting_year = 2000)
mbbs_chatham <- mbbs_chatham %>% standardize_year(starting_year = 2000)
mbbs <- bind_rows(mbbs_orange, mbbs_chatham, mbbs_durham) %>% #bind all three counties together
  mutate(route_ID = route_num + case_when(
    mbbs_county == "orange" ~ 100L,
    mbbs_county == "durham" ~ 200L,
    mbbs_county == "chatham" ~ 300L)) %>%
  filter(count > 0) %>%
  ungroup() %>%
  #remove the 1999 data, since it's only 1/3 of the routes that were created by then. 
  filter(year > 1999) %>%
  #clean up for ease of use, lots of columns we don't need rn
  dplyr::select(-sub_id, -tax_order, -count_raw, -state, -loc, -locid, -lat, -lon, -protocol, -distance_traveled, -area_covered, -all_obs, -breed_code, -checklist_comments, -source) %>%
  #create a route-standard from 1-34
  group_by(route_ID) %>%
  mutate(route_standard = dplyr::cur_group_id()) %>%
  ungroup()
#help keep the environment clean
rm(mbbs_chatham); rm(mbbs_durham); rm(mbbs_orange) 
#load in mbbs_survey_events from current branch (stop_num)
#load("C:/git/mbbs/data/mbbs_survey_events.rda")

#Data structure changes in 2019 to have stop-level records. We need to group together this data for analysis. We cannot use date here or it causes problems with future analysis. 
mbbs <- mbbs %>%
  group_by(common_name, year, route_ID) %>% 
  mutate(count = sum(count)) %>% 
  distinct(common_name, year, route_ID, count, .keep_all = TRUE) %>% #keep_all is used to retain all other columns in the dataset
ungroup() #this ungroup is necessary 

#filter out species that haven't been seen more than the min number of times on the min number of routes.
#9 to include bobwhite!
mbbs <- filter_to_min_sightings(mbbs, min_sightings_per_route = 9, min_num_routes = 5) %>%
  #filter out waterbirds and hawks
  filter(!common_name %in% excluded_species) %>%
  #create a variable so that has common name from 1:however many species are 
  #included
  group_by(common_name) %>%
  mutate(common_name_standard = dplyr::cur_group_id()) %>%
  ungroup() 

n_distinct(mbbs$common_name) #ok, 56 species rn make the cut with the borders set at 5 routes and 9 sightings on those routes. Nice!

#save a version of the mbbs before adding 0s
  mbbs_nozero <- mbbs
#add in the 0 values for when routes were surveyed but the species that remain in this filtered dataset were not seen.
 mbbs <- mbbs %>% complete(
  nesting(year, year_standard, mbbs_county, route_ID, route_num, route_standard),
  nesting(common_name, sci_name, common_name_standard),
  fill = list(count = 0))  

#check that no issues have occurred - all species should have the same number of occurences in the df 
 if(length(unique(table(mbbs$common_name))) == 1) { #all species have the same number of occurances
   print("All species have same number of occurances, good to continue")
   beep()
 } else {
     print("ERROR: Adding in the 0 values has led to some species having more occurances than others."); beep(11);}
 
#now that everything else is ready to go, leftjoin survey_events so we have observer information
mbbs <- add_survey_events(mbbs, mbbs_survey_events)
mbbs_nozero <- add_survey_events(mbbs_nozero, mbbs_survey_events)
  
# Remove Orange route 11 from 2012 due to uncharacteristically high counts from a one-time observer
#! In newer versions of the mbbs (that have to be downloaded from the website due to new data handling processes, this checklist is already removed as a data filtering step.)
mbbs <- mbbs %>% dplyr::filter(!primary_observer %in% "Ali Iyoob") %>%
  group_by(primary_observer) %>%
  mutate(observer_ID = cur_group_id()) %>%
  ungroup()
mbbs_nozero <- mbbs_nozero  %>% dplyr::filter(!primary_observer %in% "Ali Iyoob")

#save copies of analysis df.
write.csv(mbbs, "data/analysis.df.csv", row.names = FALSE)
write.csv(mbbs_nozero, "data/analysis.df.nozero.csv", row.names = FALSE)

#---------------------------------
#Add in landcover information
#---------------------------------

#leftjoin for landcover information
#read in nlcd data, filter to just what we're interested in. Otherwise it's a many-to-many join relationship. Right now, just the % developed land. Workflow similar to "calc_freq_remove_rows()" from the generate_percent_change_+_map.. code, but altered for this use.
nlcd <- read.csv("data/landtype_byroute.csv", header = TRUE) %>%
  #filter to nlcd class of interest, here, my classification of all 'developed'
  filter(ijbg_class == "developed") %>%
  #group_by, because calc_freq_remove_rows requested an already grouped dataset
  group_by(mbbs_county, year, route_num, ijbg_class, totpix) %>%
  transmute(frequency = sum(frequency)) %>%
  distinct() %>% #remove now extraneous rows
  mutate(percent_developed = (frequency/totpix) * 100,
         year = as.integer(year)) %>%
  ungroup() 

#read in landfire information
landfire <- read.csv("data/landfire_byroute.csv", header = TRUE) %>%
  #right now, I'm only concerned here with means. So I want to just have one bit of information for each route, we don't need to keep all the different percent landtypes. If we wanted to do that, we could pivot wider based on landtype and get the % frequency for each category.
  distinct(mbbs_county, route_num, lf_mean_route, lf_median_route, lf_year, lf_q3_route, lf_difmean_22_16) 

landfire$mbbs_county <- str_to_lower(landfire$mbbs_county)

  
  #need to now combine all the % developed land into one row for each county/route/year
##heads up, nlcd data is missing years, assigns it the last value, only change when a new yr that has new data happens. 
add_nlcd <- function(mbbs, nlcd) {
  mbbs <- mbbs %>%
    left_join(nlcd, by = c("mbbs_county", "year", "route_num")) %>%
    group_by(route_ID, common_name) %>%
    arrange(common_name, route_ID, year) %>%
    tidyr::fill(percent_developed, .direction = "downup")
  #check <- mbbs %>% filter(route_ID == 106)
  return(mbbs)
}
mbbs <- add_nlcd(mbbs, nlcd)
mbbs_nozero <- add_nlcd(mbbs_nozero, nlcd)

add_landfire <- function(mbbs, landfire) {
  mbbs <- mbbs %>% 
    left_join(landfire, by = c("mbbs_county", "route_num", "year" = "lf_year")) %>%
    group_by(route_ID, common_name) %>%
    arrange(common_name, route_ID, year) %>%
    tidyr::fill(lf_mean_route, lf_median_route, lf_q3_route, lf_difmean_22_16, .direction = "downup")
}

mbbs <- add_landfire(mbbs,landfire)
mbbs_nozero <- add_landfire(mbbs_nozero, landfire)

#------------------------------------------------------------------------------
# Set up for modeling
#------------------------------------------------------------------------------

#read in analysis df
mbbs <- read.csv("data/analysis.df.csv", header = TRUE)

#set up for modeling
species_list <- unique(mbbs$common_name)
filtered_mbbs <- mbbs %>% filter(common_name == species_list[1])

#create trend table to store results in
cols_list <- c("common_name")
trend_table <- make_trend_table(cols_list, species_list)

#make route_ID a factor
mbbs <- mbbs %>% mutate(route_ID = as.factor(route_ID))
mbbs_nozero <- mbbs_nozero %>% mutate(route_ID = as.factor(route_ID))

#add in UAI
uai <- read.csv("data/species-traits/UAI-NateCleg-etall.csv") %>%
  filter(City == "Charlotte_US")

mbbs <- mbbs %>%
  left_join(uai, by = c("common_name" = "Species"))

#formulas to plug into the models
  f_base <- count ~ year  
  f_pd <- update(f_base, ~ . + percent_developed)
  f_obs <- update(f_base, ~ . + observer_quality)
  f_pdobs <- update(f_base, ~ . + percent_developed + observer_quality)
  f_wlandfire <- update(f_pdobs, ~. + lf_mean_route)

#------------------------------------------------------------------------------
# Bayes model, based on Link and Sauer 2001
  # https://academic.oup.com/auk/article/128/1/87/5149447
# Modeling help from review of DiCecco Pop Trends code
  # https://github.com/gdicecco/poptrends_envchange/tree/master
#------------------------------------------------------------------------------
  #can use either brms' 'make_standcode' function
  #or rethinking's 'ulam' function to build the stan model
  
#Okay so, summary: yeah, I can make a poisson model run. BUt the outputs are not making sense, because it's all just giving me the intercept. And none of the other information relevant to the model. This will require another couple statistical rethinking videos I think. Also downloading Grace's poptrends git and working through script_bayes_model_trial.R
  
#set up model paramaters
  #for the moment. Let's begin with the count ~ year specification. I just want to 
  #get a bayes model running, and from there we will add in observer and the route
  #hierarchy.
#Testing purposes - just one species.
filtered_mbbs <- mbbs %>% filter(common_name == "Wood Thrush" | common_name == "Acadian Flycatcher") %>%
    group_by(common_name) %>%
    mutate(common_name_standard = cur_group_id()) %>%
    ungroup()
  
dat <- list(
  C = filtered_mbbs$count, #Count. Not sure this should be standardized
  Y = filtered_mbbs$year_standard, #first year is year 1, set as an index veriable
  S = filtered_mbbs$common_name_standard, #species, as an index variable
  R = filtered_mbbs$route_standard #route, as an index variable
  #O = filtered_mbbs$max_qual_observer #variable to account for the quality of the observer
  #R = mbbs$route_ID
)
  
#really basic model
mbasic <- ulam(
  alist(
    C ~ dpois( lambda ),
    log(lambda) <-  a[S] + b[S]*Y, #predicted by year. 
    a[S] ~ dnorm(1,.5),
    b[S] ~ dnorm(0,.2)
  ), data = dat, chains = 4, cores = 4, log_lik = TRUE
)

precis(mbasic, depth= 2)


filtered_mbbs <- mbbs %>% filter(common_name == "Wood Thrush") %>%
  group_by(common_name) %>%
  mutate(common_name_standard = cur_group_id()) %>%
  ungroup()
datWT <- list(
  C = filtered_mbbs$count, #Count. Not sure this should be standardized
  Y = filtered_mbbs$year_standard, #first year is year 1, set as an index veriable
  S = filtered_mbbs$common_name_standard, #species, as an index variable, just WT
  R = filtered_mbbs$route_standard #route, as an index variable
)

#hierachical model
mWThierarch <- ulam(
  alist(
    C ~ dpois( lambda ),
    log(lambda) <- a[R] + b*Y, #predicted by year
    b ~ dnorm(0,10), #prior for the slope year will have
    a[R] ~ dnorm( a_bar, sigma ), #prior for the intercept, going to have it vary
    a_bar ~ dnorm(1, .5),
    sigma ~ dexp(1)
  ), data = datWT, chains = 4, log_lik = TRUE
)

traceplot_ulam(mWThierarch)

#can we add in more species?
mhierarch <- ulam(
  alist(
    C ~ dpois( lambda ),
    log(lambda) <- a[R,S] + b[S]*Y, #predicted by year
    b[S] ~ dnorm(0,.2), #prior for the slope year will have
    matrix[R,S]:a ~ dnorm( a_bar, sigma ), #prior for the intercept, going to have it vary
    a_bar ~ dnorm(1, .5),
    sigma ~ dexp(1)
  ), data = dat, chains = 4, log_lik = TRUE
)

precis(mhierarch, depth = 2)

#what if :) all the species. 
#not all the species, just wt and acfl
mbbs <- mbbs %>% filter(common_name == "Wood Thrush" | common_name == "Acadian Flycatcher") %>%
  group_by(common_name) %>%
  mutate(common_name_standard = cur_group_id()) %>%
  ungroup()

datmbbs <-
  list(
  C = mbbs$count, #Count. Not sure this should be standardized
  Y = mbbs$year_standard, #first year is year 1, set as an index veriable
  S = mbbs$common_name_standard, #species, as an index variable, just WT
  R = mbbs$route_standard, #route, as an index variable
  O = mbbs$observer_quality #ought to be observer_ID, but trying it out with the quality instead to see if that improves the model fit. 
)


#if using observer_quality
mhierarchallobs <- ulam(
  alist(
    C ~ dpois( lambda ),
    log(lambda) <- a[R,S] + b[S]*Y + O, #predicted by year
    b[S] ~ dnorm(0,.2), #prior for the slope year will have
    matrix[R,S]:a ~ dnorm( a_bar, sigma ), #prior for the intercept, going to have it vary
    a_bar ~ dnorm(1, .5),
    sigma ~ dexp(1)
  ), data = datmbbs, chains = 4, cores = 4, log_lik = TRUE
)
#hey okay! The metropolis proposals were not automatically causing rejections. Adding the observer and calculating an independent value for each one is causing some problems. 





#also add in observer
#for Casey meeting, use UAI as species trait
mhierarchallobs <- ulam(
  alist(
    C ~ dpois( lambda ),
    log(lambda) <- a[R,S] + b[S]*Y + c[O], #predicted by year
    b[S] ~ dnorm(0,.2), #prior for the slope year will have
    matrix[R,S]:a ~ dnorm( a_bar, sigma ), #prior for the intercept, going to have it vary
    a_bar ~ dnorm(1, .5),
    sigma ~ dexp(1),
    c[O] ~ dnorm(0, tau_c),
    tau_c ~ gamma(0.0001,0.0001)
  ), data = datmbbs, chains = 4, cores = 4, log_lik = TRUE
)





#Warning: 500 of 2000 (25.0%) transitions hit the maximum treedepth limit of 10.
#See https://mc-stan.org/misc/warnings for details.
#If this is the only warning you are getting and your ESS and R-hat diagnostics are good, it is likely safe to ignore this warning, but finding the root cause could result in a more efficient model.

#We do not generally recommend increasing max treedepth. In practice, the max treedepth limit being reached can be a sign of model misspecification, and to increase max treedepth can then result in just taking longer to fit a model that you don’t want to be fitting.


#OKAY SO - part of the problem is that I think the priors are two wide. the traceplots are kinda buckwild bc things are starting SO far apart for the first guess, which is why those proposals are being rejected. 
check <- traceplot_ulam(mhierarchallobs)

precis(mhierarchallobs, depth = 2)

results <- precis(mhierarchallobs, depth = 2)
#write.csv(results, "data/bayes_hierarchical_year_results.csv", row.names = FALSE)

resultsdf <- data.frame(results)

resultsdf$parameter <- c(1:61, "a_bar", "sigma", 64:128)

species_to_param <- mbbs %>% 
  group_by(common_name_standard) %>%
  mutate(common_name_standard = as.character(common_name_standard)) %>%
  summarize(common_name = first(common_name))

resultsdf <- left_join(resultsdf, species_to_param, by = c("parameter" = "common_name_standard"))

write.csv(resultsdf, "data/bayes_hierarchical_year_results_61sp_obs.csv", row.names = FALSE)

#left_join GEE results
GEEresults <- read.csv("data/trend-table-GEE.csv", header = TRUE)
resultsdf <- left_join(resultsdf, GEEresults, by = c("common_name" = "species"))

#remove a_bar and sigma from resultsdf
resultsdf <- resultsdf %>%
  filter(parameter != "sigma" & parameter != "a_bar")

par(mfrow = c(1, 1))
plot(resultsdf$mean, resultsdf$trend)


mST <- ulam(
  alist(
    S ~ dbinom( D , p ),
    logit(p) <- a[T],
    a[T] ~ dnorm( a_bar, sigma ),
    a_bar ~ dnorm( 0, 1.5 ),
    sigma ~ dexp( 1 )
  ), data = datfrog, chains = 4, log_lik = TRUE
)

  
#--------------------------------------------------------------------------
# Stan
#-------------------------------------------------------------------------

#read in analysis file created up top.
mbbs <- read.csv("data/analysis.df.csv", header = TRUE)

library(rstan) #for running stan
library(loo) #leave one out, works with testing models and how good they are at replicating the datapoint that's been left out. Well fit models are better at replicating missing data.

#filtered mbbs, same as from above is just acadian flycatcher and woodthrush. for testing purposes

filtered_mbbs <- mbbs %>% filter(common_name == "Wood Thrush" | common_name == "Acadian Flycatcher") %>%
  #Recreate IDs for common name
  group_by(common_name) %>%
  mutate(common_name_standard = cur_group_id()) %>%
  ungroup() %>%
  #Recreate IDs for primary observer
  group_by(primary_observer) %>%
  mutate(observer_ID = cur_group_id()) %>%
  ungroup() %>%
  #add mock trait variables. WOTH is doing poorly and ACFL is doing well
  mutate(
    mock_diet = #we'll give WOTH a low diversity diet, ACFL high diversity, so except strong positive effect
      case_when(common_name == "Wood Thrush" ~ .1,
                common_name == "Acadian Flycatcher" ~ .8),
    mock_climate = #let's give them both a medium climate, so expect no effect
      case_when(common_name == "Wood Thrush" ~ .5,
                common_name == "Acadian Flycatcher" ~ .5),
    mock_ssi = #meh. okay these things are going to be multicolinear bc there's only two data points yk. Let's go a with a weak negative effect
      case_when(common_name == "Wood Thrush" ~ .7, #mock generalist
                common_name == "Acadian Flycatcher" ~ .3) #mock specialist (confirm that's how ssi works?)
    ) 

#prepare the data list for Stan
datstan <- list(
  N = nrow(filtered_mbbs), #number of observations
  S = length(unique(filtered_mbbs$common_name_standard)), #n species
  R = length(unique(filtered_mbbs$route_standard)), #n routes
  Y = length(unique(filtered_mbbs$year_standard)), #n years
  species = filtered_mbbs$common_name_standard, #species indices
  route = filtered_mbbs$route_standard, #route indicies
  year = filtered_mbbs$year_standard, #year indices
  observer_quality = filtered_mbbs$observer_quality, #measure of observer quality, NOT CENTERED and maybe should be? Right now there are still negative and positive observer qualities, but these are ''centered'' within routes. Actually I think this is fine non-centered, because the interpretation is that the observer observes 'quality' species of birds more or less than any other observer who's run the route. Only way it could not be fine is bc it's based on each individual route, but the observer is actually judged cross-routes.
  #observer_ID = filtered_mbbs$observer_ID, #observer index
  #O = length(unique(filtered_mbbs$observer_ID)), #n observers
  trait_diet = filtered_mbbs$mock_diet, #! NOT CENTERED YET
  trait_climate = filtered_mbbs$mock_climate, #! NOT CENTERED YET
  trait_habitat = filtered_mbbs$mock_ssi, #! NOT CENTERED YET
  C = filtered_mbbs$count #count data
)

#specify the stan model code
stan_model_code <- "
data {
  int<lower=0> N; // number of rows
  int<lower=1> S; // number of species
  int<lower=1> R; // number of routes
  int<lower=1> Y; // number of years
//  int<lower=1> O; // number of observers
  array[N] int<lower=1, upper=S> species; // there is a species for every row and it's an integer between 1 and S
  array[N] int<lower=1, upper=R> route;  // there is a route for every row and it's an integer between 1 and R
  array[N] int<lower=1, upper=Y> year;  // there is an integer for every year and it's an integer between 1 and Y
//  array[N] int<lower=1, upper=O> observer_ID;  // there is an observer_ID for every row and it is in integer between 1 and 'O'(not a zero)
 vector[N] observer_quality;  // there is an observer quality for every row and it is a real number, because it is continuous it can be a vector instead of an array
  array[N] int<lower=0> C;  // there is a count (my y variable!) for every row and it is an unbounded integer that is at least 0.
  vector[N] trait_diet; //there is a trait_diet for every row and it is a continuous real number
  vector[N] trait_climate; //trait_climate for every row and it's a continuous real number
  vector[N] trait_habitat; //trait_habitat for every row and it's a continuous real number
}

parameters {
  vector[S] b;
  matrix[R, S] a;
//  real a_bar;
  vector[S] a_bar;
  real<lower=0> sigma;
  real gamma_b;
  real kappa_diet;
  real kappa_climate;
  real kappa_habitat;
  real<lower=0> sig_b;
  
//  vector[O] c; //for obs_ID
//  real<lower=0> tau_c; //for obs_ID
//  real<lower = 0> gamma_obs; //for adding obs quality
//  real<lower = 0> kappa_obs; //for adding obs quality
//  real<lower = 0> sig_obs; //for adding obs quality
}

model {
  a_bar ~ normal(1, 0.5);
  sigma ~ exponential(1);
//  b ~ normal(0, 0.2); //without traits
  b ~ normal(gamma_b + kappa_diet*trait_diet[S] + kappa_climate*trait_climate[S] + kappa_habitat*trait_habitat[S], sig_b);
  gamma_b ~ normal(0, 0.2);
  kappa_diet ~ normal(0, 0.02); 
  kappa_climate ~ normal(0, 0.02); //have also set to .05, doesn't seem to make a difference.
  kappa_habitat ~ normal(0, 0.02);
  sig_b ~ exponential(1);
  to_vector(a) ~ normal(a_bar[S], sigma); //uses a species specific a_bar
  
//  c ~ normal(0, tau_c); //observer_ID ONLY
//  tau_c ~ gamma(.001, .001); //observer_ID ONLY

// NEW OBSERVER BLOCK, replaces c and tau_c above  
//  gamma_obs ~ exponential(1); //not sure where to set this prior
//  kappa_obs ~ exponential(1); //not sure where to set this prior
//  sig_obs ~ gamma(.001,.001); //not sure where to set this prior
//  c ~ normal(gamma_obs + kappa_obs * observer_quality, sig_obs);

// Non-vectorized, so slower than it could be. Let's ignore speed and work on content.
   for (n in 1:N) {
     C[n] ~ poisson_log(a[route[n], species[n]] + b[species[n]] * year[n] + observer_quality[n]);
   }
}

// um, for right now, let's leave the generated quantities alone.
generated quantities {
  real log_lik[N];
  for (n in 1:N) {
    log_lik[n] = poisson_log_lpmf(C[n] | a[route[n], species[n]] + b[species[n]] * year[n] + observer_quality[n]);
  }
}
"

#compile the stan model
stan_model <- stan_model(model_code = stan_model_code)
beepr::beep()

#fit the model to the data
fit <- sampling(stan_model, data = datstan, chains = 4, cores = 4, iter = 2000, warmup = 1000)
beepr::beep()

#view results
#Accessing the contents of a stanfit object:
#https://cran.r-project.org/web/packages/rstan/vignettes/stanfit-objects.html
print(fit)
#perfect, that looks great! b[1] and b[2] are as expected, at 0.05 and -0.06
#View(fit)
fit_summary <- summary(fit)
View(fit_summary$summary) #R hats and neff look good

#extract posterior samples
post <- extract(fit)

#------------------------------------------------------------------------------
#GEE model  
#------------------------------------------------------------------------------
  #GEE models from GEEpack assume that things are listed in order of cluster
  #cluster is the route. that's fine, we can sort in order
  mbbs <- mbbs %>% arrange(route_ID, year, common_name)  #also by year and common_name just to improve readability of the datatable

    gee_table <- make_trend_table(cols_list = c("common_name", "gee_estimate", "gee_trend","gee_error", "gee_significant", "gee_percdev_estimate", "gee_percdev_significant", "gee_observer_estimate", "gee_Waldtest"),
                                  rows_list = species_list)
  
  #let's try another way of having the trend table. 
  pivot_tidied <- function(model, current_species) {
    
    #tidy up the model
    tidied <- tidy(model) %>%
      pivot_longer(cols = term) %>% 
      mutate(common_name = current_species) %>% #add species column
      relocate(c(common_name, value), .before = estimate)
    
    return(tidied)
    
  }
  
  
  run_gee <- function(formula, mbbs, species_list) {
    
    #make first rows of trend table based off the first species
    current_species <- species_list[1]
    filtered_mbbs <- mbbs %>% filter(common_name == current_species)
    model <- geeglm(formula,
                    family = poisson,
                    id = route_ID,
                    data = filtered_mbbs,
                    corstr = "ar1")
    gee_table <- pivot_tidied(model, current_species)
    
    #do the same thing to the rest of the species list, and rbind those rows together
    for(s in 2:length(species_list)) {
      
      current_species <- species_list[s]
      
      filtered_mbbs <- mbbs %>% filter(common_name == current_species)
      
      model <- geeglm(formula,
                      family = poisson,
                      id = route_ID,
                      data = filtered_mbbs,
                      corstr = "ar1")
      #rbind to gee_table
      gee_table <- rbind(gee_table, pivot_tidied(model, current_species))
      
    }
    
    return(gee_table)
  }
  
  output <- run_gee(formula = 
                     f_base,
                   mbbs, species_list) %>%
    #remove intercept information
    filter(!value %in% "(Intercept)") %>%
    #add trend into (exponentiate the esimate)
    mutate(trend = exp(estimate)-1) %>%
    #pivot_wider
    pivot_wider(names_from = value, values_from = c(estimate, std.error, statistic, p.value, trend)) %>%
    dplyr::select(-name)%>%
    left_join(uai, by = c("common_name" = "Species"))
  
  
  #urbanization
  output_urb <- run_gee(formula = 
                      update(f_pd, ~ .+ observer_quality),
                    mbbs, species_list) %>%
    #remove intercept information
    filter(!value %in% "(Intercept)") %>%
    #add trend into (exponentiate the esimate)
    mutate(trend = exp(estimate)-1) %>%
    #pivot_wider
    pivot_wider(names_from = value, values_from = c(estimate, std.error, statistic, p.value, trend)) %>%
    dplyr::select(-name)%>%
    left_join(uai, by = c("common_name" = "Species"))
  
  fit <- lm(trend_year ~ UAI, data = output_urb)
  
  
  #---------------------------------bsft---------------------------------------
  output$trend_year <- output$trend_year*100
  output$std.error_year <- output$std.error_year*100
  
  minout <- min(output$trend_year)
  maxout <- max(output$trend_year)
  output$trend_std <-  round(50*((output$trend_year)-minout)/(maxout - minout),0) +1
  colorramp = colorRampPalette(c("red", "white", "blue"))
  output <- output %>% arrange(trend_year)
  
  hist(output$trend_year, breaks = 15)
  
  
  mid <- barplot(output$trend_year)
  barplot(height = output$trend_year,
         # names.arg = output$common_name,
         #percent_change - 2 * standard_deviation, percent_change + 2 * standard_deviation),  # Adjust ylim to include error bars,
          #main = "Species Yearly Trends",
         xlab = "Bird Species",
         ylab = "% Yearly Change",
         ylim = c(-7,7),
         col = colorramp(51)[output$trend_std],
         border = "black",
         cex.lab=1.5,
         cex.axis=1.5,
         yaxs = "i")
  #arrows(x0 = mid, y0 = output$trend_year + output$std.error_year,
  #       x1 = mid, y1 = output$trend_year - output$std.error_year,
  #       angle = 90, code =3, length = 0.01)
  
  #read in Grace's diet and migration metrics
  diet <- read.csv("data/gdicecco-avian-range-shifts/diet_niche_breadth.csv", header = TRUE)
  migdist <- read.csv("data/gdicecco-avian-range-shifts/migratory_distance.csv", header = TRUE)
  
  output <- output %>% 
    left_join(diet, by = c("common_name" = "english_common_name")) %>%
    left_join(migdist, by = c("common_name" = "english_common_name"))
  
  fit <- lm(trend_year ~ shannonE_diet, data = output)
  summary(fit)
  fit <- lm(trend_year ~ mig_dist_m, data = output)
  summary(fit)
  #nope, either are significant.
  
  #okay. um. some start to visuals for this presentation..
  average.count <- 
    mbbs_nozero %>%
    filter(year < 2003) %>%
    group_by(common_name) %>%
    summarize(average.count = mean(count))
  
  output <- left_join(output, average.count, by = "common_name")
  
  fit <- lm(trend_year ~ average.count, data = output, family = gaussian)
  summary(fit)
  plot(output$average.count, output$trend_year,
       xlab = "Mean Count 1999-2002",
       ylab = "% Change/Year", 
       pch = 16,
       cex = 1.4,
       cex.axis = 1.5,
       cex.lab = 1.5
  )
  abline(fit, lwd = 2)
  abline(h = 0, lty = 2, col = "red")
  text(20, 7, "P = 0.046, R2 = 0.069", cex = 1.5)
  
  #meantime. let's do migratory distance and diet etc. predictions. left_join traits
  traits <- read.csv("data/NC_species_traits.csv", header = TRUE)
  output <- output %>%
    left_join(traits, by = c("common_name" = "english_common_name"))  
  
  output$Winter_Biome <- as.factor(output$Winter_Biome)
  output$Diet_5Cat <- as.factor(output$Diet_5Cat)
  output$Migrate <- as.factor(output$Migrate)
  output 
  levels(output$Winter_Biome) 
  library(ggpubr)
  library(rstatix)
  ggboxplot(output, x = "Winter_Biome", y = "trend_year")
  ggboxplot(output, x = "Diet_5Cat", y = "trend_year",
            font.label = list(size = 25, color = "black"))
  ggboxplot(output, x = "Migrate", y = "trend_year")
  
  output %>% levene_test(trend_year ~ Winter_Biome) #passes with p > 0.05  #fit an ANOVA
  winter.aov <- output %>% anova_test(trend_year ~ Winter_Biome, detailed = T)
  winter.aov
  #no significance  
  
  library(rstatix)
  
  #fit an ANOVA for diet
  diet.aov <- output %>% anova_test(trend_year ~ Diet_5Cat, detailed = T)
  diet.aov
  pwc <- output %>% tukey_hsd(trend_year ~ Diet_5Cat)
  pwc <- pwc %>% add_xy_position(x = "Diet_5Cat")
  ggboxplot(output, x = "Diet_5Cat", y = "trend_year", add = "jitter") +
    stat_pvalue_manual(pwc, hide.ns = TRUE) +
    labs(
      subtitle = get_test_label(diet.aov, detailed = TRUE),
      caption = get_pwc_label(pwc)
    )
  #no significance  
  
  #git an ANOVA for migration
  migration.aov <- output %>% anova_test(trend_year ~ Migrate, detailed = T)
  migration.aov  #this is how you do it in base R
  summary(aov(trend_year ~ Winter_Biome, data = output))
  pwc <- output %>% tukey_hsd(trend_year ~ Migrate)
  pwc <- pwc %>% add_xy_position(x = "Migrate")
  ggboxplot(output, x = "Migrate", y = "trend_year") +
    stat_pvalue_manual(pwc, hide.ns = TRUE) +
    labs(
      subtitle = get_test_label(migration.aov, detailed = TRUE),
      caption = get_pwc_label(pwc)
    )
  
  #hey, Ivara, this is the wrong way to analyze this data. Rather than fitting a linear line to a category vs trend, this is a t-test style analysis that requires box plots of differences. let's do that and THEN make calls, ok? uai is a continous variable and can be fit with a line, these categorical variables are not and you should treat them like the cat data.
  fit <- lm(trend_year ~ Breeding_Biome, data = output)
  summary(fit)
  #____________________________________________________________________________
  
  
  output_base <- run_gee(formula = f_base,
                    mbbs, species_list) %>%
    #remove intercept information
    filter(!value %in% "(Intercept)") %>%
    #add trend into (exponentiate the esimate)
    mutate(trend = exp(estimate)-1) %>%
    #pivot_wider
    pivot_wider(names_from = value, values_from = c(estimate, std.error, statistic, p.value, trend)) %>%
    dplyr::select(-name)%>%
    left_join(uai, by = c("common_name" = "Species"))
  
  #estimate and trend are 1:1 matched. !!! Something is probably wrong with trend, I don't think it should all be in the .30s when the estimates are -06:0.08
  plot(output$trend_year, output$estimate_year)
  
  fit <- lm(trend_year ~ UAI, data = output)
  plot(y=output$trend_year, x=output$UAI)
  plot(trend_year ~ UAI, data = output, 
       pch = 16,
       col = colorramp(51)[output$trend_std])
  abline(fit)
  
  plot(count ~ year, data = pw)
  fit <- lm(count ~ year, data = pw)
  abline(fit)
  
  plot(x=output_cor$trend_year,y= output_cor$estimate_percent_developed)
    
#------------------------------------------  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
#------------------------------------------------------------------------------
#poisson model
#------------------------------------------------------------------------------

#issues with glmer poisson - I'm getting a "model failed to converge" when I include the random effect of observers, "Model is nearly unidentifiable: very large eigenvalue" - this is using primary_observer (58 possible random variations)
  #sample_model <- glmer(count ~ year + percent_developed + (1 | observer_ID), data = filtered_mbbs, family = "poisson")
  
#a poisson model is not meant for a version that has the added in zeros. so we will use the mbbs without added zeros
  
  pois_table <- make_trend_table(c("common_name","pois_estimate", "pois_trend", "pois_error", "pois_significant", "pois_percdev_estimate", "pois_percdev_trend", "pois_percdev_significant"))
  
  for(s in 1:length(species_list)) {
    
    current_species <- species_list[s]
    
    filtered_mbbs <- mbbs_nozero %>% filter(common_name == current_species)
    
    model <- glmer(formula_randomeffects, family = "poisson", data = filtered_mbbs)
    
    #tidied <- tidy(model)
    
    #add to trend table
    pois_table[s,1] <- current_species
    pois_table$pois_estimate[s] <- summary(model)$coefficients[2,1]
    #pois_table$pois_estimate[s] <- tidied$estimate[2]
    #pois_table$pois_trend[s] <- exp(pois_table$pois_estimate[s]) -1
    #pois_table$pois_error[s] <- tidied$std.error[2]
    #pois_table$pois_significant[s] <- case_when(tidied$p.value[2] <= 0.05 ~ 1,
#                                                tidied$p.value[2] > 0.05 ~ 0)
    #pois_table$pois_percdev_estimate[s] <- tidied$estimate[3]
    #pois_table$pois_percdev_trend[s] <- exp(pois_table$pois_percdev_estimate[s]) -1
    # pois_table$pois_percdev_significant[s] <- case_when(tidied$p.value[3] <= 0.05 ~ 1,
    #                                                     tidied$p.value[3] > 0.05 ~ 0)
  }

  trend_table <- left_join(trend_table, pois_table, by = "common_name")

#------------------------------------------------------------------------------
#negative binomial model
#------------------------------------------------------------------------------
  
  #I think not necessary, but reserve the space.
  negb_table <- make_trend_table(c("common_name","negb_estimate", "negb_trend", "negb_error", "negb_significant", "negb_percdev_estimate", "negb_percdev_trend", "negb_percdev_significant"))
  
#------------------------------------------------------------------------------
#GEE model  
#------------------------------------------------------------------------------
  #GEE models from GEEpack assume that things are listed in order of cluster
  #cluster is the route. that's fine, we can sort in order
  mbbs <- mbbs %>% arrange(route_ID, year, common_name)  #also by year and common_name just to improve readability of the datatable

    gee_table <- make_trend_table(c("common_name", "gee_estimate", "gee_trend","gee_error", "gee_significant", "gee_percdev_estimate", "gee_percdev_significant", "gee_observer_estimate", "gee_Waldtest"))
    
    #add in the species
    for(s in 1:length(species_list)) {
      
      gee_table[s,1] <- species_list[s]
      
    }
    
    for(s in 1:length(species_list)) {
      
      current_species <- species_list[s]
      
      filtered_mbbs <- mbbs %>% filter(common_name == current_species)
      
      model <- geeglm(formula_simple,
                      family = poisson,
                      id = route_ID,
                      data = filtered_mbbs)
      
      tidied <- tidy(model)
      
      gee_table$common_name[s] <- current_species
      gee_table$gee_estimate[s] <- tidied$estimate[2]
      gee_table$gee_trend[s] <- exp(gee_table$gee_estimate[s]) -1
      gee_table$gee_error[s] <- tidied$std.error[2]
      gee_table$gee_significant[s] <- case_when(tidied$p.value[2] <= 0.05 ~ 1,
                                             tidied$p.value[2] > 0.05 ~ 0)
      gee_table$gee_percdev_estimate[s] <- tidied$estimate[3]
      gee_table$gee_percdev_significant[s] <- case_when(tidied$p.value[3] <= 0.05 ~ 1,
                                                        tidied$p.value[3] > 0.05 ~ 0)
      gee_table$gee_observer_estimate[s] <- NA
      gee_table$gee_Waldtest[s] <- summary(model)$coefficients[2,3]
    }

    trend_table <- left_join(trend_table, gee_table, by = "common_name")
    
    
#------------------------------------------
    plot(trend_table$pois_estimate, trend_table$gee_estimate)
    abline(a=0,b=1)
    text(trend_table$pois_estimate, trend_table$gee_estimate, labels = trend_table$common_name, pos = 4, cex = 0.8, col = "darkblue")    
    
    plot(trend_table$pois_percdev_estimate, trend_table$gee_percdev_estimate)
    abline(a=0,b=1)
    
    plot(trend_table$pois_error.x, trend_table$pois_error.y)
    abline(a=0,b=1)

  
  
  
    