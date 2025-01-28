######################################
# Using ulam() from the package rethinking
# to calculate species-trend estimates
# moved on from this code to STAN code
# but keeping it for posterity, might need it again 
# This was part of species-trend-estimates-allmodels.R
#####################################

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

