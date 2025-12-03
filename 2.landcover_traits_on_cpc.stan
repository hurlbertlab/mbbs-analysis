//
// Predicting the effect of species traits
// temperature niche
// grassland specializatoin
// forest specialization
// urban association index
// on species degree of change in response to a change in landcover
// which, depending on the imput, could be response to change in development or change in forest

data {
  int<lower=0> N; //number of observations/species
  vector[N] z_score_tempwq; //x variable, eg. climate position
  vector[N] forest_association;
  vector[N] grassland_association;
  vector[N] uai; //urban assocation index
  vector[N] cpc_mean; //mean effect of a change in landcover (eg. development or forest) on the change in species counts at each quarter route
  vector[N] cpc_sd; //standard deviation in the slopes passed from the previous model
}


parameters {
  real a; //intercept
  real b_tempwq;
  real b_forest;
  real b_grassland;
  real b_uai;
  vector[N] cpc; //each species has one effect of change in landcover on change in count, sampled form the mean,sd distribution provided in the data block
  real<lower=0> sig_cpc; //hyperparam for our estimated cpc
  //was just vector<lower=0>[N] sig_cpc, which is how it is set up for regional trend, just...not sure it should be that way there either I suppose.
}


transformed parameters {
  
  vector[N] mu_cpc = a + 
  b_tempwq*z_score_tempwq +
  b_forest*forest_association +
  b_grassland*grassland_association +
  b_uai*uai;
  
  //will need to transform to z-score things to make them fit better I think.
  
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  
  //priors, start off simple
  a ~ normal(0,1);
  b_tempwq ~ normal(0,1); 
  b_forest ~ normal(0,1);
  b_grassland ~ normal(0,1);
  b_uai ~ normal(0,1);
  
  //calculation of cpc from the mean,sd provided in data block
  sig_cpc ~ normal(0,.5); //half-normal
  //estimate of regional trend fit with hyperparameters
  cpc ~ normal(mu_cpc, sig_cpc); //cpc is predicted by mu_cpc (which has the intercept and all the effects of traits!) and sig_cpc.
  
  //observed means come from estimated cpc
  cpc_mean ~ normal(cpc, cpc_sd); //this is how I constrain the values of cpc to be in the right places. the estimates of trait effects occur yeah, with the cpc calculation and not with the cpc mean calculation

}
