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
  vector[N] cpc_slope; //y variable, mean effect of a change in landcover (eg. development or forest) on the change in species counts at each quarter route
}


parameters {
  real a; //intercept
  real b_tempwq;
  real b_forest;
  real b_grassland;
  real b_uai;
  real<lower=0> sigma; //standard deivation sigma
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  cpc_slope ~ normal(a + 
  b_tempwq*z_score_tempwq +
  b_forest*forest_association +
  b_grassland*grassland_association +
  b_uai*uai,
  sigma);
  
  //priors, start off simple
  b_tempwq ~ normal(0,1); 
  b_forest ~ normal(0,1);
  b_grassland ~ normal(0,1);
  b_uai ~ normal(0,1);
}

