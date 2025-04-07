
data {
  int<lower=0> N; //number of observations
  vector[N] effect_of_development; //y variable, known mean for each species
  int<lower=0> Nspg; //number of species groups
  array[N] int<lower=1, upper=Nspg> species_group; //species group id for each mass observation
  vector[N] mass; //mass of species, scaled with a z-score within each group
}


parameters {
  real a;
  //vector[Nspg] b_size;
  vector[Nspg] b_size_raw; //de-centering parameters.
  real b_bar_size;
  real<lower=0> b_sig_size;
  real<lower=0> sig;
}

transformed parameters {
  vector[Nspg] b_size = b_bar_size + b_sig_size * b_size_raw;
}


model {
  effect_of_development ~ normal(a + b_size[Nspg]*mass, sig); //regression

  b_size_raw ~ normal(0,1); // Implies b_size ~ normal(b_bar_size, b_sigma_size)
  
  //b_size ~ normal(b_bar_size, b_sig_size); //likely causing divergence issues bc its centered
  //let's add priors
  b_bar_size ~ normal(0, 1); //This suggests that a 1-SD change in size is a priori expected to have an effect of 0 on Y, with a 95% probability that the true effect lies between -2 and 2 (in units of Y).
  b_sig_size ~ normal(0, 0.5);  // half-normal, expect not much variation between groups but leave it open to the possibility there is some. exp(1) would mean I really don't expect variation between groups, but if I set it at that, it's also like.. okay so why am I bothering to fit a hyperparameter?
  //exponential(1) would strongly regularize small groups
 // b_sig_size ~ exponential(1);
 sig ~ exponential(1);
}
