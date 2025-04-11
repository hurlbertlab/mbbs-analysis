//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; //number of observations
  vector[N] climate_position; //x variable, eg. climate position
  vector[N] habitat;
  vector[N] effect_of_development; //y variable, known mean for each species
  int<lower=0> Nspg; //number of species groups
  array[N] int<lower=1, upper=Nspg> species_group; //species group id for each mass observation
  vector[N] mass_group; //mass of species, scaled with a z-score within each group
  vector[N] log_mass; //mass of species, log-scaled and ACROSS ALL SPECIES GROUPS
}


parameters {
  real a;
  real b_climate;
  real b_habitat;
  real b_log_size;
  vector[Nspg] b_size_raw; //de-centering parameters.
  real b_bar_size;
  real<lower=0> b_sig_size;
  real<lower=0> sig;
}

transformed parameters {
  vector[Nspg] b_size_group = b_bar_size + b_sig_size * b_size_raw;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  effect_of_development ~ normal(a + b_climate*climate_position + b_habitat*habitat + b_size_group[Nspg]*mass_group + b_log_size*log_mass, sig); #regression
  
  b_climate ~ normal(0,1); #start off simple.
  b_habitat ~ normal(0,1);
  b_log_size ~ normal(0,1);
  b_size_raw ~ normal(0,1);
  b_bar_size ~ normal(0, 1);
  b_sig_size ~ normal(0, 0.5);  // half-normal, expect not much variation between groups but leave it open to the possibility there is some. exp(1) would mean I really don't expect variation between groups and want to be cautious about expecting that.
}
