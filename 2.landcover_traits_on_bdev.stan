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
  array[N] int<lower=1, upper=Nspg> species_group; //species group for each mass observation
  vector[N] mass; //mass of species, scaled with a z-score within each group

  //int<lower=1> Nsp; //number of species
  //int<lower=1> Ngroup; //number of groups
  //array[N] int<lower=1, upper=Ngroup> group; //species group for each mass observation.
  //vector[N] dev_mean; //effect of development for each observation
  //vector[N] dev_sd; //effect of development for each observation
  //vector[N] climate_position;
  //vector[N] habitat;
  //vector[N] mass;
}


parameters {
  real a;
  real b_climate;
  real b_habitat;
  vector[Nspg] b_size;
  real b_bar_size;
  real b_sig_size;
  real<lower=0> sig;
}


// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  effect_of_development ~ normal(a + b_climate*climate_position + b_habitat*habitat + b_size[Nspg]*b_size, sig); #regression
  
  b_climate ~ normal(0,1); #start off simple.
  b_habitat ~ normal(0,1);
  b_size ~ normal(b_bar_size, b_sig_size);
  b_bar_size ~ normal(0,1);
  b_bar_size ~ exponential(1);
}
