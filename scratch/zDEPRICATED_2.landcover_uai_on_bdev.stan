//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//
data {
  int<lower=0> N; //number of observations
  vector[N] uai; //x variable, eg. climate position
  vector[N] effect_of_development; //y variable, known mean for each species

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
  real b_uai;
  real<lower=0> sig;
}


model {
  effect_of_development ~ normal(a + b_uai*uai, sig); #regression
  
  b_uai ~ normal(0,1); #start off simple.
}
