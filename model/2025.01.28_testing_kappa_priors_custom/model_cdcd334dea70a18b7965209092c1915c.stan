
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
  vector[N] trait_regional; 
  vector[N] trait_climateposition; 
  vector[N] trait_habitatselection; 
}

parameters {
  vector[S] b;
  matrix[R, S] a;
//  real a_bar;
  vector[S] a_bar;
  real<lower=0> sigma;
  real gamma_b;
  real kappa_regional;
  real kappa_climateposition;
  real kappa_habitatselection;
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
  b ~ normal(gamma_b + kappa_regional*trait_regional[S] + kappa_climateposition*trait_climateposition[S] + kappa_habitatselection*trait_habitatselection[S], sig_b);
  gamma_b ~ normal(0, 0.2);
  kappa_regional ~ normal(0, .02); 
  kappa_climateposition ~ normal(0, .2); 
  kappa_habitatselection ~ normal(0, .2);
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

