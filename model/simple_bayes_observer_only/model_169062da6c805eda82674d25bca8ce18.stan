
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

