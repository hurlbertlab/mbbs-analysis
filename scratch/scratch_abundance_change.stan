//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=1> N; //number of surveys
  int<lower=1> Nrt; //number of routes
  
  array[N] int<lower=0> abundance; //abundance for each survey
  vector[N] julian_day; //julian day for each survey
  vector[N] observer_quality; //observer quality for each survey
  vector[N] year; //year for each survey
  array[N] int<lower=1, upper=Nrt> rt; //route for every survey
  
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[Nrt] a_raw; //fit a different intercept for each route
  real a_bar; //average route
  real<lower = 0> sigma_a; //variance in routes
  
  vector[Nrt] beta_year_raw; //fit a different effect of year for each route
  real b_year_bar; //average year
  real<lower = 0> sigma_b_year; //variance in routes
  real beta_obs; //fit one effect of observer quality
  real beta_jday; //fit one effect of julian day
}

transformed parameters {
  
  vector[Nrt] a = a_bar + sigma_a * a_raw;
  vector[Nrt] beta_year = b_year_bar + sigma_b_year * beta_year_raw;
  
  
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  
  a_raw ~ normal(0,1); //Hierarchical prior for each route's intercept
  a_bar ~ normal(5, 2); //model intercepts are between 200 and 300
  sigma_a ~ exponential(1);
  
  beta_year_raw ~ normal(0,1); //prior for the effect of year
  b_year_bar ~ normal(0,1); //prior for the average effect of year across routes
  sigma_b_year ~ exponential(1);
  
  beta_obs ~ normal(0,1); //prior for the effect of observers
  beta_jday ~ normal(0,1); //prior for the effect of julian day
  
  abundance ~ poisson_log(
    a[rt] +
    beta_year[rt] .* year +
    beta_obs * observer_quality +
    beta_jday * julian_day
  );
}
