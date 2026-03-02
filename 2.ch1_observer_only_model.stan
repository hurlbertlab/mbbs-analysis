//
// Observer only model for chapter 1 predicting species trends at the route level
// This model is mostly used for testing to ensure everything is running correctly
// Before I add in complexity with modeling the effect of species traits.
//
//

data {
  int<lower=0> N; // number of observations or rows
  int<lower=1> Nsp; // number of species
  int<lower=1> Nrt; // number of routes
  int<lower=1> Nyr; //number of years
  array[N] int<lower=1, upper=Nsp> sp; //species id for each observation
  array[N] int<lower=1, upper = Nrt> rt; //route for each observation
  array[N] int<lower=1, upper=Nyr> year; //year for each observation
  vector[N] observer_quality; //there is an observer_quality for each observation
  array[N] int<lower=0> C; // there is a count (my y variable!) for every row, and it is a bounded integer that is at least 0.
  //vector[Nsp] regional_trend_mean;
  //vector[Nsp] regional_trend_sd;
}

parameters {
  real a; //universal intercept, taking the mean out of the intercept distribution and treating it as a constrant plus a gaussian distribution centered on zero
  matrix[Nsp, Nrt] a_sprt_raw; //intercept for each unique sp
  real<lower = 0> sig_sprt; //variance in a_sprt
  //vector[Nsp] a_sp_raw; //intercept for each unique sp
  //real<lower = 0> sig_sp; //variance in a_sp
  //vector[Nrt] a_rt_raw; //intercept for each unique route
  //real<lower = 0> sig_rt; //variance in a_rt
  
  vector[Nsp] b_year; //species trend, fit one for each species
  real gamma_b; //mean for species trends. calculated across species, and we only want one value, so this is not a vector.
  real<lower = 0> sig_year; //variance in year betas across species
//    real gamma_b; //intercept for species trends. calculated across species, and we only want one value, so this is not a vector.

  //vector[Nsp] regional_trend; //one regional trend for every species.
  //real kappa_regional; //one effect of regional trend on slopes across species
  
  real c_obs; //effect of observer
  real<lower = 0> overdispersion_param; //phi
}

transformed parameters {

//transform z-score easy-to-fit alphas 
  matrix[Nsp, Nrt] a_sprt = a_sprt_raw * sig_sprt;
  //vector[Nsp] a_sp = a_sp_raw * sig_sp;
  //vector[Nrt] a_rt = a_rt_raw * sig_rt;
  
  
  vector[N] eta;
  for (n in 1:N) {
    eta[n] = a +
             a_sprt[sp[n], rt[n]] +
             b_year[sp[n]] * year[n] +
             c_obs * observer_quality[n];
  }
  
// vectorize eta
//  vector[N] eta = a + 
//                  a_sprt[sp,rt] + 
//                  b_year[sp] .* to_vector(year) + 
//                  c_obs * observer_quality;
                  
}

model {

// Non-vectorized, so slower than it could be. Let's ignore speed and work on content.
//   for (n in 1:N) {
//     C[n] ~ neg_binomial_2_log(
//       a + 
//       a_sprt[sp[n],rt[n]] +
      // a_rt[rt[n]] +
      // a_sp[sp[n]] +
//       b_year[sp[n]] * year[n] + 
//       c_obs * observer_quality[n],
//       overdispersion_param);
//   }

  a ~ normal(0, 2); //universal intercept, trying not to constrain the prior too lightly so using 2 instead of 1
  to_vector(a_sprt_raw) ~ std_normal(); //centered on zero, use a hyperparam to set the distrubition param
  //The to_vector() function flattens the matrix into a vector, which allows you to assign a prior distribution to all elements at once. This is equivalent to saying each element of a_sprt_raw independently follows a standard normal distribution.
  sig_sprt ~ exponential(1);
  //a_sp_raw ~ std_normal(); //centered on zero, use a hyperparam to set the distribution param.
  //a_rt_raw ~ std_normal(); //^^ same as above
  //sig_rt ~ normal(0, .5); //half normal
  //sig_sp ~ normal(0, .5); //half normal
  //sig_rt ~ exponential(1);
  //sig_sp ~ exponential(1);
  
  //b_year ~ normal(gamma_b + kappa_regional*regional_trend, sig_year); //without traits
  b_year ~ normal(gamma_b, sig_year); //without any traits or reg. trend
  gamma_b ~ normal(0, 1); //was 0, 0.2
  sig_year ~ exponential(1);
  
  c_obs ~ normal(0, 0.5); //half normal, there's one effect of changing observers across routes and species and I don't expect it to be a large effect so I constrain it to half normal
  
  //regional trend we already know the mean and standard deviation
  //headsup, this is not modified to casey's version p sure.
  //regional_trend ~ normal(regional_trend_mean, regional_trend_sd);
  
  overdispersion_param ~ gamma(2,1);
  
  //Vectorized
  C ~ neg_binomial_2_log(eta, overdispersion_param);

}
