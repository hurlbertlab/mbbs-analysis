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
  vector[N] regional_trend_mean;
  vector[N] regional_trend_sd;
}

parameters {
  real a; //universal intercept, taking the mean out of the intercept distribution and treating it as a constrant plus a gaussian distribution centered on zero
  vector[Nsp] a_sp_raw; //intercept for each unique sp
  real<lower = 0> sig_sp; //variance in a_sp
  vector[Nrt] a_rt_raw; //intercept for each unique route
  real<lower = 0> sig_rt; //variance in a_rt
  
  vector[Nsp] b_year; //species trend, fit one for each species
  real gamma_b; //mean for species trends. calculated across species, and we only want one value, so this is not a vector.
  real<lower = 0> sig_year; //variance in year betas across species
//    real gamma_b; //intercept for species trends. calculated across species, and we only want one value, so this is not a vector.

  vector[Nsp] regional_trend; //one regional trend for every species.
  
  real c_obs; //effect of observer
  
}

transformed parameters {

//transform z-score easy-to-fit alphas 
  vector[Nsp] a_sp = a_sp_raw * sig_sp;
  vector[Nrt] a_rt = a_rt_raw * sig_rt;
  
  //pretty sure this method is making the model take..longer to fit than before? Since the testing model with just a couple species is still taking a good while. But, I like that this model makes explit that we should be fitting a different intercept for each species and each route, and doesn't fit .... a combo ... for each species-route combo.... hm. Maybe it should be fitting an intercept for each species and then an additional intercept for each species-route? m.
}

model {

// Non-vectorized, so slower than it could be. Let's ignore speed and work on content.
   for (n in 1:N) {
     C[n] ~ poisson_log(
       a + 
       a_rt[rt[n]] +
       a_sp[sp[n]] +
       b_year[sp[n]] * year[n] + 
       c_obs * observer_quality[n]);
   }

  a ~ normal(0, 2); //universal intercept, trying not to constrain the prior too lightly so using 2 instead of 1
  a_sp_raw ~ std_normal(); //centered on zero, use a hyperparam to set the distribution param.
  a_rt_raw ~ std_normal(); //^^ same as above
  sig_rt ~ normal(0, .5); //half normal
  sig_sp ~ normal(0, .5); //half normal
  
  b_year ~ normal(gamma_b, sig_year); //without traits
  gamma_b ~ normal(0, 0.2);
  sig_year ~ exponential(1);
  
  c_obs ~ normal(0, 0.5); //half normal, there's one effect of changing observers across routes and species and I don't expect it to be a large effect so I constrain it to half normal
  
//PREV VERSION OF B:
  //b ~ normal(gamma_b, sig_b); //without traits
  //gamma_b ~ normal(0, 0.2);
  //sig_b ~ exponential(1);
//Which......maybe I ought to keep? I never had problem fitting betas with the previous method, and I don't want to make things more complicated than needed for myself....
//Let's try this new version and go from there! ok!

}
