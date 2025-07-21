//
// This stan program takes calulates how the change in count on a quarter-route changes in response to change in a landcover. 
// Used with two models, calculating change_dev and change_forest.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//
//Stan model for a change per change analysis

  data {
  int<lower=0> N; //number of observations
  int<lower=1> Nqrt; //number of quarter routes
  array[N] int<lower=1, upper=Nqrt> qrt; //quarter route for each observation
  int<lower=1> Nsp; //number of species
  array[N] int<lower=1, upper=Nsp> sp; //species for each observation
  vector[N] change_landcover; //change in development or forest since the last year
//  vector[N] base_landcover; //percent developed or forested for each observation, this is a standardized z-score
//  vector[N] year; //year for each observation
// ^ year is pulled out because we're working with dif and the magnitude of changes is not so huge that I expect exponential change to really be having an effect over time. There's a check for species having issues with this assumption, and as of 2025.06.06 all the species pass
  vector[N] change_obs; //0 or 1 for if the observer changed between the two surveys
  vector[N] change_C; //change in count since the last survey for each row, vector bc it doesn't have bounds like an array does
  }
  
  parameters {
    real a; //universal intercept, taking the mean out of the intercept distribution and treating it as a constant plus a gaussian distribution centered on zero
    vector[Nqrt] a_qrt; //intercept for each unique qrt
    real<lower=0> sig_qrt; //variance in a_qrt
    vector[Nsp] a_sp; //intercept for each unique sp
    real<lower=0> sig_sp; //variange in a_sp
    vector[Nsp] b_landcover_change; //effect of change in development or forest, across routes. Fit one for each species
    //let's test this out, BUT it might make the most sense to take out the mean treat it as a distribution of values
    real<lower=0> sig_lcc; //variance in b_landcover_change
    
//   real b_year; //effect of year, across routes.
//    real b_landcover_base; //effect of development or forest, across routes
    
    real c_obs; //effect of if the observer changed
    
    real<lower=0> sigma;
  
  }
  
  model {
    // Normal distribution bc change_c can be negative and no longer represents counts
    for (n in 1:N) {
    change_C[n] ~ normal(
      a +
      a_qrt[qrt[n]] +
      a_sp[sp[n]] +
      b_landcover_change[sp[n]]*change_landcover[n] + 
//      b_year*year[n] + 
//      b_landcover_base*base_landcover[n] +
      c_obs*change_obs[n], 
      sigma);
    }
  


    a ~ normal(0,10); //universal intercept, trying not to constrain the prior too tightly so using 10 instead of 1
    a_qrt ~ normal(0, sig_qrt); //centered on zero, use a hyperparam to set the distribution param. 
    a_sp ~ normal(0, sig_sp); //centered on zero, use a hyperparam to set the distribution param.
    //use the half cauchy (lower bound = 0 set above) bc thats what stat rethinking uses, pg 371. Gives more credence to extreme tails than a normal distribution does
    sig_qrt ~ exponential(1); //ok reduce credence to extreme tails on sig_qrt, routes should b p similar to each other.
    sig_sp ~ cauchy(0,1);
    // okay so, with interpretation...
    // take a (global mean) and add a_sp*sig_sp(variance in sp)

    
    
    //there is one effect of change in urbanization across routes
    b_landcover_change ~ normal(0, sig_lcc);
    sig_lcc ~ cauchy(0,1);
    //there is one effect of year across routes
//    b_year ~ normal(0,1);
    //there is one effect of baseline urbanization across routes
//    b_landcover_base ~ normal(0,1);
    
    //there's one effect of changing observers across routes, and I don't expect it to be a large effect so I constrain it a bit more than the other variables (0,0.5)
    c_obs ~ normal(0, 0.5); 
    
    //just a normal distribution, so we'll model sigma with exponential
    sigma ~ exponential(1);
    
}
