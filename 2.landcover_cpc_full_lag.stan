//
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//
// Stan model for a change per change analysis with a long lag. Includes analysis of the effects of species traits

  data {
  int<lower=0> N; //number of observations
  int<lower=1> Nsp; //number of species
  array[N] int<lower=1, upper=Nsp> sp; //species for each observation
  vector[N] change_landcover; //change in development or forest since the last year
  vector[N] change_obs; //0 or 1 for if the observer changed between the two surveys. 
  vector[N] change_C; //change in count since the last survey for each row, vector bc it doesn't have bounds like an array does
  
  //species-traits
  vector[Nsp] forest_association; 
  vector[Nsp] uai; //urban association index
  }
  
  parameters {
    real a; //universal intercept, taking the mean out of the intercept distribution and treating it as a constant plus a gaussian distribution centered on zero
    vector[Nsp] a_sp_raw; //intercept for each species. Each quarter route-species combo is represented only once so we don't partially pool across quarter-routes, but species are represented multiple times. 
    real<lower = 0> sig_sp; //variance in intercepts across species
  
    vector[Nsp] b_landcover_change; //effect of landcover for each species
    real gamma_b; //mean effect of landcover across species
    real<lower = 0> sig_b; //variance in b_landcover_change
    
    real kappa_forest; //species-trait effect of forest
    real kappa_uai; //species-trait effect of uai

    real c_obs; //effect of if the observer changed
    
    real<lower=0> sigma;
  
  }
  
  transformed parameters {
    
  //transform z-score easy-to-fit alphas
  vector[Nsp] a_sp = a_sp_raw * sig_sp; 
  
}
  
  
  model {
    // Normal distribution bc change_c can be negative and no longer represents counts
    for (n in 1:N) {
    change_C[n] ~ normal(
      a + //universal intercerpt
      a_sp[sp[n]] + //species-specific intercept
      b_landcover_change[sp[n]]*change_landcover[n] + //landcover slope
      c_obs*change_obs[n], //observer slope
      sigma); //residual unexplained variation
    }
  
    b_landcover_change ~ normal(gamma_b +
      kappa_forest * forest_association +
      kappa_uai * uai, 
      sig_b);

    a ~ normal(0,2); //universal intercept, trying not to constrain the prior too tightly so using 10 instead of 1
    a_sp_raw ~ std_normal();
    sig_sp ~ normal(0, 0.5); //half normal, species can be more variable from one another than exp(1) suggests
    
    //there is one effect of change in urbanization across routes
    gamma_b ~ normal(0, 1);  // NEW: prior on the group mean
    kappa_forest ~ normal(0, 1);
    kappa_uai ~ normal(0, 1);
    sig_b ~ normal(0, 0.5); //easier to fit than exponential
    
    //there's one effect of changing observers across routes, and I don't expect it to be a large effect so I constrain it a bit more than the other variables (0,0.5)
    c_obs ~ normal(0, 0.5); 
    
    //just a normal distribution, so we'll model sigma with exponential
    sigma ~ exponential(1);
    
}
