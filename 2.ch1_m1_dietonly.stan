//
// This Stan program defines a hierarchical model
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//
// Looking for a specific model run previously? check out Z:/Hurlbertlab/Goulden/
// mbbs-analysis and the models that are stored there. They include a txt file 
// with the stan code.
//
data {
  int<lower=0> N; // number of observations or rows
  int<lower=1> Nsp; // number of species
  int<lower=1> Nrt; //number of routes
  int<lower=1> Nyr; //number of years
  int<lower=1> Ndc; //number of diet categories
  array[N] int<lower=1, upper=Nsp> sp; //species id for each observation
  array[N] int<lower=1, upper=Nrt> rt; //route id for each observation
  array[N] int<lower=1, upper=Nyr> year; //year for each observation 
//.............observer section....................................
  vector[N] observer_quality; //there is an observer_quality for every observation [vector of Nobs otherwise]
//..............count................................................
  array[N] int<lower=0> C; // there is a count (my y variable!) for every row, and it is a bounded integer that is at least 0.
//...............predictor variables.....................
  array[Nsp] int<lower=1, upper=Ndc> t_diet_cat; //diet category for every species
  
}

parameters {
  
  //intercept
  real a; //universal base intercept, taking the mean out of the intercept distribution and treating it as a constant plus a species and route gaussian distribution centered on zero
  vector[Nsp] a_sp_raw; //intercept for each unique sp
  real<lower = 0> sig_sp; //variance in species-specific intercepts
  vector[Nrt] a_rt_raw; //intercept for each unique route
  real<lower = 0> sig_rt; //variance in route-specific intercepts
  
  
  //calculating species trend year effect slope
  vector[Nsp] b; //species trend, fit one for each species
  real gamma_b; //intercept for the distribution of species trends. calculated across species, and we only want one value, so this is not a vector.
  real<lower = 0> sig_b; //variance in the slope across species, also in some ways deviation from the explanatory power of the traits on predicting trends. Represents residual variance / measure of scatter. In some ways like R2 a bit?
  
  //traits variables predicting betas
  vector[Ndc] kappa_diet_cat; //each diet category may have a different effect on slopes across species.
  
  real c_obs; //effect of observer, calculated across all observers since we use a quantitative measure of observer quality rather than a vector of observer identifiers
  
  real<lower=0> overdispersion_param; //negative binomial overdispersion parameter
  
}

transformed parameters {
  
  //transform z-score easy-to-fit alphas 
  vector[Nsp] a_sp = a_sp_raw * sig_sp;
  vector[Nrt] a_rt = a_rt_raw * sig_rt;
  
  //vectorize eta
  vector[N] eta = a + a_sp[sp] + a_rt[rt] + b[sp] .* to_vector(year) + c_obs * observer_quality;
  
}

model {

//priors first
//intercepts
  a ~ normal(0, 2); //universal intercept, trying not to constrain the prior too lightly so using 2 instead of 1. 
  a_sp_raw ~ std_normal(); //centered on zero, use a hyperparam to set the distribution param.
  a_rt_raw ~ std_normal(); //^^ same as above
  sig_rt ~ normal(0, .5); //half normal
  sig_sp ~ normal(0, .5); //half normal

//observer prior
  c_obs ~ normal(0, 0.5); //half normal, there's one effect of changing observers across routes and species and I don't expect it to be a large effect so I constrain it.

 
//kappa priors
    kappa_diet_cat ~ normal(0, 1);

//overdispersion param prior
//chatgpt says typical prior for the overdispersion param is a gamma prior. I've seen a beta used as well, some questions remain here.
    overdispersion_param ~ gamma(2,1);
    
//modeling beta
  gamma_b ~ normal(0, 0.2);
  sig_b ~ normal(0, .5); //half normal
  b ~ normal(gamma_b + 
             kappa_diet_cat[t_diet_cat], //categorical so no *
             sig_b);

// Vectorized
C ~ neg_binomial_2_log(eta, overdispersion_param);

}
