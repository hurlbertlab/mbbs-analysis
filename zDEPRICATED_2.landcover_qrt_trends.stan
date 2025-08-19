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

// The input data is a vector 'y' of length 'N'.
  data {
  int<lower=0> N; //number of observations
  int<lower=1> Nqrt; //number of quarter routes
  array[N] int<lower=1, upper=Nqrt> qrt; //quarter route for each observation
  vector[N] perc_dev; //percent developed for each observation, this is a standardized z-score
  vector[N] year; //year for each observation, z-scored
  array[N] int<lower=0> C; //count for each row
  }
  
  parameters {
//We use non-centered z-score variables to make the MCMC chain dramatically easier to fit. 
    vector[Nqrt] a_z; //fit an intercept for each q_route, z-score
    real a_bar; //'average q_route'
    real<lower=0> sig_a; //variance from average q_route
    
    real b_dev; //effect of development, across routes.
    real b_year; //effect of year, across routes.
    
    real<lower=0> overdispersion_param;
    //define lambda and overdis param here?
  
  }
  transformed parameters {
    vector[Nqrt] a = a_bar + a_z*sig_a; //how to transform a
  }
  
  model {
    //Non-vectorized alas! neg binomial
    for (n in 1:N) {
    C[n] ~ neg_binomial(exp(a[qrt[n]] + b_dev*perc_dev[n] + b_year*year[n]), overdispersion_param);
    }
  
  //Vectorize attempt
  //  C ~ neg_binomial(exp(a[qrt] + b[qrt] * perc_dev), overdispersion_param);

    //priors
   // lambda = exp(a[]); //intercept only right now. in a more advanced model, you have eg. + b*year + b*landcover
   //we substitute the equasion for lambda directly into the model above. No need to mess around with where lambda gets declared - it's a placeholder value anyway.
    a_z ~ normal(0,1); //partially pool the intercepts
    a_bar ~ normal(0,2); //or some prior. this is kinda large and maybe should be narrowed but whatever. Let's go with it for now.
    sig_a ~ exponential(1);
    
    
    //there is one effect of urbanzation across routes
    b_dev ~ normal(0,1);
    //there is one effect of year across routes
    b_year ~ normal(0,1);
    
    //chatgpt says typical prior for the overdispersion param is a gamma prior. I've seen a beta used as well, some questions remain here.
    overdispersion_param ~ gamma(2,1);
    
}
