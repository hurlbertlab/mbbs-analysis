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
//Stan model for a change per change analysis

// The input data is a vector 'y' of length 'N'.
  data {
  int<lower=0> N; //number of observations
  int<lower=1> Nqrt; //number of quarter routes
  array[N] int<lower=1, upper=Nqrt> qrt; //quarter route for each observation
  vector[N] change_dev; //change in development since the last year
  vector[N] dev; //percent developed for each observation, this is a standardized z-score
//  vector[N] year; //year for each observation
// ^ year is pulled out because we're working with dif and the magnitude of changes is not so huge that I expect exponential change to really be having an effect over time. There's a check for species having issues with this assumption, and as of 2025.06.06 all the species pass
  vector[N] change_C; //change in count since the last survey for each row, vector bc it doesn't have bounds like an array does
  }
  
  parameters {
//We use non-centered z-score variables to make the MCMC chain dramatically easier to fit. 
    vector[Nqrt] a_z; //fit an intercept for each q_route, z-score
    real a_bar; //'average q_route'
    real<lower=0> sig_a; //variance from average q_route
    
    real b_dev_change; //effect of change in development, across routes.
//   real b_year; //effect of year, across routes.
    real b_dev_base; //effect of development, across routes
    
    real<lower=0> sigma;
  
  }
  transformed parameters {
    vector[Nqrt] a = a_bar + a_z*sig_a; //how to transform a
  }
  
  model {
    //Non-vectorized alas! normal distribution now bc change_c can be negative
    for (n in 1:N) {
    change_C[n] ~ normal(
      a[qrt[n]] + 
      b_dev_change*change_dev[n] + 
//      b_year*year[n] + 
      b_dev_base*dev[n], 
      sigma);
    }
  
  //Vectorize attempt
  //  C ~ neg_binomial(exp(a[qrt] + b[qrt] * change_dev), overdispersion_param);

    //priors
   // lambda = exp(a[]); //intercept only right now. in a more advanced model, you have eg. + b*year + b*landcover
   //we substitute the equasion for lambda directly into the model above. No need to mess around with where lambda gets declared - it's a placeholder value anyway.
    a_z ~ normal(0,1); //partially pool the intercepts
    a_bar ~ normal(0,2); //or some prior. this is kinda large and maybe should be narrowed but whatever. Let's go with it for now.
    sig_a ~ exponential(1);
    
    
    //there is one effect of change in urbanization across routes
    b_dev_change ~ normal(0,1);
    //there is one effect of year across routes
//    b_year ~ normal(0,1);
    //there is one effect of baseline urbanization across routes
    b_dev_base ~ normal(0,1);
    
    //just a normal distribution, so we'll model sigma with exponential
    sigma ~ exponential(1);
    
}
