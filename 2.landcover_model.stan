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
  array[N] int<lower=0> C; //count for each row
  }
  
  parameters {
    vector[Nqrt] a; //fit an intercept for each q_route
    real a_bar; //'average q_route'
    real<lower=0> sig_a; //variance from average q_route
    
    real<lower=0> overdispersion_param;
    //define lambda and overdis param here?
  
  }
  
  model {
    //Non-vectorized alas! neg binomial
    for (n in 1:N) {
    C[n] ~ neg_binomial(exp(a[qrt[n]]), overdispersion_param);
    }
  
  //Vectorize attempt
  //  C ~ neg_binomial_2(exp(a[qrt]), overdispersion_param);

    //priors
   // lambda = exp(a[]); //intercept only right now. in a more advanced model, you have eg. + b*year + b*landcover
   //we substitute the equasion for lambda directly into the model above. No need to mess around with where lambda gets declared - it's a placeholder value anyway.
    a ~ normal(a_bar, sig_a); //partially pool the intercepts
    a_bar ~ normal(2,2); //or some prior. this is kinda large and maybe should be narrowed but whatever. Let's go with it for now.
    sig_a ~ exponential(1);
    //chatgpt says typical prior for the overdispersion param is a gamma prior. I've seen a beta used as well, some questions remain here.
    overdispersion_param ~ gamma(2,1);
    
}
