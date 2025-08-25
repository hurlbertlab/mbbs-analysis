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
  array[N] int<lower=1, upper=Nsp> sp; //species id for each observation
  array[N] int<lower=1, upper=Nrt> rt; //route id for each observation
  array[N] int<lower=1, upper=Nyr> year; //year for each observation
//.............observer section....................................
  vector[N] observer_quality; //there is an observer_quality for every observation [vector of Nobs otherwise]
//..............count................................................
  array[N] int<lower=0> C; // there is a count (my y variable!) for every row, and it is a bounded integer that is at least 0.
//...............predictor variables.....................
  vector[Nsp] t_temp_pos; //temp position value for every species 
  vector[Nsp] t_habitat_selection; //ndvi habitat selection for every species
  vector[Nsp] t_diet_cat; //diet category for every species
  //here, you give it the LENGTH of the vector (Nsp), eg. same as the number of species. Later, in the model section, you give it the SPECIES (sp)
  vector[Nsp] regional_trend_mean; //mean for each sp regional trend
  vector[Nsp] regional_trend_sd; //sd for each sp regional trend
  
}

parameters {
  matrix[Nsp, Nrt] a; //species-route interaction matrix, fit a species trend along each sp+rt combo.
  vector[Nsp] a_bar; // the intercept eg. initial count at yr 0, fit one per species. species-level mean for the intercept a
  vector<lower=0>[Nsp] sigma_a; //standard deviation in a, if we're fitting a_bar we ought to also fit a species-specific sigma_a... if they all look similar we can re-assess
  //vector[Nsprt] a; //species trend along a specific route, fit one for each sp+rt combo
  
  vector[Nsp] b; //species trend, fit one for each species
  real gamma_b; //intercept for species trends. calculated across species, and we only want one value, so this is not a vector.
//  real kappa_regional; //effect of t_regional on betas. real b/c we only want one.
  real kappa_temp_pos; //effect of t_temp_pos on betas. real b/c we only want one.
  real kappa_habitat_selection; //effect of t_habitat_selection on betas. real b/c we only want one.
  real<lower=0> sig_b; //deviation from explanatory power of the traits on predicting the trends. Represents residual variance / measure of scatter. ...In some ways, R2??
  
  vector[Nsp] regional_trend; //one regional trend for every species.
  real kappa_regional; //one effect of regional trend on the b slopes across species
  
  real c_obs; //effect of observer, cross observers
  
  real<lower=0> overdispersion_param; //negative binomial overdispersion parameter
  
}

transformed parameters {
  
  matrix[Nsp, Nrt] a = a_bar + a_z*sigma_a;
  
}

model {

// Non-vectorized, so slower than it could be. Let's ignore speed and work on content.
   for (n in 1:N) {
     C[n] ~ neg_binomial(a[sp[n], rt[n]] + b[sp[n]] * year[n] + observer_quality[n], overdispersion_param);
   }
// eg... for every row/observation in the data.
// The count is a function of the poisson distribution log(lambda), and lamda modeled by (literally subbed in, didn't bother with a lambda intermediary step) the species trend along a species+route combo, the b*year overall trend, and observer quality.

##!!!! CHANGES HERE
  to_vector(a) ~ normal(a_bar[sp_sprt], sigma_a[sp_sprt]); //use sp_sprt to index a_bar and sigma_a because a is a vector of length sp*rt after we to_vector the matrix
  a_bar ~ normal(2, 2); //bc a_bar is a vector of sp_a, fits once for each species. Keeping narrow for now...with bad neffs the average ranges from -4 -> 4, so perhaps this prior should be expanded. Fitting it mean 2 with stdev of 2, so 0 is a reasonable value and as is 0. Gives it a little more space to explore.
  sigma_a ~ exponential(1);
//  a ~ normal(a_bar[sp_sprt], sigma_a); //sp_sprt maps species+route combos to species
//  a_bar ~ normal(1, 0.5); //bc a_bar is a vector of sp_sprt, fits one for each sp.
//  sigma_a ~ exponential(1);
  
  b ~ normal(gamma_b + 
           //  kappa_regional*t_regional[sp_t] +
             kappa_temp_pos*t_temp_pos[sp_t] +
             kappa_habitat_selection*t_habitat_selection[sp_t],
             sig_b);
  gamma_b ~ normal(0, 0.2);
  sig_b ~ exponential(1);
  
  //regional trend we already know the mean and standard deviation
  regional_trend ~ normal(regional_trend_mean, regional_trend_sd);
//.............BY COMMENTING OUT KAPPAS, DEFAULT UNIFORM PRIORS ARE USED.................
//  kappa_regional ~ normal(0, .02); 
//  kappa_temp_pos ~ normal(0, .2); 
//  kappa_habitat_selection ~ normal(0, .2);
//.............^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^..................

//    c ~ normal(gamma_c + kappa_obs*observer_quality, sig_c); //observer quality may need some indexing? no, b/c dependent on both observer and route.
    //add priors for gamma_c and sig_c
//    gamma_c ~ normal(0,0.5);
//    sig_c ~ exponential(1);

    //chatgpt says typical prior for the overdispersion param is a gamma prior. I've seen a beta used as well, some questions remain here.
    overdispersion_param ~ gamma(2,1);

}
