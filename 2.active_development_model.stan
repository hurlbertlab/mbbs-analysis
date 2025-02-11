//
// This Stan program defines a hierarchical model
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//
data {
  int<lower=0> N; // number of observations or rows
  int<lower=1> Nsp; // number of species
  int<lower=1> Nsprt; // number of species+route combinations
  int<lower=1> Nyr; //number of years
  array[N] int<lower=1, upper=Nsp> sp; //species id for each observation
  array[N] int<lower=1, upper = Nsprt> sprt; //species+route combo for each observation
  array[Nsprt] int<lower=1, upper=Nsp> sp_sprt; //species id for each species+route combo
  //note: the 'for each x' that 'x' is what the array length is.
  array[N] int<lower=1, upper=Nyr> year; //year for each observation
  vector[N] observer_quality; //there is an observer_quality for each observation..but not really! 
//...........................................
//when I back back in observer intercepts or w/e...
// int<lower=1> Nobs; //number of observers
// array[N] int<lower=1, upper=Nobs> obs; //there is an observer for every observation
// vector[obs] observer_quality; //there is an observer_quality for every observer
//...........................................
  array[N] int<lower=0> C; // there is a count (my y variable!) for every row, and it is an unbounded integer that is at least 0.
//.................okay, now for the predictor variables.....................
  array[Nsp] int<lower=1, upper = Nsp> sp_t; //species ID to associate with each species trait
  vector[Nsp] t_regional; //regional trait value for every species 
  vector[Nsp] t_climate_pos; //climate position value for every species 
  vector[Nsp] t_habitat_selection; //ndvi habitat selection for every species
  //here, you give it the LENGTH of the vector (Nsp), eg. same as the number of species. Later, in the model section, you give it the SPECIES (sp)
  
}

parameters {
  vector[Nsp] b; //species trend, fit one for each species
  vector[Nsprt] a; //species trend along a specific route, fit one for each sp+rt combo
  vector[Nsp] a_bar; // the intercept eg. initial count at yr 0, fit one per species
  real<lower=0> sigma_a; //standard deviation in a
  real gamma_b; //intercept for species trends. calculated across species, and we only want one value, so this is not a vector.
  real kappa_regional; //effect of t_regional on betas. real b/c we only want one.
  real kappa_climate_pos; //effect of t_climate_pos on betas. real b/c we only want one.
  real kappa_habitat_selection; //effect of t_habitat_selection on betas. real b/c we only want one.
  real<lower=0> sig_b; //deviation from explanatory power of the traits on predicting the trends. Represents residual variance / measure of scatter. ...In some ways, R2??
  
}

model {

// Non-vectorized, so slower than it could be. Let's ignore speed and work on content.
   for (n in 1:N) {
     C[n] ~ poisson_log(a[sprt[n]] + b[sp[n]] * year[n] + observer_quality[n]);
   }
// eg... for every row/observation in the data.
// The count is a function of the poisson distribution log(lambda), and lamda modeled by (literally subbed in, didn't bother with a lambda intermediary step) the species trend along a species+route combo, the b*year overall trend, and observer quality.

  a ~ normal(a_bar[sp_sprt], sigma_a); //sp_sprt maps species+route combos to species
  a_bar ~ normal(1, 0.5); //bc a_bar is a vector of sp_sprt, fits one for each sp.
  sigma_a ~ exponential(1);
  
  b ~ normal(gamma_b + 
             kappa_regional*t_regional[sp_t] +
             kappa_climate_pos*t_climate_pos[sp_t] +
             kappa_habitat_selection*t_habitat_selection[sp_t],
             sig_b);
  gamma_b ~ normal(0, 0.2);
  sig_b ~ exponential(1);
//.............BY COMMENTING OUT KAPPAS, DEFAULT UNIFORM PRIORS ARE USED.................
//  kappa_regional ~ normal(0, .02); 
//  kappa_climate_pos ~ normal(0, .2); 
//  kappa_habitat_selection ~ normal(0, .2);
//.............^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^..................
}

