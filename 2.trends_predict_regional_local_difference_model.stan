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

data {
  int<lower=0> N; // number of observations or rows
  int<lower=1> Nsp; // number of species
  int<lower=1> Nsp_sample; //number of samples per species
  array[N] int<lower=1, upper=Nsp> sp; //species id for each observation
  vector[N] sample_local; //local trend for each row
  vector[N] sample_regional; //regional trend for each row
  vector[N] D; //there is a difference between local and regional for every row!
    array[Nsp] int<lower=1, upper = Nsp> sp_t; //species ID to associate with each species trait
  vector[Nsp] t_climate_pos; //climate position value for every species 
  vector[Nsp] t_habitat_selection; //ndvi habitat
  
}

parameters {
  
  
  
  
  
  
  matrix[Nsp, Nrt] a; //species-route interaction matrix, fit a species trend along each sp+rt combo.
  vector[Nsp] a_bar; // the intercept eg. initial count at yr 0, fit one per species. species-level mean for the intercept a
  vector<lower=0>[Nsp] sigma_a; //standard deviation in a, if we're fitting a_bar we ought to also fit a species-specific sigma_a... if they all look similar we can re-assess
  //vector[Nsprt] a; //species trend along a specific route, fit one for each sp+rt combo
  
  vector[Nsp] b; //species trend, fit one for each species
  real gamma_b; //intercept for species trends. calculated across species, and we only want one value, so this is not a vector.
//  real kappa_regional; //effect of t_regional on betas. real b/c we only want one.
  real kappa_climate_pos; //effect of t_climate_pos on betas. real b/c we only want one.
  real kappa_habitat_selection; //effect of t_habitat_selection on betas. real b/c we only want one.
  real<lower=0> sig_b; //deviation from explanatory power of the traits on predicting the trends. Represents residual variance / measure of scatter. ...In some ways, R2??
  
  vector[Nobs] c; //effect of observer, fit one for each observer
  real gamma_c; //fit one intercept across observers. Let's keep this simple, bc we don't need to super complicate the role observers play
  real kappa_obs; //fit one effect of observer quality
  real <lower=0> sig_c; //deviation btwn observed offset for count and score I gave each observer
  
}

model {

// Non-vectorized, so slower than it could be. Let's ignore speed and work on content.
   for (n in 1:N) {
     C[n] ~ poisson_log(a[sp[n], rt[n]] + b[sp[n]] * year[n] + c[obs[n]]);
   }
// eg... for every row/observation in the data.
// The count is a function of the poisson distribution log(lambda), and lamda modeled by (literally subbed in, didn't bother with a lambda intermediary step) the species trend along a species+route combo, the b*year overall trend, and observer quality.

  to_vector(a) ~ normal(a_bar[sp_sprt], sigma_a[sp_sprt]); //use sp_sprt to index a_bar and sigma_a because a is a vector of length sp*rt after we to_vector the matrix
  a_bar ~ normal(2, 2); //bc a_bar is a vector of sp_a, fits once for each species. Keeping narrow for now...with bad neffs the average ranges from -4 -> 4, so perhaps this prior should be expanded. Fitting it mean 2 with stdev of 2, so 0 is a reasonable value and as is 0. Gives it a little more space to explore.
  sigma_a ~ exponential(1);
//  a ~ normal(a_bar[sp_sprt], sigma_a); //sp_sprt maps species+route combos to species
//  a_bar ~ normal(1, 0.5); //bc a_bar is a vector of sp_sprt, fits one for each sp.
//  sigma_a ~ exponential(1);
  
  b ~ normal(gamma_b + 
            // kappa_regional*t_regional[sp_t] +
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

    c ~ normal(gamma_c + kappa_obs*observer_quality, sig_c); //observer quality may need some indexing? no, b/c dependent on both observer and route.
    //add priors for gamma_c and sig_c
    gamma_c ~ normal(0,0.5);
    sig_c ~ exponential(1);

}

generated quantities {
  vector[Nsp] d_bar;
  
  // Compute 'd_bar' based on 'b' and 'r'
  for (n in 1:Nboot) {
    d_bar[n] = b[sp[n]] - boot_regional[boot_sp[n]];  // Example computation, replace with your actual formula
  }
  
}
