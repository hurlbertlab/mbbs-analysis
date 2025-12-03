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
  vector[Nsp] t_temp_pos; //temp position value for every species 
  vector[Nsp] t_habitat_selection; //ndvi habitat selection for every species
  array[Nsp] int<lower=1, upper=Ndc> t_diet_cat; //diet category for every species
  vector[Nsp] regional_trend_mean; //mean for each sp regional trend
  vector[Nsp] regional_trend_sd; //sd for each sp regional trend
  
}

parameters {
  
  //intercept
  real a; //universal base intercept, taking the mean out of the intercept distribution and treating it as a constant plus a species and route gaussian distribution centered on zero
  vector[Nsp] a_sp_raw; //intercept for each unique sp
  real<lower = 0> sig_sp; //variance in species-specific intercepts
  vector[Nrt] a_rt_raw; //intercept for each unique route
  real<lower = 0> sig_rt; //variance in route-specific intercepts
  
//...............PREV MATRIX METHOD OF INTERCEPT.......................
//  matrix[Nsp, Nrt] a; //species-route interaction matrix, fit a species trend along each sp+rt combo.
//  vector[Nsp] a_bar; // the intercept eg. initial count at yr 0, fit one per species. species-level mean for the intercept a
//  vector<lower=0>[Nsp] sigma_a; //standard deviation in a, if we're fitting a_bar we ought to also fit a species-specific sigma_a... if they all look similar we can re-assess
  //vector[Nsprt] a; //species trend along a specific route, fit one for each sp+rt combo
//................................................................
  
  //calculating species trend year effect slope
  vector[Nsp] b; //species trend, fit one for each species
  real gamma_b; //intercept for the distribution of species trends. calculated across species, and we only want one value, so this is not a vector.
  real<lower = 0> sig_b; //variance in the slope across species, also in some ways deviation from the explanatory power of the traits on predicting trends. Represents residual variance / measure of scatter. In some ways like R2 a bit?
  
  //traits variables predicting betas
  vector[Nsp] regional_trend; //each species has one regional trend, sampled from the mean,sd distribution provided in the data block
  vector[Nsp] mu_regional_trend;
  vector<lower=0>[Nsp] sigma_regional_trend;
  real kappa_regional; //one effect of regional trend on slopes across species
  real kappa_habitat_selection; //one effect of habitat selectivity on slopes across species
  real kappa_temp_pos; //one effect of temperature niche position on slopes across species
  vector[Ndc] kappa_diet_cat; //each diet category may have a different effect on slopes across species.
  
  real c_obs; //effect of observer, calculated across all observers since we use a quantitative measure of observer quality rather than a vector of observer identifiers
  
  real<lower=0> overdispersion_param; //negative binomial overdispersion parameter
  
}

transformed parameters {
  //old matrix method
  //matrix[Nsp, Nrt] a = a_bar + a_z*sigma_a;
  
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
 
//modeling regional trend
//regional trend we already know the mean and standard deviation
//  regional_trend ~ normal(regional_trend_mean, regional_trend_sd); //prev version, updating with Casey's notes:
  mu_regional_trend ~ normal(0, .15); 
  //expect regional trend to vary between like -6.38 and 3.78. ALRIGHT OKAY, so, here's a possible reason the effect size is so small? The USGS data is on a different scale!
  //If I rescale the USGS data and divide by 100, so it's on the same scale as the regional trend at the end, would that help? I think it ought to!
  //I'll leave this here as is and rescale on the .R script side. Once that's fixed this 0,.15 should be fine, if a little wide for the actual mean/max seen in the data, but I think that's okay.
  sigma_regional_trend ~ normal(0, .5); //half normal
  regional_trend ~ normal(mu_regional_trend, sigma_regional_trend);
  regional_trend_mean ~ normal(regional_trend, regional_trend_sd);

 
//kappa priors
//.............BY COMMENTING OUT KAPPAS, DEFAULT UNIFORM PRIORS ARE USED.................
//default uniform priors are not like ultimately reccomended, and since variables are scaled it's not unreasonable to set a normal(0,1) prior, but default uniform Is Workable...
//see: https://github.com/stan-dev/stan/wiki/prior-choice-recommendations
    kappa_regional ~ normal(0, 1);
    kappa_temp_pos ~ normal(0, 1);
    kappa_habitat_selection ~ normal(0, 1);
    kappa_diet_cat ~ normal(0, 1);

//overdispersion param prior
//chatgpt says typical prior for the overdispersion param is a gamma prior. I've seen a beta used as well, some questions remain here.
    overdispersion_param ~ gamma(2,1);
    
//modeling beta
  gamma_b ~ normal(0, 0.2);
  sig_b ~ normal(0, .5); //half normal
  b ~ normal(gamma_b + 
             kappa_regional * regional_trend + 
             kappa_temp_pos * t_temp_pos + 
             kappa_habitat_selection * t_habitat_selection + 
             kappa_diet_cat[t_diet_cat], //categorical so no *
             sig_b);

// Vectorized
C ~ neg_binomial_2_log(eta, overdispersion_param);

//notes from previous versions below here.

// Non-vectorized, so slower than it could be. Let's ignore speed and work on content.
//   for (n in 1:N) {
//     C[n] ~ neg_binomial_2_log(
//       a + //universal intercept
//       a_sp[sp[n]] + //species specific intercept modifier
//       a_rt[rt[n]] + //route specific intercept modifier
//       b[sp[n]] * year[n] + //slope for effect of time
//       c_obs * observer_quality[n], //slope effect for observer quality
//       overdispersion_param //controls for overdispersion in a neg_binom
//       );
//   }
// eg... for every row/observation in the data.
// The count is a function of the poisson distribution log(lambda), and lamda modeled by (literally subbed in, didn't bother with a lambda intermediary step) the species trend along a species+route combo, the b*year overall trend, and observer quality.

//previous method
//  to_vector(a) ~ normal(a_bar[sp_sprt], sigma_a[sp_sprt]); //use sp_sprt to index a_bar and sigma_a because a is a vector of length sp*rt after we to_vector the matrix
//  a_bar ~ normal(2, 2); //bc a_bar is a vector of sp_a, fits once for each species. Keeping narrow for now...with bad neffs the average ranges from -4 -> 4, so perhaps this prior should be expanded. Fitting it mean 2 with stdev of 2, so 0 is a reasonable value and as is 0. Gives it a little more space to explore.
//  sigma_a ~ exponential(1);
////  a ~ normal(a_bar[sp_sprt], sigma_a); //sp_sprt maps species+route combos to species
////  a_bar ~ normal(1, 0.5); //bc a_bar is a vector of sp_sprt, fits one for each sp.
////  sigma_a ~ exponential(1);
  
//    c ~ normal(gamma_c + kappa_obs*observer_quality, sig_c); //observer quality may need some indexing? no, b/c dependent on both observer and route.
    //add priors for gamma_c and sig_c
//    gamma_c ~ normal(0,0.5);
//    sig_c ~ exponential(1);

}
