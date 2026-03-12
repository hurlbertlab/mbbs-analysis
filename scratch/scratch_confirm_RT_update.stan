// This stan model is for testing that regional trend is working as expected

data {
  int<lower=0> N; // number of observations or rows
  int<lower=1> Nsp; // number of species
  array[N] int<lower=1, upper=Nsp> sp; //species id for each observation
  vector[Nsp] regional_trend_mean; //mean for each sp regional trend
  vector[Nsp] regional_trend_sd; //sd for each sp regional trend
  
}

parameters {
  
 
  //traits variables predicting betas
  vector[Nsp] regional_trend; //each species has one regional trend, sampled from the mean,sd distribution provided in the data block
//new version trying to get a better fit..
  real mu_regional_trend;
  real<lower = 0> sigma_regional_trend;
//prev version that produces an almost-perfect 1:1 line with the actual values fed in
//  vector[Nsp] mu_regional_trend;
//  vector<lower=0>[Nsp] sigma_regional_trend;

}

model {

  mu_regional_trend ~ normal(0, 1.5); 
  //since regional trend is now scaled let's give it a wider and more reasonable parameter than .15
  sigma_regional_trend ~ normal(0, .5); //half normal
  regional_trend ~ normal(mu_regional_trend, sigma_regional_trend);
  regional_trend_mean ~ normal(regional_trend, regional_trend_sd);

  
}

