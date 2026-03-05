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
  vector[Nsp] mu_regional_trend;
  vector<lower=0>[Nsp] sigma_regional_trend;

}

model {

  mu_regional_trend ~ normal(0, .15); 
  //expect regional trend to vary between like -6.38 and 3.78. ALRIGHT OKAY, so, here's a possible reason the effect size is so small? The USGS data is on a different scale!
  //If I rescale the USGS data and divide by 100, so it's on the same scale as the regional trend at the end, would that help? I think it ought to!
  //I'll leave this here as is and rescale on the .R script side. Once that's fixed this 0,.15 should be fine, if a little wide for the actual mean/max seen in the data, but I think that's okay.
  sigma_regional_trend ~ normal(0, .5); //half normal
  regional_trend ~ normal(mu_regional_trend, sigma_regional_trend);
  regional_trend_mean ~ normal(regional_trend, regional_trend_sd);

  
}

