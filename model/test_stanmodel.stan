// Modeled after gdicecco bbs_abund_mod.stan. Poisson model of BBS counts at each stateroute, groups by species

// model.stan
data {
  int<lower=0> N;              // number of observations
  int<lower=1> n_sites;        // number of sites
  int<lower=1> n_years;        // number of years
  int<lower=1, upper=n_sites> site[N]; // site index
  int<lower=1, upper=n_years> year[N]; // year index
  int<lower=0> count[N];       // observed counts
}

parameters {
  real mu_site[n_sites];       // site-level effects
  real mu_year[n_years];       // year-level effects
}

model {
  // Priors
  mu_site ~ normal(0, 10);
  mu_year ~ normal(0, 10);

  // Likelihood
  for (i in 1:N) {
    count[i] ~ poisson(exp(mu_site[site[i]] + mu_year[year[i]]));
  }
}
