################################
#
# Testing out plotting with bayesplot.
#
################################

library(rstan)
library(bayesplot)
library(ggplot2)

#let's make text larger :)
bayesplot_theme_update(text = element_text(size = 14, family = "sans")) 
#families are "serif" for times new roman, "sans" for TT Arial, and "mono" for very typewriter
bayesplot_theme_set(theme_minimal())

x <- example_mcmc_draws()
mcmc_hist(x)

#change color scheme
color_scheme_set("brightblue")

#panel_bg() can change the color of the panel background without changing the bayesplot theme
mcmc_hist(x) + panel_bg(fill = "black")

#ok! let's load in a stanfit
  load_from <- "Z:/Goulden/mbbs-analysis/model/2025.02.11_allspecies_traits_index_fixed/"
  stanfit <- "stanfit.rds"
  
#Load the stanfit object
fit <- readRDS(paste0(load_from, stanfit))
fit_summary <- read.csv(paste0(load_from, "fit_summary.csv")) 
fit@model_pars

  esa <- fit_summary[1:61,]

#plot posterior distributions
  posterior <- as.data.frame(fit)
  
  plot_title <- ggtitle("Posterior distributions",
                        "with medians and 80% intervals")
  
  mcmc_areas(posterior,
             pars = c("b[1]", "b[2]"),
             prob = 0.8) +
    plot_title

  #better than b[1], b[2] etc. would be to have the names of the species. This might be something we ought to save as well.
  convert_betas <- read.csv(paste0(load_from, "beta_to_common_name.csv"))
  nbetas <- nrow(convert_betas)
  colnames(posterior)[1:nbetas] <- convert_betas$common_name[1:nbetas]
  
  #okay cool, now let's plot again!
  mcmc_areas(posterior, 
             pars = convert_betas$common_name,
             prob = 0.8) + 
    plot_title
  #yahaha!
  #okay, now can I arrange by how the species are doing?
  
  #deepseek says:
  # trim my posterior down to just those parameters of interest
  sorted_posterior <- posterior[, convert_betas$common_name]
  # Calculate the mean of each parameter
  param_means <- colMeans(sorted_posterior)
  #sort the parameters by their means
  sorted_params <- names(sort(param_means))
  #subset the posterior...
  sorted_posterior <- posterior[,sorted_params]
  
  mcmc_areas(sorted_posterior,
             prob = 0.95) + #oh! okay yay that did work! C:
    ggtitle("Posterior distributions",
            "with median and 95% interval") +
    geom_vline(xintercept = 0, color = "grey30")
  #use a ridgeline plot for hierarchically related parameters, like b[1] and then that species' a[1,1] thru a[1,36] so, across all the routes show how the species is doing
  #eg
  m <- shinystan::eight_schools@posterior_sample
  mcmc_areas_ridges(m, 
                    pars = "mu",
                    regex_pars = "theta",
                    border_size = 0.75) +
    ggtitle("Treatment effect on eight schools (Rubin, 1981)")
  