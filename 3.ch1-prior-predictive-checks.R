#######################################
#
# Script for checking Ch1 model fits
# with prior predictive checks..
#
#
######################################

# 4. How to Validate Your Choice: Prior Predictive Checks
# 
# This is the most important step. You should never just use a default without checking. Simulate data from your prior (and your model structure) to see if the simulated data looks plausible.
# 
# Code a simple version of your model without the data (or with fake data).
# 
# Generate samples from the prior for all parameters, including phi.
# 
# Simulate count data y_sim using neg_binomial_2_rng(mu, phi).
# 
# Look at the distribution of the simulated counts. Do they have a reasonable amount of dispersion? Are there absurdly large counts? The prior for phi is good if the simulated data y_sim looks like real data you might expect to collect.
# 
# 
# This is a step I don't have in my modeling yet that I really need to incorporate