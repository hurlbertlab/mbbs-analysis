####
#
# Scratch how to check models after being run
#
####

check_hmc_diagnostics(fit)
mcmc_intervals(fit, regex_pars = "^b_size")