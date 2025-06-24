##########################
#
# Testing the effects of species traits
# on how strongly a species' count changes
# in response to change in perc. developed
# AND percent forest. Test both here.
#
#
## traits:
# temperature niche position (where NC is in the species temperature range)
# ebird-derived forest selectivity
# ebird-derived grasslands selectivity
# Neate-Clegg et al. Urban Association Index
#
#########################

library(dplyr)
library(rstan)
library(stringr)
unloadNamespace("rethinking") #just in case, can interfere