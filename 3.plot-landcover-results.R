##################################
#
# Use bayesplot to plot figures
# for chapter 2. 
# Figures include:
# two-panel effect of b_yr and b_dev
#
#
##################################

library(dplyr)
library(stringr)
library(bayesplot)

########
#section for fitting bayesplot themes

########


###
#two-panel effect of b_yr and b_dev
#separate out into two dfs, then pivot back to assumed structure of columns with 4000 rows
#for bayesplotting.