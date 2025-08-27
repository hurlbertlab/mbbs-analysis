##################################
#
# Plot figures
# for chapter 2. 
# Figures include:
# two-panel effect of b_yr and b_dev
# effect of habitat/climate/size on b_dev
#
##################################

library(dplyr) #%>% and functions
library(stringr) #for selecting based on text
library(ggplot2) #maybe helpful for plotting
library(bayesplot) #package to plot posterior samples
library(cowplot) #used to make multi-panel figures

#load functions
source("3.plot-functions.R")

#where are we pulling data from?
load_from <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.03.27_loopdevelopment/"
#load_from_bdev <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.04.11_traits_on_bdev_add_logsize/"
#load_from_uai <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.04.15_uai_on_bdev/"
load_from_change <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.06.26_cpc_rmbaseline_obsqual/"
load_from_change_together <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.07.25_cpc_allsp_in_one/"
load_from_cpc_traits <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.07.14_traits_on_cpc/"
load_from_cpc_traits_together <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.07.29_traits_on_cpc/"