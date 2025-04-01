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
library(ggplot2)
library(bayesplot)
library(cowplot) #used to make multi-panel figures

#where are we pulling data from?
load_from <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.03.27_loopdevelopment/"

######### section for fitting bayesplot themes
  #let's make text larger :)
  bayesplot_theme_update(text = element_text(size = 12, family = "sans")) 
  #families are "serif" for times new roman, "sans" for TT Arial, and "mono" for very typewriter
  bayesplot_theme_set(theme_minimal())
  color_scheme_set("purple")
######## 


####two-panel effect of b_yr and b_dev
  
#also going to filter to just species that we're keeping.
  species_list <- read.csv(paste0(load_from, "species_list.csv"))
  posterior_samples <- read.csv(paste0(load_from, "posterior_samples.csv")) %>%
    #filter out NA row and for common_names only in the list of species we're interested in
    filter(is.na(common_name) == FALSE,
           common_name %in% species_list$common_name)
  
  #separate out into two dfs, pivot both back to assumed structure of columns with 4000 rows bayesplot wants
seperate_betas_pivot <- function(posterior_samples, column_select_list, values_from_column) {
  x <- posterior_samples %>%
    dplyr::select(all_of(column_select_list)) %>%
    tidyr::pivot_wider(names_from = common_name, 
                       values_from = {{values_from_column}}) %>%
    dplyr::select(-row_id)
}  
  
  dev <- posterior_samples %>%
    seperate_betas_pivot(column_select_list = c("common_name", "row_id", "b_dev"),
                         values_from_column = "b_dev")

  year <-  posterior_samples %>%
    seperate_betas_pivot(column_select_list = c("common_name", "row_id", "b_year"),
                         values_from_column = "b_year")
  
  #sort the columns so they plot in a nice stack according to the effect of development
  #Calculate the mean of each parameter
  param_means <- colMeans(dev)
  #sort the parameters by their means
  sorted_params <- names(sort(param_means))
  #now, apply that sort
  sorted_dev <- dev[,sorted_params]
  sorted_year <- year[,sorted_params]
  
  
  dev_plot <- mcmc_intervals(sorted_dev,
             prob_outer = 0.95) + 
    geom_vline(xintercept = 0, color = "grey30") 
  
  year_plot <- mcmc_intervals(sorted_year,
             prob_outer = 0.95) +
    geom_vline(xintercept = 0, color = "grey30") +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank())
  
  cowplot::plot_grid(dev_plot, year_plot,
                     rel_widths = c(2,1))
  #move the labels so they're underneath
  #and like scale the x and y so they fit nicer in the final plot?
  #seems like rather than the slopes, want to plot these with like, the same way Allen plots for all the species of plants....like with a thick bar representing the middle of the distribution and then a thin bar outside that
  
  