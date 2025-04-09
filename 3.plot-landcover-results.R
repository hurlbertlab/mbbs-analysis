##################################
#
# Use bayesplot to plot figures
# for chapter 2. 
# Figures include:
# two-panel effect of b_yr and b_dev
# effect of habitat/climate/size on b_dev
#
##################################

library(dplyr)
library(stringr)
library(ggplot2)
library(bayesplot)
library(cowplot) #used to make multi-panel figures

#where are we pulling data from?
load_from <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.03.27_loopdevelopment/"
load_from_bdev <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.04.07_traits_on_bdev_fixsize_bootstrap/"

######### section for fitting bayesplot themes
  #let's make text larger :)
  bayesplot_theme_update(text = element_text(size = 12, family = "sans")) 
  #families are "serif" for times new roman, "sans" for TT Arial, and "mono" for very typewriter
  bayesplot_theme_set(theme_minimal())
  color_scheme_set("purple")
  color_scheme_set("blue")
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
  
  
# hist of species more strongly affected by development than by year
  df_hist <- read.csv(paste0(load_from, "fit_summaries.csv")) %>%
    #filter to just species we're keeping, and we only want our slopes
    filter(common_name %in% species_list$common_name,
           is.na(slope) == FALSE) %>%
    dplyr::select(-NA., -q_rt_standard) %>%
    relocate(slope:common_name, .before = rownames) %>%
    dplyr::select(-rownames) 
  
  year_effect <- df_hist %>%
    filter(slope == "year") %>%
    mutate(year_effect = mean) %>%
    select(common_name, year_effect)
  
  df_hist <- df_hist %>%
    filter(slope == "dev") %>%
    left_join(year_effect) %>%
    mutate(dev_minus_year = mean - year_effect)
  
  hist(df_hist$dev_minus_year)
  plot(df_hist$mean, df_hist$year_effect) + 
    abline(h = 0) + 
    abline(v = 0)
  
############################ effect of habitat/climate/size on b_dev
  posterior_samples <- read.csv(paste0(load_from_bdev, "posterior_samples1-400.csv"))
  #so, this is still about double the number of samples from the first, but thankfully once we cut it to the first 400 bootstrap runs we can actually get some info!
  
  mcmc_intervals(posterior_samples,
                 regex_pars = "^b_")
  #need to bring back info about what groups are each b_size[number]
  species_variables <- read.csv(paste0(load_from_bdev, "species_variables.csv"))%>%
    select(SPECIES_GROUP, group_standard) %>%
    arrange(group_standard) %>%
    group_by(SPECIES_GROUP, group_standard) %>%
    summarize(n = n())
  #and probably plot the size effects and b_ s differently
  
  size_graphing <- posterior_samples %>%
    select(b_size.1.:b_size.24.) %>%
    rename_with(
      ~paste0(
        species_variables$SPECIES_GROUP[match(
        as.numeric(gsub("b_size\\.(\\d+)\\.", "\\1", .x)),
        species_variables$group_standard)], 
        " (",
        species_variables$n[match(
          as.numeric(gsub("b_size\\.(\\d+)\\.", "\\1", .x)),
          species_variables$group_standard)],
        ")"),
      matches("^b_size")
    )
    
  mcmc_intervals(size_graphing) +
    labs(title = "No Effect of Species Mass")
  