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
load_from_bdev <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.04.11_traits_on_bdev_add_logsize/"
load_from_uai <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.04.15_uai_on_bdev/"
load_from_change <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.06.06_change_per_change_woyear/"

######### section for fitting bayesplot themes
  #let's make text larger :)
  bayesplot_theme_update(text = element_text(size = 20, family = "sans")) 
  #families are "serif" for times new roman, "sans" for TT Arial, and "mono" for very typewriter
  bayesplot_theme_set(theme_minimal())
  color_scheme_set("purple")
  color_scheme_set("blue")
######## 

####two-panel effect of b_yr and b_dev
  #!!!!!!!!!!!!!!! bayesplot has a built in grid function!!!! bayesplot_grid() - explore that later
  
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
             prob = 0.01,
             prob_outer = 0.95) + 
    geom_vline(xintercept = 0, color = "grey30") 
  
  year_plot <- mcmc_intervals(sorted_year,
             prob = 0.01, 
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
  tbdev_samples <- read.csv(paste0(load_from_bdev, "posterior_samples1-400.csv"))
  #so, this is still about double the number of samples from the first, but thankfully once we cut it to the first 400 bootstrap runs we can actually get some info!
  
  mcmc_intervals(tbdev_samples,
                 regex_pars = "^b_")
  #need to bring back info about what groups are each b_size[number]
  species_variables <- read.csv(paste0(load_from_bdev, "species_variables.csv"))%>%
    select(SPECIES_GROUP, group_standard) %>%
    arrange(group_standard) %>%
    group_by(SPECIES_GROUP, group_standard) %>%
    summarize(n = n()) %>%
    ungroup()
  #and probably plot the size effects and b_ s differently
  
  size_graphing <- tbdev_samples %>%
    select(`b_size_group[1]`:`b_size_group[24]`) %>%
    rename_with(
      ~paste0(
        species_variables$SPECIES_GROUP[
          match(
            as.numeric(str_extract(.x, "\\d+")),  # Extract all digits
            species_variables$group_standard
          )], 
        " (",
        species_variables$n[
          match(
            as.numeric(str_extract(.x, "\\d+")),  # Same extraction here
            species_variables$group_standard
          )],
        ")"
      ),
      matches("^b_size")
    )
    
  mcmc_intervals(size_graphing) +
    labs(title = "No Effect of Species Mass")
  
  #okay and now if we want to graph just the main effects. Let's also rename
  main_effects_graphing <- tbdev_samples %>%
    select(b_climate, b_habitat, b_log_size, b_bar_size) %>%
    rename("Climate Position" = b_climate,
           "Forest Selectivity" = b_habitat,
           "Log Mass" = b_log_size,
           "Within-Group Mass" = b_bar_size)
  mcmc_intervals(main_effects_graphing) +
    geom_vline(xintercept = 0,
               col = "black",
               lty = "dashed")

  
################################uai
  uai <- read.csv(paste0(load_from_uai, "posterior_samples.csv")) %>%
    dplyr::rename("UAI" = b_uai)
  
  mcmc_intervals(uai,
                 pars = "UAI") +
    xlab("Effect of Development") + 
    xlim(-0.5, 2) +
    vline_0(lty = "dashed")
  
  
######################### landcover change per change
  
  
  #####b_yr and b_change and b_dev
  species_list <- read.csv(paste0(load_from_change,"species_list.csv"))
  posterior_samples <- read.csv(paste0(load_from_change,"posterior_samples.csv")) %>%
    filter(is.na(common_name) == FALSE,
           common_name %in% species_list$common_name)
  
  dev_change <- posterior_samples %>%
    seperate_betas_pivot(column_select_list = c("common_name", "row_id", "b_dev_change"),
                         values_from_column = "b_dev_change")
  param_means <- colMeans(dev_change)
  sorted_params <- names(sort(param_means))
  sorted_dev_change <- dev_change[,sorted_params]
  
  dev_base <- posterior_samples %>%
    seperate_betas_pivot(column_select_list = c("common_name", "row_id", "b_dev_base"),
                         values_from_column = "b_dev_base")
  param_means <- colMeans(dev_base)
  sorted_params <- names(sort(param_means))
  sorted_dev_base <- dev_base[,sorted_params]
    

  color_if_zero_crossed <- function(x) {
    ifelse(x[1] < 0 & x[2] > 0, "grey50", "black")  # Interval crosses 0 → grey
  }
  
  dev_plot <- mcmc_intervals(sorted_dev_change,
                             prob = 0.01,
                             prob_outer = 0.95,
                             ) +
    geom_vline(xintercept = 0, color = "grey30") +
    ggtitle("Change in Development on Change in Count")
  
  dev_plot
  
  