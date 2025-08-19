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

#load functions
source("3.plot-functions.R")

#where are we pulling data from?
load_from <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.03.27_loopdevelopment/"
load_from_bdev <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.04.11_traits_on_bdev_add_logsize/"
load_from_uai <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.04.15_uai_on_bdev/"
load_from_change <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.06.26_cpc_rmbaseline_obsqual/"
load_from_change_together <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.07.25_cpc_allsp_in_one/"
load_from_cpc_traits <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.07.14_traits_on_cpc/"
load_from_cpc_traits_together <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.07.29_traits_on_cpc/"

######### section for fitting bayesplot themes
  #let's make text larger :)
  bayesplot_theme_update(text = element_text(size = 20, family = "sans")) 
  #families are "serif" for times new roman, "sans" for TT Arial, and "mono" for very typewriter
  bayesplot_theme_set(theme_minimal())
  color_scheme_set(scheme = "purple")
 # color_scheme_set("blue")
######## 

  

####two-panel effect of b_yr and b_dev
  #!!!!!!!!!!!!!!! bayesplot has a built in grid function!!!! bayesplot_grid() - explore that later
  
#also going to filter to just species that we're keeping.
  species_list <- read.csv(paste0(load_from, "species_list.csv"))
  posterior_samples <- read.csv(paste0(load_from, "posterior_samples.csv")) %>%
    #filter out NA row and for common_names only in the list of species we're interested in
    filter(is.na(common_name) == FALSE,
           common_name %in% species_list$common_name)
  
  year_results <- read.csv(paste0(load_from, "fit_summaries.csv")) %>%
    filter(slope == "year") %>%
    left_join(read.csv(paste0(load_from_change_together,"species_list.csv")), by = "common_name") %>%
    filter(!is.na(sp_id)) %>%
    arrange(desc(mean)) %>%
    mutate(sp_id = row_number(),
           significant = ifelse(X2.5. < 0 & X97.5. > 0, FALSE, TRUE),
           color = case_when(mean > 0 & significant == TRUE ~ "forestgreen",
                             mean > 0 & significant == FALSE ~ "palegreen",
                             mean < 0 & significant == TRUE ~ "darkred",
                             mean < 0 & significant == FALSE ~ "salmon",
                             TRUE ~ "black"))
  plot_intervals(year_results, "Population Change Over Time", xlim_select = c(-.15, .13))
  
  dev <- posterior_samples %>%
    seperate_betas_pivot(column_select_list = c("common_name", "row_id", "b_dev"),
                         values_from_column = "b_dev")

  year <-  posterior_samples %>%
    seperate_betas_pivot(column_select_list = c("common_name", "row_id", "b_year"),
                         values_from_column = "b_year")
  
  #sort the columns so they plot in a nice stack according to the effect of year(prev.development)
  #Calculate the mean of each parameter
  param_means <- colMeans(year)
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
  
  #first, let's take a look at how the fits worked out. 
  dev_fit_summary <- read.csv(paste0(load_from_change, "dev+barren_fit_summaries.csv")) 
  max(dev_fit_summary$Rhat) #yay, just about 1
  hist(dev_fit_summary$n_eff)
  forest_fit_summary <- read.csv(paste0(load_from_change, "forest_fit_summaries.csv"))
  max(forest_fit_summary$Rhat)
  hist(forest_fit_summary$n_eff)
  
  dev_ps <- read.csv(paste0(load_from_change, "dev+barren_posterior_samples.csv"))  %>%
    filter(is.na(common_name) == FALSE,
           common_name %in% species_list$common_name)
  forest_ps <- read.csv(paste0(load_from_change, "forest_posterior_samples.csv")) %>%
    filter(is.na(common_name) == FALSE,
           common_name %in% species_list$common_name)
  
  dev_change <- dev_ps %>%
    seperate_betas_pivot(column_select_list = c("common_name", "row_id", "b_landcover_change"),
                         values_from_column = "b_landcover_change") %>%
    return_sorted_params()
  dev_base <- dev_ps %>%
    seperate_betas_pivot(column_select_list = c("common_name", "row_id", "b_landcover_base"),
                         values_from_column = "b_landcover_base") %>%
    return_sorted_params()
  forest_change <- forest_ps %>%
    seperate_betas_pivot(column_select_list = c("common_name", "row_id", "b_landcover_change"),
                         values_from_column = "b_landcover_change") %>%
    return_sorted_params()
  forest_base <- forest_ps %>%
    seperate_betas_pivot(column_select_list = c("common_name", "row_id", "b_landcover_base"),
                         values_from_column = "b_landcover_base") %>%
    return_sorted_params()
  
  dev_change_plot <- mcmc_intervals(dev_change,
                                    prob = 0.01, #set this to get ride of inner bars
                                    prob_outer = 0.95) +
    geom_vline(xintercept = 0, color = "grey30") +
    ggtitle("Change in Development on Change in Count")
  dev_base_plot <- mcmc_intervals(dev_base,
                                  prob = 0.01, #set this to get ride of inner bars
                                  prob_outer = 0.95) +
    geom_vline(xintercept = 0, color = "grey30") +
    ggtitle("Total Development Level on Change in Count (all NS)")
  forest_change_plot <- mcmc_intervals(forest_change,
                                       prob = 0.01,
                                       prob_outer = 0.95) +
    geom_vline(xintercept = 0, color = "green4") +
    ggtitle("Change in Forest on Change in Count")
  forest_base_plot <-mcmc_intervals(forest_base,
                                    prob = 0.01,
                                    prob_outer = 0.95) +
    geom_vline(xintercept = 0, color = "green4") +
    ggtitle("Effect of Total Forest on Change in Count (all NS)")
  
  
  
  
  
  dev_base <- posterior_samples %>%
    seperate_betas_pivot(column_select_list = c("common_name", "row_id", "b_dev_base"),
                         values_from_column = "b_dev_base")
  param_means <- colMeans(dev_base)
  sorted_params <- names(sort(param_means))
  sorted_dev_base <- dev_base[,sorted_params]
  
  dev_plot <- mcmc_intervals(sorted_dev_change,
                             prob = 0.01,
                             prob_outer = 0.95) +
    scale_color_manual(values = significant_change$color) +
    geom_vline(xintercept = 0, color = "grey30") +
    ggtitle("Change in Development on Change in Count")
  
  dev_plot
  
  
  
  
  
  posterior_samples <- read.csv(paste0(load_from_change,"posterior_samples.csv")) %>%
    filter(is.na(common_name) == FALSE,
           common_name %in% species_list$common_name)
  
  significant_change <- posterior_samples %>%
    group_by(common_name) %>%
    mutate(minconf = as.numeric(quantile(b_dev_change, probs = .025)),
           maxconf = as.numeric(quantile(b_dev_change, probs = .975))) %>%
    ungroup() %>%
    distinct(common_name, .keep_all = TRUE) %>%
    mutate(sig = ifelse(minconf < 0 & maxconf > 0, FALSE, TRUE)) %>%
    dplyr::select(-row_id)
    
  
  dev_change <- posterior_samples %>%
    seperate_betas_pivot(column_select_list = c("common_name", "row_id", "b_dev_change"),
                         values_from_column = "b_dev_change")
  param_means <- colMeans(dev_change)
  sorted_params <- names(sort(param_means))
  sorted_dev_change <- dev_change[,sorted_params]
  #fix things for color plotting
  order_dev_change <- as.data.frame(colnames(sorted_dev_change)) %>%
    mutate(order = row_number())
  significant_change <- significant_change %>%
    left_join(order_dev_change, by = c("common_name" = "colnames(sorted_dev_change)"))
  #wouldn't break this up but there's some error going on when they're together
  significant_change <- significant_change %>%
    arrange(order) %>%
    dplyr::mutate(color = ifelse(sig == TRUE, "black", "grey80"))
  #okay well. there's a color vector in the right order now. um. it kinda of seems like bayesplot isn't built for this kind of sig vs non-sig demonstration in this way. May require putting up an issue on the github or submitting a question. Otherwise like. idk. Going to need to plot twice, once with NA columns for the non-sig and the next with NA coloumns for the sig, both on the same plot, but dif colors associated with bayplot_theme(). hm
    
  #lets try to move off bayesplot. Especially when we are plotting intervals and not like the shape of the whole distribution, all I need are the 95% confidence intervals and the mean to plot. The trick will be figuring out how to have the y axis be the species names... could plot with y = speciesid or something like that, and then remake the axis bars to account for that. I think that would work nicely!
  
#################### change per change without bayesplot

  species_list <- read.csv(paste0(load_from_change_together,"species_list.csv"))
  dev <- read.csv(paste0(load_from_change_together, "dev+barren_fit_summaries.csv")) %>%
    #filter out the individual quarter route a[] fits, just keep the variables we're most interested in.
   # filter(rownames %in% c("a_bar", "sig_a", "b_landcover_change", "b_landcover_base", "c_obs", "sigma")) %>%
    #and really.. all we want is b_landcover_change here in plotting
   # filter(rownames %in% c("b_landcover_change")) %>%
    filter(!is.na(common_name),
           !is.na(slope)) %>%
    mutate(rm = ifelse(str_detect(.$rownames, "raw"), FALSE, TRUE)) %>%
    filter(rm == TRUE) %>%
    group_by(mean, common_name) %>%
    arrange(desc(mean)) %>% #
    mutate(sp_id = cur_group_rows()) %>% #sweet, indigio bunting with the most negative mean effect is at ID 66, Carolina Wren with the least negative effect is at ID 1.
    ungroup() %>%
    mutate(significant = ifelse(X2.5. < 0 & X97.5. > 0, FALSE, TRUE),
           color = ifelse(significant == FALSE, "lightblue", "blue"),
           pch = ifelse(significant == FALSE, 1, 19))
  
  forest_all <- read.csv(paste0(load_from_change, "forest_all_fit_summaries.csv")) %>%
    filter(rownames %in% c("b_landcover_change")) %>%
    group_by(mean, common_name) %>%
    arrange(desc(mean)) %>% #
    mutate(sp_id = cur_group_rows()) %>% #sweet, indigio bunting with the most negative mean effect is at ID 66, Carolina Wren with the least negative effect is at ID 1.
    ungroup() %>%
    mutate(significant = ifelse(X2.50. < 0 & X97.50. > 0, FALSE, TRUE),
           color = ifelse(significant == FALSE, "grey60", "black"),
           pch = ifelse(significant == FALSE, 1, 19))
  
  species_list <- species_list %>%
    left_join(forest_all[,17:18], by = "common_name") 
  
  forest_pos <- read.csv(paste0(load_from_change, "forest_positive_fit_summaries.csv")) %>%
    filter(rownames %in% c("b_landcover_change")) %>%
    left_join(species_list, by = "common_name") %>% #add the forest sorted sp id
    ungroup() %>%
    mutate(significant = ifelse(X2.50. < 0 & X97.50. > 0, FALSE, TRUE),
           color = ifelse(significant == FALSE, "lightblue", "blue"),
           pch = ifelse(significant == FALSE, 1, 19)) %>%
    arrange(mean) %>%
    dplyr::select(-q_rt_standard)
  
  forest_neg <- read.csv(paste0(load_from_change, "forest_negative_fit_summaries.csv")) %>%
    filter(rownames %in% c("b_landcover_change")) %>%
    left_join(species_list, by = "common_name") %>% #add the forest sorted sp id
    ungroup() %>%
    mutate(significant = ifelse(X2.50. < 0 & X97.50. > 0, FALSE, TRUE),
           color = ifelse(significant == FALSE, "pink", "red"),
           pch = ifelse(significant == FALSE, 1, 19)) %>%
    arrange(mean) %>%
    dplyr::select(-q_rt_standard)
  
  
  plot_intervals(forest_all, "Effect of Change in Forest on Change in Count", first_overlay = forest_pos, second_overlay = forest_neg)

  plot_intervals(dev, "dev species run together")
  #now plots with enough width and such, just still needs to be color changed. Altho being thick enough the light blue might be fine
  #just need to save from my other computer screne

  
par(mar = c(4, 17, 1, 1), cex.axis = 1)  
plot_intervals <- function(plot_df, xlab, first_overlay = NA, second_overlay = NA, xlim_select = c(-0.4, 0.2), ...) {
  plot(y = plot_df$sp_id,
       x = plot_df$mean,
       xlim = xlim_select,
       col = plot_df$color,
      # ylim = c(1,66),
       yaxt = "n",
       xlab = xlab,
       ylab = "",
       pch = 16, 
      cex = 2
       ) +
    segments(x0 = plot_df$X2.5.,
             x1 = plot_df$X97.5.,
             y0 = plot_df$sp_id,
             col = plot_df$color,
             lwd = 5) +
    abline(v = 0, lty = "dashed") + 
    axis(2, at = seq(round(min(plot_df$sp_id)),
                     round(max(plot_df$sp_id)), by = 1),
         labels = plot_df$common_name,
         las = 1,
         cex.axis = 1.25)
  
  if(any(is.na(first_overlay)) == FALSE) {
    segments(x0 = first_overlay$X2.50.,
             x1 = first_overlay$X97.50.,
             y0 = first_overlay$sp_id,
             col = first_overlay$color)
    points(x = first_overlay$mean,
           y = first_overlay$sp_id,
           col = first_overlay$color)
  }
  
  if(any(is.na(first_overlay)) == FALSE) {
    segments(x0 = second_overlay$X2.50.,
             x1 = second_overlay$X97.50.,
             y0 = second_overlay$sp_id,
             col = second_overlay$color)
    points(x = second_overlay$mean,
           y = second_overlay$sp_id,
           col = second_overlay$color)
  }
}

#okay, and for the presentation in Baltimore I also want to plot species trends overall, so I can do a brief dicussion of those as well. For that we want...
traits <- read.csv("data/species-traits/2.landcover_cpc_analysis_full_traits.csv")
sig_only_dev <- dev %>%
  left_join(traits, by = "common_name") %>%
  filter(significant == TRUE) %>%
  mutate(sp_id = row_number(), 
         color = ifelse(mean > 0, "forestgreen", "darkred"),
         X2.50. = X2.5.,
         X97.50. = X97.5.)
sig_only_dev$scalecolor <- viridisLite::viridis(option = "viridis", n = length(plot_df$scale_UAI))[as.numeric(cut(plot_df$scale_UAI, breaks = length(plot_df$scale_UAI)))]

plot_df <- sig_only_dev

plot_only_sig(sig_only_dev) 
        
  sig_only_fpos <- forest_pos %>%
    filter(significant == TRUE) %>%
    arrange(mean) %>%
    mutate(sp_id = row_number(), 
           color = ifelse(mean > 0, "forestgreen", "darkred"))
  sig_only_fneg <- forest_neg %>%
    filter(significant == TRUE) %>%
    arrange(mean) %>%
    mutate(sp_id = row_number(), 
           color = ifelse(mean > 0, "darkred", "forestgreen"))
  
  plot_only_sig(sig_only_fpos, xlim_sig = c(-.4, .25)) + 
    title("Forest Gain")
  plot_only_sig(sig_only_fneg, xlim_sig = c(-.15, .3)) + 
    title("Forest Loss")
        
#do a scatterplot of the positive vs negative forest model results

#scatterplot of forest vs dev model results
dev <- dev %>%
  left_join(species_list, by = "common_name") %>%
  rename(f_sp_id = sp_id.y) %>%
  arrange(f_sp_id) #have to arrange in same order as the forest ones, so they match when plotting

plot(x = dev$mean,
     y = forest_neg$mean,
     xlab = "Effect of Change in Development on Change in Count", 
     ylab = "Effect of Negative Change in Forest on Change in Count",
     pch = 16,
     xlim = c(-0.6, 0.2),
     ylim = c(-0.15, 0.2)) +
  abline(h = 0,
         lty = "dashed") +
  abline(v = 0, 
         lty = "dashed")
  segments(x0 = dev$X2.50.,
           x1 = dev$X97.50.,
           y0 = forest_all$mean,
           col = "grey10") + 
  segments(y0 = forest_all$X2.50.,
           y1 = forest_all$X97.50.,
           x0 = dev$mean,
           col = "seagreen") 


#######################################################################
  # change per change species traits
  # three panel with dev, forest pos, forest neg panels and intervals in same order
######################################################################
  
  cpct_dev <- read.csv(paste0(load_from_cpc_traits, "dev+barrenposterior_samples1-400.csv")) %>%
    dplyr::select(-row_id, -bootstrap_run)
  #hm, maybe I need to do this with bayesplot, since that calculates the confidence intervals for me? lets see..
  mcmc_intervals(cpct_dev,
                 prob = 0.01,
                 prob_outer = 0.95) + 
    geom_vline(xintercept = 0, color = "grey30") +
    ggtitle("cpc dev traits")
  #if I change to mcmc_areas, data is all very normally distributed

  cpct_fpos <- read.csv(paste0(load_from_cpc_traits, "forest_positiveposterior_samples1-400.csv"))  %>%
    dplyr::select(-row_id, -bootstrap_run)

  mcmc_intervals(cpct_fpos,
                 prob = 0.01,
                 prob_outer = 0.95) + 
    geom_vline(xintercept = 0, color = "grey30") +
    ggtitle("cpc forest gain traits")
  
  cpct_fneg <- read.csv(paste0(load_from_cpc_traits, "forest_negativeposterior_samples1-400.csv"))  %>%
    dplyr::select(-row_id, -bootstrap_run)
  
  mcmc_intervals(cpct_fneg,
                 prob = 0.01,
                 prob_outer = 0.95) + 
    geom_vline(xintercept = 0, color = "grey30") +
    ggtitle("cpc forest loss traits")

  #ha.. traits not significant. rolls around  
  #I guess here yeah I'm bagging all the data together. How many times between the bootstrap runs do these come out significant?
  
###############################
#
# Change per change traits with all the species run together
#
#####################################
  cpct_dev_tog <- read.csv(paste0(load_from_cpc_traits_together, "dev+barrenposterior_samples1k.csv")) %>%
    dplyr::select(-row_id, -bootstrap_run)
  #hm, maybe I need to do this with bayesplot, since that calculates the confidence intervals for me? lets see..
  dtplot <- mcmc_intervals(cpct_dev_tog,
                 prob_outer = 0.95) + 
    geom_vline(xintercept = 0, color = "grey30", lty = "dashed") #+
    #ggtitle("Species Traits Predicting Effect of Development")
  dtplot
  #if I change to mcmc_areas, data is all very normally distributed
  
  cpct_fpos_tog <- read.csv(paste0(load_from_cpc_traits_together, "forest_positiveposterior_samples1k.csv"))  %>%
    dplyr::select(-row_id, -bootstrap_run)
  
  fptplot <- mcmc_intervals(cpct_fpos_tog,
                 prob = 0.01,
                 prob_outer = 0.95) + 
    geom_vline(xintercept = 0, color = "grey30") +
    ggtitle("cpc forest gain traits species run together")
  
  cpct_fneg_tog <- read.csv(paste0(load_from_cpc_traits_together, "forest_negativeposterior_samples1k.csv"))  %>%
    dplyr::select(-row_id, -bootstrap_run)
  
  fpnplot <-  mcmc_intervals(cpct_fneg_tog,
                 prob = 0.01,
                 prob_outer = 0.95) + 
    geom_vline(xintercept = 0, color = "grey30") +
    ggtitle("cpc forest loss traits species run together")
  
  plot_grid(dtplot, fptplot, fpnplot, ncol = 3)
  
  #ha.. traits not significant. rolls around  
  #I guess here yeah I'm bagging all the data together. How many times between the bootstrap runs do these come out significant?
  