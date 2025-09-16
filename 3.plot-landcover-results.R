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
#load_from_bdev <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.04.11_traits_on_bdev_add_logsize/"
#load_from_uai <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.04.15_uai_on_bdev/"
load_from_change <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.06.26_cpc_rmbaseline_obsqual/"
load_from_change_together <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.09.09_cpc_allspin1_rm0to0_halfnormalsig_sp/"
load_from_cpc_traits <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.07.14_traits_on_cpc/"
load_from_cpc_traits_together <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.07.29_traits_on_cpc/"
load_from_cpc_grassland <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.09.15_cpc_rm0to0_grassland/"

lf_ch1m1 <- "Z:/Goulden/mbbs-analysis/model/2025.08.29_ch1m1_longleaf_model_test_1/"
lf_ch1m2 <- "Z:/Goulden/mbbs-analysis/model/2025.09.02_ch1m2_longleaf_model2_test_4/"

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


#################### change per change without bayesplot

  species_list <- read.csv(paste0(load_from_change_together,"species_list.csv")) %>%
    left_join(read.csv("data/species-traits/2.landcover_cpc_analysis_full_traits.csv")) %>%
    select(-sp_id)
  
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
    mutate(significant = ifelse(conf_2.5 < 0 & conf_97.5 > 0, FALSE, TRUE),
           color = ifelse(significant == FALSE, "lightblue", "blue"),
           pch = ifelse(significant == FALSE, 1, 19)) %>%
    left_join(species_list, by = "common_name")
#    #let's do color by uai.
#    mutate(color = grDevices::gray.colors(66, start = min(UAI), end= max(UAI)))
    
   # dev$color <- viridisLite::viridis(option = "rocket", n = length(dev$scale_UAI))[as.numeric(cut(dev$scale_UAI, breaks = length(dev$scale_UAI)))]
  
  forest_pos <- read.csv(paste0(load_from_change_together, "forest_positive_fit_summaries.csv")) %>%
    filter(!is.na(slope)) %>%
    left_join(species_list, by = "common_name") %>% #add the forest sorted sp id
    ungroup() %>%
    mutate(significant = ifelse(conf_2.5 < 0 & conf_97.5 > 0, FALSE, TRUE),
           color = ifelse(significant == FALSE, "lightblue", "blue"),
           pch = ifelse(significant == FALSE, 1, 19)) %>%
    arrange(desc(mean)) %>%
    mutate(sp_id = cur_group_rows()) %>%
    dplyr::select(-q_rt_standard)
  #forest_pos$color <- viridisLite::viridis(option = "viridis", n = length(forest_pos$scale_eaforest))[as.numeric(cut(forest_pos$scale_eaforest, breaks = length(forest_pos$scale_eaforest)))]
  
  forest_neg <- read.csv(paste0(load_from_change_together, "forest_negative_fit_summaries.csv")) %>%
    filter(!is.na(slope)) %>%
    left_join(species_list, by = "common_name") %>% #add the forest sorted sp id
    ungroup() %>%
    mutate(significant = ifelse(conf_2.5 < 0 & conf_97.5 > 0, FALSE, TRUE),
           color = ifelse(significant == FALSE, "pink", "red"),
           pch = ifelse(significant == FALSE, 1, 19)) %>%
    arrange(desc(mean)) %>%
    mutate(sp_id = cur_group_rows()) 
  #forest_neg$color <- viridisLite::viridis(option = "mako", n = length(forest_neg$scale_eagrassland))[as.numeric(cut(forest_neg$scale_eagrassland, breaks = length(forest_neg$scale_eagrassland)))]
  
  grassland_neg <- read.csv(paste0(load_from_cpc_grassland, "grassland_negative_fit_summaries.csv")) %>%
    filter(!is.na(slope)) %>%
    left_join(species_list, by = "common_name") %>%
    ungroup() %>%
    mutate(significant = ifelse(conf_2.5 < 0 & conf_97.5 > 0, FALSE, TRUE),
           color = ifelse(significant == FALSE, "orange", "yellow1"),
           pch = ifelse(significant == FALSE, 1, 19)) %>%
    arrange(desc(mean)) %>%
    mutate(sp_id = cur_group_rows())
  
  grassland_pos <- read.csv(paste0(load_from_cpc_grassland, "grassland_positive_fit_summaries.csv")) %>%
    filter(!is.na(slope)) %>%
    left_join(species_list, by = "common_name") %>%
    ungroup() %>%
    mutate(significant = ifelse(conf_2.5 < 0 & conf_97.5 > 0, FALSE, TRUE),
           color = ifelse(significant == FALSE, "lightslateblue", "blueviolet"),
           pch = ifelse(significant == FALSE, 1, 19)) %>%
    arrange(desc(mean)) %>%
    mutate(sp_id = cur_group_rows())
  
  
  #plot_intervals(forest_all, "Effect of Change in Forest on Change in Count", first_overlay = forest_pos, second_overlay = forest_neg)

  png(filename = "figures/ch2_dev_rm0to0_halfnormal.png", 
      width = 1200,
      height = 600,
      units = "px", 
      type = "windows")
  par(mar = c(4, 17, 1, 1), cex.axis = 1, mfrow = c(1,2))
  plot_intervals(plot_df = dev[34:66,],
                 xlab = "Change in species count with change in % urban", 
                 ylim_select = c(33.5,66.5))
  plot_intervals(plot_df = dev[1:33,], 
                 xlab = "", 
                 ylim_select = c(.5,33.5))
  #now plots with enough width and such, just still needs to be color changed. Altho being thick enough the light blue might be fine
  #just need to save from my other computer screen
  #well, really need to update this file to save results nicely to pdfs
  dev.off()
  
  
  png(filename = "figures/ch2_fpos_rm0to0_halfnormal.png", 
      width = 1200,
      height = 600,
      units = "px", 
      type = "windows")
  par(mar = c(4, 17, 1, 1), cex.axis = 1, mfrow = c(1,2))
  plot_intervals(plot_df = forest_pos[34:66,],
                 xlab = "Change in species count with change in forest gain", 
                 ylim_select = c(33.5,66.5),
                 xlim_select = c(-.3, .15))
  plot_intervals(plot_df = forest_pos[1:33,], 
                 xlab = "", 
                 ylim_select = c(.5,33.5),
                 xlim_select = c(-.3, .15))
  #now plots with enough width and such, just still needs to be color changed. Altho being thick enough the light blue might be fine
  #just need to save from my other computer screen
  #well, really need to update this file to save results nicely to pdfs
  dev.off()
  
  
  png(filename = "figures/ch2_fneg_rm0to0_halfnormal.png", 
      width = 1200,
      height = 600,
      units = "px", 
      type = "windows")
  par(mar = c(4, 17, 1, 1), cex.axis = 1, mfrow = c(1,2))
  plot_intervals(plot_df = forest_neg[34:66,],
                 xlab = "Change in species count with change in forest loss", 
                 ylim_select = c(33.5,66.5),
                 xlim_select = c(-.1, .1))
  plot_intervals(plot_df = forest_neg[1:33,], 
                 xlab = "", 
                 ylim_select = c(.5,33.5),
                 xlim_select = c(-.1, .1))
  #now plots with enough width and such, just still needs to be color changed. Altho being thick enough the light blue might be fine
  #just need to save from my other computer screen
  #well, really need to update this file to save results nicely to pdfs
  dev.off()
  
  png(filename = "figures/ch2_grassland_neg_rm0to0.png", 
      width = 1200,
      height = 600,
      units = "px", 
      type = "windows")
  par(mar = c(4, 17, 1, 1), cex.axis = 1, mfrow = c(1,2))
  plot_intervals(plot_df = grassland_neg[34:66,],
                 xlab = "Change in species count per grassland loss", 
                 ylim_select = c(33.5,66.5),
                 xlim_select = c(-0.25, 0.25))
  plot_intervals(plot_df = grassland_neg[1:33,], 
                 xlab = "", 
                 ylim_select = c(.5,33.5),
                 xlim_select = c(-0.25, 0.25))
  dev.off()
  #rm0to0 has 16870 negative changes and 2054 0 changes
  
  
  png(filename = "figures/ch2_grassland_pos_rm0to0.png", 
      width = 1200,
      height = 600,
      units = "px", 
      type = "windows")
  par(mar = c(4, 17, 1, 1), cex.axis = 1, mfrow = c(1,2))
  plot_intervals(plot_df = grassland_pos[34:66,],
                 xlab = "Change in species count per grassland gain", 
                 ylim_select = c(33.5,66.5),
                 xlim_select = c(-0.25, 0.25))
  plot_intervals(plot_df = grassland_pos[1:33,], 
                 xlab = "", 
                 ylim_select = c(.5,33.5),
                 xlim_select = c(-0.25, 0.25))
  dev.off()
  #so, for the grassland gain, how many observations do we have exactly going into that model?
  #it's 9634 observations with change > 0
  # and 2054 with change at 0
  #yeah okay, birds just aren't really responding to positive grassland gain IG - I mean most of the changes are REALLLYY clustered around 0, there is like a couple 13% changes but they are not the majority. shrug! 
  
par(mar = c(4, 17, 1, 1), cex.axis = 1)  

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
  
  
#####################################
  #ch1
#####################################
  c1m1 <- read.csv(paste0(lf_ch1m1, "fit_summary.csv")) %>%
    filter(str_detect(.$rownames, "kappa")) %>%
    mutate(model = "m1", 
           color = "salmon")
  c2m2 <- read.csv(paste0(lf_ch1m2, "fit_summary_m2_5k.csv")) %>%
    filter(!str_detect(.$rownames, "eta")) %>%
    filter(str_detect(.$rownames, "kappa")) %>%
    mutate(model = "m2", 
           color = "turquoise") %>%
    bind_rows(c1m1) %>%
    mutate(id = row_number())
  
  par(mar = c(4, 12, 1, 1), cex.axis = 1)  
  plot(x = c2m2$mean, 
       y = c2m2$id,
       xlim = c(-.4, .4),
       ylab = "",
       yaxt = "n", 
       col = c2m2$color,
       pch = 16) +
    abline(v = 0, lty = "dashed")
    segments(x0 = c2m2$conf_2.5,
             x1 = c2m2$conf_97.5,
             y0 = c2m2$id,
             col = c2m2$color) +
      axis(2, at = seq(round(min(c2m2$id)),
                       round(max(c2m2$id)), by = 1),
           labels = paste(c2m2$rownames, c2m2$model),
           las = 1)

  
############DEPRECATED ANALYSES###################################
  #DEPRECATED############################### uai on bdev
  uai <- read.csv(paste0(load_from_uai, "posterior_samples.csv")) %>%
    dplyr::rename("UAI" = b_uai)
  
  mcmc_intervals(uai,
                 pars = "UAI") +
    xlab("Effect of Development") + 
    xlim(-0.5, 2) +
    vline_0(lty = "dashed")
  
  