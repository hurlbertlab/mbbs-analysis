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
library(reshape2) #for the correlation matrix heatmap
library(vioplot) #for violin plots

#load functions
source("3.plot-functions.R")

#where are we pulling data from?
load_from <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.03.27_loopdevelopment/"
#load_from_bdev <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.04.11_traits_on_bdev_add_logsize/"
#load_from_uai <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.04.15_uai_on_bdev/"
load_from_change <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.06.26_cpc_rmbaseline_obsqual/"
load_from_change_together <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.09.09_cpc_allspin1_rm0to0_halfnormalsig_sp/"
load_from_cpc_traits <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.07.14_traits_on_cpc/"
load_from_cpc_traits_together <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.10.16_traits_on_cpc/"
load_from_cpc_grassland <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.09.15_cpc_rm0to0_grassland/"

lf_ch1m1 <- "Z:/Goulden/mbbs-analysis/model/2025.11.25_ch1_m1_bayesmeeting/"
lf_dieto <- "Z:/Goulden/mbbs-analysis/model/2025.11.25_ch1_m1_dietonly/"
lf_tempo <- "Z:/Goulden/mbbs-analysis/model/2025.11.25_ch1_m1_temponly/"
lf_ch1m2 <- "Z:/Goulden/mbbs-analysis/model/2025.09.08_ch1_m2_kpriors1_FINAL/"

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
  
  z <- qnorm((1+0.87)/2) #confidence interval 87%
  
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
    mutate(conf_6.5 = (mean - (sd * z)),
           conf_93.5 = (mean + (sd * z)),
           significant = ifelse(conf_2.5 < 0 & conf_97.5 > 0, FALSE, TRUE),
           sig_87 = ifelse(conf_6.5 < 0 & conf_93.5 > 0, FALSE, TRUE),
           color = case_when(significant == FALSE & sig_87 == FALSE ~ "grey60",
                             significant == FALSE & sig_87 == TRUE ~ "grey30",
                             significant == TRUE ~ "black"),
           pch = ifelse(significant == FALSE, 1, 19)) %>%
    left_join(species_list, by = "common_name")
#    #let's do color by uai.
#    mutate(color = grDevices::gray.colors(66, start = min(UAI), end= max(UAI)))
    
   # dev$color <- viridisLite::viridis(option = "rocket", n = length(dev$scale_UAI))[as.numeric(cut(dev$scale_UAI, breaks = length(dev$scale_UAI)))]
  
  forest_pos <- read.csv(paste0(load_from_change_together, "forest_positive_fit_summaries.csv")) %>%
    filter(!is.na(slope)) %>%
    left_join(species_list, by = "common_name") %>% #add the forest sorted sp id
    ungroup() %>%
    mutate(conf_6.5 = (mean - (sd * z)),
           conf_93.5 = (mean + (sd * z)),
           significant = ifelse(conf_2.5 < 0 & conf_97.5 > 0, FALSE, TRUE),
           sig_87 = ifelse(conf_6.5 < 0 & conf_93.5 > 0, FALSE, TRUE),
           color = case_when(significant == FALSE & sig_87 == FALSE ~ "lightgreen",
                             significant == FALSE & sig_87 == TRUE ~ "springgreen",
                             significant == TRUE ~ "forestgreen"),
           pch = ifelse(significant == FALSE, 1, 19)) %>%
    arrange(desc(mean)) %>%
    mutate(sp_id = cur_group_rows()) %>%
    dplyr::select(-q_rt_standard)
  #forest_pos$color <- viridisLite::viridis(option = "viridis", n = length(forest_pos$scale_eaforest))[as.numeric(cut(forest_pos$scale_eaforest, breaks = length(forest_pos$scale_eaforest)))]
  
  forest_neg <- read.csv(paste0(load_from_change_together, "forest_negative_fit_summaries.csv")) %>%
    filter(!is.na(slope)) %>%
    left_join(species_list, by = "common_name") %>% #add the forest sorted sp id
    ungroup() %>%
    mutate(conf_6.5 = (mean - (sd * z)),
           conf_93.5 = (mean + (sd * z)),
           significant = ifelse(conf_2.5 < 0 & conf_97.5 > 0, FALSE, TRUE),
           sig_87 = ifelse(conf_6.5 < 0 & conf_93.5 > 0, FALSE, TRUE),
           color = case_when(significant == FALSE & sig_87 == FALSE ~ "lightblue",
                             significant == FALSE & sig_87 == TRUE ~ "steelblue1",
                             significant == TRUE ~ "blue"),
           pch = ifelse(significant == FALSE, 1, 19)) %>%
    arrange(desc(mean)) %>%
    mutate(sp_id = cur_group_rows()) 
  #forest_neg$color <- viridisLite::viridis(option = "mako", n = length(forest_neg$scale_eagrassland))[as.numeric(cut(forest_neg$scale_eagrassland, breaks = length(forest_neg$scale_eagrassland)))]
  
  grassland_neg <- read.csv(paste0(load_from_cpc_grassland, "grassland_negative_fit_summaries.csv")) %>%
    filter(!is.na(slope)) %>%
    left_join(species_list, by = "common_name") %>%
    ungroup() %>%
    mutate(conf_6.5 = (mean - (sd * z)),
           conf_93.5 = (mean + (sd * z)),
           significant = ifelse(conf_2.5 < 0 & conf_97.5 > 0, FALSE, TRUE),
           sig_87 = ifelse(conf_6.5 < 0 & conf_93.5 > 0, FALSE, TRUE),
           color = case_when(significant == FALSE & sig_87 == FALSE ~ "yellow2",
                             significant == FALSE & sig_87 == TRUE ~ "goldenrod1",
                             significant == TRUE ~ "orange"),
           pch = ifelse(significant == FALSE, 1, 19)) %>%
    arrange(desc(mean)) %>%
    mutate(sp_id = cur_group_rows())
  
  grassland_pos <- read.csv(paste0(load_from_cpc_grassland, "grassland_positive_fit_summaries.csv")) %>%
    filter(!is.na(slope)) %>%
    left_join(species_list, by = "common_name") %>%
    ungroup() %>%
    mutate(conf_6.5 = (mean - (sd * z)),
           conf_93.5 = (mean + (sd * z)),
           significant = ifelse(conf_2.5 < 0 & conf_97.5 > 0, FALSE, TRUE),
           sig_87 = ifelse(conf_6.5 < 0 & conf_93.5 > 0, FALSE, TRUE),
           color = case_when(significant == FALSE & sig_87 == FALSE ~ "lightslateblue",
                             significant == FALSE & sig_87 == TRUE ~ "mediumpurple1",
                             significant == TRUE ~ "blueviolet"),
           pch = ifelse(significant == FALSE, 1, 19)) %>%
    arrange(desc(mean)) %>%
    mutate(sp_id = cur_group_rows())
  
  
  #plot_intervals(forest_all, "Effect of Change in Forest on Change in Count", first_overlay = forest_pos, second_overlay = forest_neg)

  png(filename = "figures/ch2_dev_0rts_halfnormal.png", 
      width = 1200,
      height = 600,
      units = "px", 
      type = "windows")
  par(mar = c(4, 16, 1, 1), cex.axis = 1, mfrow = c(1,2))
  plot_intervals(plot_df = dev[34:66,],
                 xlab = "Change in species count with change in % urban", 
                 ylim_select = c(33.5,66.5),
                 xlim_select = c(-0.5, 0.4))
  plot_intervals(plot_df = dev[1:33,], 
                 xlab = "", 
                 ylim_select = c(.5,33.5),
                 xlim_select = c(-0.5, 0.4))
  #now plots with enough width and such, just still needs to be color changed. Altho being thick enough the light blue might be fine
  #just need to save from my other computer screen
  #well, really need to update this file to save results nicely to pdfs
  dev.off()
  
  
  png(filename = "figures/ch2_fpos_0rts_halfnormal.png", 
      width = 1200,
      height = 600,
      units = "px", 
      type = "windows")
  par(mar = c(4, 16, 1, 1), cex.axis = 1, mfrow = c(1,2))
  plot_intervals(plot_df = forest_pos[34:66,],
                 xlab = "Change in species count with change in forest gain", 
                 ylim_select = c(33.5,66.5),
                 xlim_select = c(-.35, .25))
  plot_intervals(plot_df = forest_pos[1:33,], 
                 xlab = "", 
                 ylim_select = c(.5,33.5),
                 xlim_select = c(-.35, .25))
  #now plots with enough width and such, just still needs to be color changed. Altho being thick enough the light blue might be fine
  #just need to save from my other computer screen
  #well, really need to update this file to save results nicely to pdfs
  dev.off()
  
  
  png(filename = "figures/ch2_fneg_0rts_halfnormal.png", 
      width = 1200,
      height = 600,
      units = "px", 
      type = "windows")
  par(mar = c(4, 16, 1, 1), cex.axis = 1, mfrow = c(1,2))
  plot_intervals(plot_df = forest_neg[34:66,],
                 xlab = "Change in species count with change in forest loss", 
                 ylim_select = c(33.5,66.5),
                 xlim_select = c(-.2, .2))
  plot_intervals(plot_df = forest_neg[1:33,], 
                 xlab = "", 
                 ylim_select = c(.5,33.5),
                 xlim_select = c(-.2, .2))
  #now plots with enough width and such, just still needs to be color changed. Altho being thick enough the light blue might be fine
  #just need to save from my other computer screen
  #well, really need to update this file to save results nicely to pdfs
  dev.off()
  
  png(filename = "figures/ch2_grassland_neg_0rts.png", 
      width = 1200,
      height = 600,
      units = "px", 
      type = "windows")
  par(mar = c(4, 16, 1, 1), cex.axis = 1.25, mfrow = c(1,2))
  plot_intervals(plot_df = grassland_neg[34:66,],
                 xlab = "Change in species count per grassland loss", 
                 ylim_select = c(33.5,66.5),
                 xlim_select = c(-0.2, 0.25))
  plot_intervals(plot_df = grassland_neg[1:33,], 
                 xlab = "", 
                 ylim_select = c(.5,33.5),
                 xlim_select = c(-0.2, 0.25))
  dev.off()
  #rm0to0 has 16870 negative changes and 2054 0 changes
  
  
  png(filename = "figures/ch2_grassland_pos_0rts.png", 
      width = 1200,
      height = 600,
      units = "px", 
      type = "windows")
  par(mar = c(4, 16, 1, 1), cex.axis = 1.25, mfrow = c(1,2))
  plot_intervals(plot_df = grassland_pos[34:66,],
                 xlab = "Change in species count per grassland gain", 
                 ylim_select = c(33.5,66.5),
                 xlim_select = c(-0.25, 0.25))
  plot_intervals(plot_df = grassland_pos[1:33,], 
                 xlab = "", 
                 ylim_select = c(.5,33.5),
                 xlim_select = c(-0.25, 0.25))
  legend("topright",
         legend = c("95% CI", "87% CI", "NS"),
         col = c("blueviolet", "mediumpurple1", "lightslateblue"),
         pch = 16)
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
  
  bayesplot_theme_set(theme_minimal())
  bayesplot_theme_update(text = element_text(size = 20, family = "sans"),
                         axis.text.x  = element_text(angle = 45)) 
  
  cpct_dev_tog <- read.csv(paste0(load_from_cpc_traits_together, "dev+barrenposterior_samples1k.csv")) %>%
    dplyr::select(-row_id, -bootstrap_run)
  #hm, maybe I need to do this with bayesplot, since that calculates the confidence intervals for me? lets see..
  color_scheme_set("darkgray")
  dtplot <- mcmc_intervals(cpct_dev_tog,
                 prob_outer = 0.95,
                 prob = 0.87) + 
    geom_vline(xintercept = 0, color = "black", lty = "dashed") +
    scale_y_discrete(
      labels = c("b_uai" = "Urban Association",
                 "b_grassland" = "Grassland Specialization",
                 "b_forest" = "Forest Specialization",
                 "b_tempwq" = "Temp Niche Position")
    ) +
    scale_x_continuous(breaks = c(-0.04, -0.02, 0, 0.02, 0.04),
                       labels = c(-0.04, -0.02, 0, 0.02, 0.04)) +
    labs(title = "Urban")
    #+
    #ggtitle("Species Traits Predicting Effect of Development")
  dtplot
  #if I change to mcmc_areas, data is all very normally distributed
  
  cpct_fpos_tog <- read.csv(paste0(load_from_cpc_traits_together, "forest_positiveposterior_samples1k.csv"))  %>%
    dplyr::select(-row_id, -bootstrap_run)
  
  color_scheme_set("green")
  fptplot <- mcmc_intervals(cpct_fpos_tog,
                 prob = 0.87,
                 prob_outer = 0.95) + 
    geom_vline(xintercept = 0, color = "black", lty = "dashed") +
    #ggtitle("cpc forest gain traits species run together") +
  scale_y_discrete(
    labels = c("b_tempwq" = "TNP",
               "b_forest" = "F",
               "b_grassland" = "G",
               "b_uai" = "UAI")
  )+
    scale_x_continuous(breaks = c(-0.04, -0.02, 0, 0.02, 0.04),
                       labels = c(-0.04, -0.02, 0, 0.02, 0.04))+
    labs(title = "Forest +")
  
  cpct_fneg_tog <- read.csv(paste0(load_from_cpc_traits_together, "forest_negativeposterior_samples1k.csv"))  %>%
    dplyr::select(-row_id, -bootstrap_run)
  
  color_scheme_set("blue")
  fpnplot <-  mcmc_intervals(cpct_fneg_tog,
                 prob = 0.87,
                 prob_outer = 0.95) + 
    geom_vline(xintercept = 0, color = "black", lty = "dashed")+
    scale_y_discrete(
      labels = c("b_tempwq" = "TNP",
                 "b_forest" = "F",
                 "b_grassland" = "G",
                 "b_uai" = "UAI")
    )+
    scale_x_continuous(breaks = c(-0.02, -0.01, 0, 0.01, 0.02),
                       labels = c(-0.02, -0.01, 0, 0.01, 0.02)) +
    labs(title = "Forest -")
    #ggtitle("cpc forest loss traits species run together")
  
  gn <- read.csv(paste0(load_from_cpc_traits_together, "grassland_negativeposterior_samples1k.csv"))  %>%
    dplyr::select(-row_id, -bootstrap_run)
  
  color_scheme_set("orange")
  gnplot <-  mcmc_intervals(gn,
                             prob = 0.87,
                             prob_outer = 0.95) + 
    geom_vline(xintercept = 0, color = "black", lty = "dashed")+
    scale_y_discrete(
      labels = c("b_tempwq" = "TNP",
                 "b_forest" = "F",
                 "b_grassland" = "G",
                 "b_uai" = "UAI")
    ) +
    scale_x_continuous(breaks = c(-0.02, -0.01, 0, 0.01, 0.02),
                       labels = c(-0.02, -0.01, 0, 0.01, 0.02)) +
    labs(title = "Grassland +")
    
  
  gp <- read.csv(paste0(load_from_cpc_traits_together, "grassland_positiveposterior_samples1k.csv"))  %>%
    dplyr::select(-row_id, -bootstrap_run)
  
  color_scheme_set("purple")
  gpplot <-  mcmc_intervals(gp,
                             prob = 0.87,
                             prob_outer = 0.95) + 
    geom_vline(xintercept = 0, color = "black", lty = "dashed")+
    scale_y_discrete(
      labels = c("b_tempwq" = "TNP",
                 "b_forest" = "F",
                 "b_grassland" = "G",
                 "b_uai" = "UAI")
    ) +
    scale_x_continuous(breaks = c(-0.01, 0, 0.01),
                       labels = c(-0.01, 0, 0.01),
                       limits = c(-0.01, 0.01)) +
    labs(title = "Grassland -")
  
  png(filename = "figures/ch2_traits.png", 
      width = 1200,
      height = 400,
      units = "px", 
      type = "windows")
  par(mar = c(4, 16, 1, 1), cex.axis = 1.25)
  plot_grid(dtplot, fptplot, fpnplot, gnplot, gpplot, ncol = 5, rel_widths = c(2, 1, 1, 1, 1))
  dev.off()
  
  #ha.. traits not significant. rolls around  
  #I guess here yeah I'm bagging all the data together. How many times between the bootstrap runs do these come out significant?
  
  
#####################################
  #ch1
#####################################
  
  #the update here is that we should make these graphs seperately - only going to report on the results from the first model in the paper, and then the second model with the interaction term will be supplemental.
  
  c1m1 <- read.csv(paste0(lf_ch1m1, "fit_summary.csv")) %>%
    filter(str_detect(.$rownames, "kappa")) %>%
    mutate(model = "m1", 
           color = "black",
           id = row_number(),
           significant = ifelse(conf_2.5 < 0 & conf_97.5 > 0, FALSE, TRUE),
           pch = ifelse(significant == TRUE, 16, 1))
  c2m2 <- read.csv(paste0(lf_ch1m2, "fit_summary.csv")) %>%
    filter(!str_detect(.$rownames, "eta")) %>%
    filter(str_detect(.$rownames, "kappa")) %>%
    mutate(model = "m2", 
           color = "turquoise") %>%
    bind_rows(c1m1) %>%
    mutate(id = row_number())
  
  ch1dieto <- read.csv(paste0(lf_dieto, "fit_summary.csv")) %>%
    filter(str_detect(.$rownames, "kappa")) %>%
    mutate(id = row_number(),
           significant = ifelse(conf_2.5 < 0 & conf_97.5 > 0, FALSE, TRUE),
           pch = ifelse(significant == TRUE, 16, 1))
  plot(x = ch1dieto$mean,
       y = ch1dieto$id, 
       pch = ch1dieto$pch,
       cex = 2,
       xlim = c(-1,1)) +
    abline(v = 0, lty = "dashed") +
    segments(x0 = ch1dieto$conf_2.5,
             x1 = ch1dieto$conf_97.5,
             y0 = ch1dieto$id,
             lwd = 5)
  
  ch1tempo <- read.csv(paste0(lf_tempo, "fit_summary.csv"))%>%
    filter(str_detect(.$rownames, "kappa")) %>%
    mutate(id = row_number(),
           significant = ifelse(conf_2.5 < 0 & conf_97.5 > 0, FALSE, TRUE),
           pch = ifelse(significant == TRUE, 16, 1))
  plot(x =  ch1tempo$mean,
       y =  ch1tempo$id, 
       pch =  ch1tempo$pch,
       cex = 2,
       xlim = c(-1,1)) +
    abline(v = 0, lty = "dashed") +
    segments(x0 =  ch1tempo$conf_2.5,
             x1 =  ch1tempo$conf_97.5,
             y0 =  ch1tempo$id,
             y1 = ch1tempo$id,
             lwd = 5)
  
  par(mar = c(4, 14, 1, 1), cex.axis = 1)  
  plot(x = c1m1$mean, 
       y = c1m1$id,
       xlim = c(-2, 2),
       ylab = "",
       xlab = "Predictor Effect Size",
       yaxt = "n", 
       col = c1m1$color,
       pch = c1m1$pch,
       lwd = 3,
       cex = 2) +
    abline(v = 0, lty = "dashed")
    segments(x0 = c1m1$conf_2.5,
             x1 = c1m1$conf_97.5,
             y0 = c1m1$id,
             col = c1m1$color,
             lwd = 5) +
      axis(2, at = seq(round(min(c1m1$id)),
                       round(max(c1m1$id)), by = 1),
           labels = c("Regional [Piedmont BCR] Trend",
                      "Habitat Selectivity",
                      "Temperature Niche Position",
                      "Diet:Omnivore",
                      "Diet:Invertivore",
                      "Diet:Granivore"),
           las = 1)
    #add smaller tick marks from 0 - 0.2 for every .01
    axis(side = 1, at = seq(-0.2, 0.2, by = 0.01), 
         labels = FALSE, tck = -0.015)
    axis(side = 1, at = seq(-0.4, 0.4, by = 0.1), 
         labels = FALSE)

    z <- qnorm((1+0.87)/2) #confidence interval 87%
    
    ch1sp <- read.csv(paste0(lf_ch1m1, "fit_summary.csv")) %>%
      filter(str_detect(.$rownames, "b")) %>%
      filter(!str_detect(.$rownames, "kappa|sig|gamma|c_")) %>%
      mutate(common_name_standard = as.integer(str_extract(.$rownames, "[0-9]([0-9])?"))) %>%
      left_join(read.csv(paste0(lf_ch1m1, "species_traits.csv")), by = "common_name_standard") %>%
      left_join(read.csv(paste0(lf_ch1m1, "beta_to_common_name.csv"))) %>%
      mutate(significant = ifelse(conf_2.5 < 0 & conf_97.5 > 0, FALSE, TRUE),
             conf_6.5 = (mean - (sd * z)),
             conf_93.5 = (mean + (sd * z)),
             sig_87 = ifelse(conf_6.5 < 0 & conf_93.5 > 0, FALSE, TRUE),
             mean_gt01 = ifelse(mean < -0.01 | mean > 0.01, TRUE, FALSE),
             significant = case_when(significant == TRUE ~ significant,
                                     sig_87 == TRUE & mean_gt01 == TRUE ~ TRUE,
                                     TRUE ~ FALSE))

    #plot mean against temperature niche
    maxColorValue <- 100
    palette <- colorRampPalette(c("blue","red"))(maxColorValue)
    plot(x, y, col = palette[cut(x, maxColorValue)])
    #saved at 400 x 400 pixels
    plot(x = ch1sp$scale_ztempwq, 
         y = ch1sp$mean,
         ylim = c(-.14, .07),
         col = palette[cut(ch1sp$scale_ztempwq, maxColorValue)],
         pch = 16,
         xlab = "Scaled Temperature Niche",
         ylab = "Population Trend") +
      abline(h = 0, lty = "dashed") +
      segments(x0 = ch1sp$scale_ztempwq,
               y0 = ch1sp$conf_2.5,
               y1 = ch1sp$conf_97.5,
               lwd = 3,
               col = palette[cut(ch1sp$scale_ztempwq, maxColorValue)]) #+
      #legend("bottomleft",
      #       )

    
    #n significantly increasing/decreasing species:
    table(ch1sp$significant, ch1sp$conf_2.5 > 0)
    #FALSE FALSE = not significant, 18 species
    #FALSE TRUE 0 = not possible, case where not significant but 2.5 was over 0
    #TRUE FALSE = 29 species significantly decreasing.
    #TRUE TRUE = 12 species significantly increasing
    #n significantly decreasing species:

    #and these are percent change per year, so maybe it makes sense to multiply the means by 100 and add to xaxis
    
    omniv <- ch1sp %>%
      filter(avonet_diet == "Omnivore") %>%
      arrange(desc(mean)) %>%
      mutate(diet_group_order = cur_group_rows(),
             sp_id = diet_group_order,
             color = "brown") %>%
      ungroup()
    
    graniv <- ch1sp %>%
      filter(avonet_diet == "Granivore") %>%
      arrange(desc(mean)) %>%
      mutate(diet_group_order = cur_group_rows(),
             sp_id = diet_group_order,
             color = "brown1") 
    
    invertiv <- ch1sp %>%
      filter(avonet_diet == "Invertivore") %>%
      arrange(desc(mean)) %>%
      mutate(diet_group_order = cur_group_rows(),
             sp_id = diet_group_order,
             color = "brown3")

      
    #now we can plot side by side the three panels, first invertivore, then omnivore, then granivore to follow size of groups
    #color code by temperature niche position (tho not predictive)
    png(filename = "figures/ch1_pop_change_by_diet.png", 
        width = 1000,
        height = 500,
        units = "px", 
        type = "windows")
    par(mar = c(4, 13.5, 1, 1), 
        cex = 1.3,
        cex.axis = 1.3,
        cex.lab = 1.6,
        cex.main = 1.6,
        cex.sub = 1.4,
        mfrow = c(1,3))
    plot_intervals(plot_df = invertiv,
                   xlab = "", 
                   ylim_select = c(0, 39),
                   xlim_select = c(-.08, .08),
                   title = "Invertivores",
                   xaxt = "n") 
      axis(side = 1, at = seq(-.08, .08, by = 0.02), 
           labels = TRUE)
    #split invertivores at purple martin?
    plot_intervals(plot_df = omniv,
                   xlab = "Population Change Per Year", 
                   ylim_select = c(.5, 14.5),
                   xlim_select = c(-.14, .08),
                   title = "Omnivores",
                   xaxt = "n") 
      axis(side = 1, at = seq(-.14, .08, by = 0.02), 
           labels = TRUE)
    par(mar = c(4, 12, 1, 1))
    plot_intervals(plot_df = graniv,
                   xlab = "", 
                   ylim_select = c(.5, 7.5),
                   xlim_select = c(-.1, .02),
                   title = "Granivores",
                   xaxt = "n") 
      axis(side = 1, at = seq(-.1, .02, by = 0.02), 
           labels = TRUE)
    dev.off()
    #notes:
    #- make sure there are more tickmarks, more helpful
    #- add label for which group
    

############################################
# ch2 landscape change correlation matrix
############################################
    barren <- read.csv("data/nlcd-landcover/nlcd_annual_barren.csv") 
    forest <- read.csv("data/nlcd-landcover/nlcd_annual_sum_forest.csv") %>%
      #summarize bc we only need 1 entry per route quarter
      group_by(route, quarter_route, year, perc_forest_quarter) %>%
      summarize() %>%
      ungroup()
    grassland <- read.csv("data/nlcd-landcover/nlcd_annual_sum_grassland.csv") %>%
      group_by(route, quarter_route, year, perc_grassland_quarter) %>%
      summarize() %>%
      ungroup()
    developed_cover <- read.csv("data/nlcd-landcover/nlcd_annual_running_max_developed.csv") %>%
      left_join(barren, by = c("route", "stop_num", "year")) %>%
      #get percent developed by route-quarter
      mutate(quarter_route = as.integer(case_when(stop_num > 15 ~ 4,
                                                  stop_num > 10 ~ 3,
                                                  stop_num > 5 ~ 2,
                                                  stop_num > 0 ~ 1)),
             perc_barren = ifelse(is.na(perc_barren), 0, perc_barren)) %>%
      group_by(route, quarter_route, year) %>%
      #summarize bc we only need to keep 1 entry per route quarter
      summarize(rmax_dev_quarter = mean(running_max_perc_developed),
                perc_barren = mean(perc_barren)) %>%
      ungroup() %>%
      #create a variable where dev has barren ground added on
      mutate(rmax_dev_plus_barren = rmax_dev_quarter + perc_barren) %>%
      #left join forest and grassland change
      left_join(forest, by = c("route", "quarter_route", "year")) %>%
      left_join(grassland, by = c("route", "quarter_route", "year")) %>%
      #replace NA with 0, there's three routes in durham with qrts affected by this in the grassland. This tracks
      replace(is.na(.), 0) %>%
      #create change variables
      group_by(route, quarter_route) %>%
      arrange(year, .by_group = TRUE) %>%
      mutate(
        change_dev = rmax_dev_plus_barren - lag(rmax_dev_plus_barren),
        change_forest = perc_forest_quarter - lag(perc_forest_quarter),
        change_grassland = perc_grassland_quarter - lag(perc_grassland_quarter),
        forest_gain = ifelse(change_forest >= 0, change_forest, NA),
        forest_loss = ifelse(change_forest <= 0, change_forest, NA),
        grassland_gain = ifelse(change_grassland >= 0, change_grassland, NA),
        grassland_loss = ifelse(change_grassland <= 0, change_grassland, NA)) %>%
      ungroup() %>%
      #remove rows that have no lag (first yr)
      filter(!is.na(change_dev))
      
    
    all_landtypes <- developed_cover %>%
      select(change_dev, forest_gain, forest_loss, grassland_gain, grassland_loss)
    #cor(x = all_landtypes$change_dev, y = all_landtypes$change_forest_gain, use = "complete.obs")
    #cor(x = all_landtpyes$change_dev, y = all_landtypes$change_forest_loss, use = "complete.obs")
    #cor(x = all_landtypes$change_dev, y = all_landtypes$change_grassland_gain, use = "complete.obs")
    #cor(x = all_landtypes$change_dev, y = all_landtypes$change_grassland_loss, use = "complete.obs")
    

    
    cor_df <- round(cor(all_landtypes, use = "pairwise.complete.obs"), 2)
    melted_cor <- melt(cor_df)
  
    ggplot(data = melted_cor,
           aes(x=Var1, y=Var2, fill=value)) +
      geom_tile() +
      geom_text(aes(Var2, Var1, label = value), size = 5) +
      scale_fill_gradient2(low = "blue", high = "red", limit = c(-1,1), name = "Correlation") +
      theme(axis.title.x = element_blank(), 
            axis.title.y = element_blank(),
            panel.background = element_blank())
    
    #one other thing to maybe check is if these correlations are stronger for eg: forest positive + dev vs forest negative + dev
    
    #labelvar1 = c("Change % Impervious", "Change % Forest", "Change % Grassland", "Change % Impervious", "Change % Forest", "Change % Grassland", "Change % Impervious", "Change % Forest", "Change % Grassland")
    #labelvar2 =  c("Change % Impervious", "Change % Impervious", "Change % Impervious", "Change % Forest", "Change % Forest", "Change % Forest", "Change % Grassland", "Change % Grassland", "Change % Grassland")
    
    #labelboth = c("Change % Impervious", "Change % Forest", "Change % Grassland")
    
    #also want a violin plot of the distribution in changes for each landcover
    vioplot(all_landtypes,
            names = c("Change % Impervious", "Change % Forest", "Change % Grassland"),
            col = c("grey40", "lightgreen", "lightslateblue")) #+
    #  abline(x = c(.5, 1.5), y = c(0,0)) #eh, only if I can specify a boundary. also ? this has some negative impervious? ah! yes, from the loss of barren ground from one year to the next. That's fine
    #add percent label to y axis side
    #rename x axis labels
    #add horizontal line at 0?
    