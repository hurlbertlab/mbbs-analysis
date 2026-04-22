###################################
#
#
# Plot figures for chapter 1
#
#
#
###################################
#install.packages('statuser')
library(dplyr)
library(stringr)
library(cowplot) #used to make multi-panel figures
library(reshape2) #for any correlation matrix heatmaps
library(statuser) #from the data collada blog, more useful tables: https://datacolada.org/132
#library(tinytex) #lightweight LaTeX distribution that makes it easy to use in R

#load functions
source("3.plot-functions.R")

#where are we pulling data from?
lf_ch1m1 <- "model/2026.03.12_ch1_m1_final/"
lf_ch1nR <- "model/2026.03.12_ch1_withoutregional_final/"
lf_ch1NOBO <- "model/2026.04.09_ch1_rmNOBO_final/"
lf_ch1NOBO_nR <- "model/2026.04.09_ch1_rmNOBO_woRegional_final/"
stable_color <- "#4393c3"
#lf_dieto <- "Z:/Goulden/mbbs-analysis/model/2025.12.1_ch1_m1_diettest/"
#lf_tempo <- "Z:/Goulden/mbbs-analysis/model/2025.11.25_ch1_m1_temponly/"


#####################################
#
# Plot kappa effect sizes
#
###################################

  c1m1 <- read.csv(paste0(lf_ch1m1, "fit_summary.csv")) %>%
    filter(str_detect(.$rownames, "kappa")) %>%
    mutate(model = "m1", 
           color = "black",
           id = row_number(),
           significant = ifelse(conf_2.5 < 0 & conf_97.5 > 0, FALSE, TRUE),
           pch = ifelse(significant == TRUE, 16, 1),
           ylab = case_when(
             rownames == "kappa_regional" ~ "Regional Trend",
             rownames == "kappa_habitat_selection" ~ "Habitat Selectivity",
             rownames == "kappa_temp_pos" ~ "Temp. Niche Position",
             rownames == "kappa_diet" ~ "Percent Insectivory"
           )
    )
  
  c1mNR <- read.csv(paste0(lf_ch1nR, "fit_summary.csv")) %>%
    filter(str_detect(.$rownames, "kappa")) %>%
    mutate(model = "m1", 
           color = "azure2",
           id = row_number(),
           significant = ifelse(conf_2.5 < 0 & conf_97.5 > 0, FALSE, TRUE),
           pch = ifelse(significant == TRUE, 16, 1))
  
  ch1NOBO <- read.csv(paste0(lf_ch1NOBO, "fit_summary.csv")) |>
    filter(str_detect(rownames, "kappa")) |>
    mutate(model = "msens",
           color = "orange",
           id = row_number(),
           significant = ifelse(conf_2.5 < 0 & conf_97.5 > 0, FALSE, TRUE),
           pch = ifelse(significant == TRUE, 16, 1),
           ylab = case_when(
             rownames == "kappa_regional" ~ "Regional Trend",
             rownames == "kappa_habitat_selection" ~ "Habitat Selectivity",
             rownames == "kappa_temp_pos" ~ "Temp. Niche Position",
             rownames == "kappa_diet" ~ "Percent Insectivory"
           )
           )
  
  ch1NOBO_nR <- read.csv(paste0(lf_ch1NOBO_nR, "fit_summary.csv")) |>
    filter(str_detect(rownames, "kappa")) |>
    mutate(model = "mNRsens",
           color = "pink",
           id = row_number(),
           significant = ifelse(conf_2.5 < 0 & conf_97.5 > 0, FALSE, TRUE),
           pch = ifelse(significant == TRUE, 16, 1))
  
  rt_model_color = "black"
  wo_model_color = "brown2"
  
  
  ###!!!!!!!!!!!!!!!!Make a new plotting function for plotting these effects. Have an option where it makes a new plot or not. rmNOBO models should be the main ones.
  png(filename = "figures/ch1_model_CI_comparison.png", 
      width = 420, # 620 for larger
      height = 440, # 640 for larger
      units = "px", 
      type = "windows")
  #honestly, might want to think about increasing the width and height and text sizes.
  {
    par(mar = c(4, 10, 2, 1),
        cex = 1.1) # 1.5 for larger
    plot(x = ch1NOBO$mean,
         y = ch1NOBO$id, 
         pch = ch1NOBO$pch,
         cex = 2,
         xlim = c(-.02,.04),
         xlab = "Effect Size",
         xaxt = "n",
         yaxt = "n",
         ylab = "",
         col = rt_model_color,
         ylim = c(.5, 4)) 
    abline(v = 0, lty = "dashed") 
    axis(side = 1, at = seq(-0.04, 0.04, by = 0.01), 
         labels = TRUE) 
    segments(x0 = ch1NOBO$conf_2.5,
             x1 = ch1NOBO$conf_97.5,
             y0 = ch1NOBO$id,
             lwd = 4.5,
             col = rt_model_color) 
    axis(side = 2,           # Side 2 is the left side (y-axis)
         at = ch1NOBO$id,     # Specify the locations of the tick marks
         labels =  ch1NOBO$ylab, # Specify the labels for those locations
         las = 2
    ) 
    #add the second model without regional trend
    points(x = ch1NOBO_nR$mean,
           y = ch1NOBO_nR$id+.75,
           pch = ch1NOBO_nR$pch,
           cex = 2,
           col = wo_model_color) +
      segments(x0 = ch1NOBO_nR$conf_2.5,
               x1 = ch1NOBO_nR$conf_97.5,
               y0 = ch1NOBO_nR$id+.75,
               lwd = 4.5,
               col = wo_model_color)
  }
  legend("right",
         bty = "n",
         legend = c("Full Model", "Model without \nRegional Trend\n"), 
         fill = c(rt_model_color, wo_model_color)
         #scientific name is Colinus virginianus, if I want to change text to reflect that.
  )
  #}
  dev.off()
  
  #Supplemental version with all four models
  png(filename = "figures/ch1_model_CI_comparison_SUP.png", 
      width = 620,
      height = 640,
      units = "px", 
      type = "windows")
  #honestly, might want to think about increasing the width and height and text sizes.
  {
  par(mar = c(4, 10, 2, 1),
      cex = 1.5)
  plot(x = c1m1$mean,
       y = c1m1$id, 
       pch = c1m1$pch,
       cex = 2,
       xlim = c(-.02,.04),
       xlab = "Effect Size",
       xaxt = "n",
       yaxt = "n",
       ylab = "",
       col = rt_model_color,
       ylim = c(.5, 4)) 
    abline(v = 0, lty = "dashed") 
    axis(side = 1, at = seq(-0.04, 0.04, by = 0.01), 
         labels = TRUE) 
    segments(x0 = c1m1$conf_2.5,
             x1 = c1m1$conf_97.5,
             y0 = c1m1$id,
             lwd = 4.5,
             col = rt_model_color) 
    axis(side = 2,           # Side 2 is the left side (y-axis)
         at = c1m1$id,     # Specify the locations of the tick marks
         labels =  c1m1$ylab, # Specify the labels for those locations
         las = 2
    ) 
    #add the second model without regional trend
    points(x = c1mNR$mean,
           y = c1mNR$id+.5, 
           pch = c1mNR$pch,
           cex = 2,
           col = wo_model_color) +
    segments(x0 = c1mNR$conf_2.5,
             x1 = c1mNR$conf_97.5,
             y0 = c1mNR$id+.5,
             lwd = 4.5,
             col = wo_model_color)
    #add the third model with the sensitivity analysis
    points(x = ch1NOBO$mean,
           y = ch1NOBO$id-.25,
           pch = ch1NOBO$pch,
           cex = 2,
           col = ch1NOBO$color) +
      segments(x0 = ch1NOBO$conf_2.5,
               x1 = ch1NOBO$conf_97.5,
               y0 = ch1NOBO$id-.25,
               lwd = 4.5,
               col = ch1NOBO$color)
    #add the fourth model with no RT and sensitivity analysis
    points(x = ch1NOBO_nR$mean,
           y = ch1NOBO_nR$id+.3,
           pch = ch1NOBO_nR$pch,
           cex = 2,
           col = ch1NOBO_nR$color) +
      segments(x0 = ch1NOBO_nR$conf_2.5,
               x1 = ch1NOBO_nR$conf_97.5,
               y0 = ch1NOBO_nR$id+.3,
               lwd = 4.5,
               col = ch1NOBO_nR$color)
  }
  legend("right",
         bty = "n",
         legend = c("Full Model,\nWith Bobwhite\n", "Full Model\n", "No Regional Trend, \nWith Bobwhite\n", "No Regional Trend\n"), 
         fill = c(rt_model_color, "orange", wo_model_color, "pink")
         #scientific name is Colinus virginianus, if I want to change text to reflect that.
  )
  #}
  dev.off()
  
######################
#
# Plot species trends, horizontal
# 
######################
  #set z for calculating an 87% credible interval
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
           #significant = case_when(significant == TRUE ~ significant,
          #                         sig_87 == TRUE & mean_gt01 == TRUE ~ TRUE,
          #                         TRUE ~ FALSE),
           trend_direction = case_when(
             conf_2.5 > 0 ~ "positive",
             conf_97.5 < 0 ~ "negative",
             conf_2.5 < 0 & conf_6.5 > 0 & mean_gt01 == TRUE ~ "slightly_positive",
             conf_2.5 < 0 & conf_6.5 > 0 & mean_gt01 == FALSE ~ "stable",
             conf_97.5 > 0 & conf_93.5 < 0 & mean_gt01 == TRUE ~ "slightly_negative",
             conf_97.5 > 0 & conf_93.5 < 0 & mean_gt01 == FALSE ~ "stable",
             conf_6.5 < 0 & conf_93.5 > 0 ~ "stable",
             TRUE ~ "unaccounted"
           ))

  
  
  #n sigifnicantly increasing/decreasing species:
  statuser::table2(ch1sp$trend_direction)
  
  plot_horiz <- ch1sp %>%
    mutate(color = case_when(
      trend_direction == "negative" ~ "#762a83", #decreasing
      #trend_direction == "slightly_negative" ~ "#c2a5cf", #decline supported at 87% confidence interval
      trend_direction == "slightly_negative" ~ stable_color,
      trend_direction == "stable" ~ stable_color, #stable, not significant at 95 or 87%
      trend_direction == "slightly_positive" ~ "#5aae61", #increase supported at 87% confidence interval
      trend_direction == "positive" ~ "#1b7837" #increasing
    )) %>%
    arrange(mean) %>%
    mutate(sp_id = cur_group_rows()) %>%
    ungroup()
  #no species are in the "decline supported at the 87% CI and mean greater than .01 
  
  png(filename = "figures/ch1_horiz_pop_change.png", 
    width = 1200,
    height = 600,
    units = "px", 
    type = "windows")
  {
  par(mar = c(13.5, 5, 2, 2), 
      cex = 1.1,
      cex.axis = 1.3,
      cex.lab = 1.2,
      cex.main = 1.6,
      cex.sub = 1.4)
  plot_intervals(plot_df = plot_horiz,
                 species_axis = "x",
                 ylim_select = c(-.15, .07),
                 xlim_select = c(0, 61),
                 title = "North Carolina Mini Breeding Bird Survey Species Population Trends",
                 yaxt = "n") 
  axis(side = 2, 
       #at = seq(-.14, .08, by = 0.02), 
       #labels = TRUE,
       at = c(-.14, -.12, -.10, -.08, -.06, -.04, -.02, 0, .02, .04, .06, .08),
       labels = c("-14%", "-12%", "-10%", "-8%", "-6%", "-4%", 
                  "-2%", "0", "2%", "4%", "6%", "8%"),
       las = 1) 
  axis(side = 2, at = seq(-.13, 0.05, by = 0.02),
       labels = FALSE,
       tck = -0.01) 
  mtext(text = "Percent Population Change per Year", side = 2, line = 4, cex = 1.3) 
  legend("topleft",
         legend = c("Decreasing [95% CI]", 
                    #"Likely Decreasing [87% CI]", 
                    "Stable", 
                    #"Likely Increasing [87% CI]",
                    "Increasing [95% CI]"),
         fill = c("#762a83",
                  #"#c2a5cf", 
                  stable_color,
                  #"#5aae61",
                  "#1b7837"),
         bty = "n")
  }
  dev.off()
  
  ##try a two-panel version
  png(filename = "figures/ch1_horiz_pop_change_twopanel.png", 
      width = 900,
      height = 800,
      units = "px", 
      type = "windows") 
  {
    par(mar = c(11, 6, 2, 1), 
        cex = 1.1,
        cex.axis = 1.4,
        cex.lab = 1.2,
        cex.main = 1.6,
        cex.sub = 1.4,
        mfrow = c(2,1))
    {
    plot_intervals(plot_df = plot_horiz[1:30,],
                   species_axis = "x",
                   ylim_select = c(-.15, .01),
                   xlim_select = c(0, 30.5),
                   title = "North Carolina Mini Breeding Bird Survey Species Population Trends",
                   yaxt = "n") 
    axis(side = 2, 
         #at = seq(-.14, .08, by = 0.02), 
         #labels = TRUE,
         at = c(-.14, -.12, -.10, -.08, -.06, -.04, -.02, 0, .02, .04, .06, .08),
         labels = c("-14%", "-12%", "-10%", "-8%", "-6%", "-4%", 
                    "-2%", "0", "2%", "4%", "6%", "8%"),
         las = 1) 
    axis(side = 2, at = seq(-.13, 0.05, by = 0.02),
         labels = FALSE,
         tck = -0.01) 
    mtext(text = "Population Change per Year", side = 2, line = 4, cex = 1.3) 
    legend("bottomright",
           legend = c("Declining", 
                      #"Likely Decreasing [87% CI]", 
                      "Stable", 
                      #"Likely Increasing [87% CI]",
                      "Increasing"),
           fill = c("#762a83", 
                    #"#c2a5cf",
                    stable_color,
                    #"#5aae61", 
                    "#1b7837"),
           bty = "n")
    }
    par(mar = c(11, 6, .5, 1))
    {
    plot_intervals(plot_df = plot_horiz[31:60,],
                   species_axis = "x",
                   ylim_select = c(-.08, .08),
                   xlim_select = c(30, 60.5),
                   title = "",
                   yaxt = "n") 
      axis(side = 2, 
           #at = seq(-.14, .08, by = 0.02), 
           #labels = TRUE,
           at = c(-.14, -.12, -.10, -.08, -.06, -.04, -.02, 0, .02, .04, .06, .08),
           labels = c("-14%", "-12%", "-10%", "-8%", "-6%", "-4%", 
                      "-2%", "0", "2%", "4%", "6%", "8%"),
           las = 1) 
      axis(side = 2, at = seq(-.13, 0.05, by = 0.02),
           labels = FALSE,
           tck = -0.01) 
      mtext(text = "Population Change per Year", side = 2, line = 4, cex = 1.3)
      legend("bottomright",
             legend = c("Declining", 
                        #"Likely Decreasing [87% CI]",
                        "Stable",
                        #"Likely Increasing [87% CI]",
                        "Increasing"),
             fill = c("#762a83", 
                      #"#c2a5cf", 
                      stable_color,
                      #"#5aae61", 
                      "#1b7837"),
             bty = "n")
    }
  }
  dev.off()
  
######################
#
# Plot linear effects
# 
#####################
  #set z for calculating an 87% credible interval
  z <- qnorm((1+0.87)/2) #confidence interval 87%
  
  #read in and manipulate data
  ch1lineareffects <- read.csv(paste0(lf_ch1m1, "fit_summary.csv")) %>%
    filter(str_detect(.$rownames, "b|gamma|kappa_")) %>%
    mutate(common_name_standard = as.integer(str_extract(.$rownames, "[0-9]([0-9])?"))) %>%
    left_join(read.csv(paste0(lf_ch1m1, "species_traits.csv")), by = "common_name_standard") %>%
     left_join(read.csv(paste0(lf_ch1m1, "beta_to_common_name.csv"))) |>
    mutate(significant = ifelse(conf_2.5 < 0 & conf_97.5 > 0, FALSE, TRUE),
            conf_6.5 = (mean - (sd * z)),
            conf_93.5 = (mean + (sd * z)),
            sig_87 = ifelse(conf_6.5 < 0 & conf_93.5 > 0, FALSE, TRUE),
            mean_gt01 = ifelse(mean < -0.01 | mean > 0.01, TRUE, FALSE),
            #significant = case_when(significant == TRUE ~ significant,
            #                         sig_87 == TRUE & mean_gt01 == TRUE ~ TRUE,
            #                         TRUE ~ FALSE),
           trend_direction = case_when(
             conf_2.5 > 0 ~ "positive",
             conf_97.5 < 0 ~ "negative",
             conf_2.5 < 0 & conf_6.5 > 0 & mean_gt01 == TRUE ~ "slightly_positive",
             conf_2.5 < 0 & conf_6.5 > 0 & mean_gt01 == FALSE ~ "stable",
             conf_97.5 > 0 & conf_93.5 < 0 & mean_gt01 == TRUE ~ "slightly_negative",
             conf_97.5 > 0 & conf_93.5 < 0 & mean_gt01 == FALSE ~ "stable",
             conf_6.5 < 0 & conf_93.5 > 0 ~ "stable",
             TRUE ~ "unaccounted"
           )) |>
    #THESE MEAN DIFFERENT THINGS THAN BEFORE, COLORS BASED ON REGIONAL TREND
    mutate(rt_sig = ifelse(usgs_2.5CI < 0 & usgs_97.5CI > 0, FALSE, TRUE),
           usgs_6.5 = (usgs_trend_estimate - (usgs_sd * z)),
           usgs_93.5 = (usgs_trend_estimate + (usgs_sd * z)),
           rt_87 = ifelse(usgs_6.5 < 0 & usgs_93.5 > 0, FALSE, TRUE),
           rtmean_gt01 = ifelse(usgs_trend_estimate < -0.01 | usgs_trend_estimate > 0.01, TRUE, FALSE),
           rt_direction = case_when(
             usgs_2.5CI > 0 ~ "positive",
             usgs_97.5CI < 0 ~ "negative",
             usgs_2.5CI < 0 & usgs_6.5 > 0 & rtmean_gt01 == TRUE ~ "slightly_positive",
             usgs_2.5CI < 0 & usgs_6.5 > 0 & rtmean_gt01 == FALSE ~ "stable",
             usgs_97.5CI > 0 & usgs_93.5 < 0 & rtmean_gt01 == TRUE ~ "slightly_negative",
             usgs_97.5CI > 0 & usgs_93.5 < 0 & rtmean_gt01 == FALSE ~ "stable",
             usgs_6.5 < 0 & usgs_93.5 > 0 ~ "stable",
             is.na(common_name_standard) ~ NA,
             TRUE ~ "unaccounted"
           )) |>
    mutate(trend_agreement = case_when(
      #cases when both significant and agree or disagree
      trend_direction %in% c("positive", "slightly_positive") & rt_direction %in% c("positive", "slightly_positive") ~ "agree",
      trend_direction %in% c("negative", "slightly_negative") & rt_direction %in% c("negative", "slightly_negative") ~ "agree",
      trend_direction %in% c("positive", "slightly_positive") & rt_direction %in% c("negative", "slightly_negative") ~ "disagree",
      trend_direction %in% c("negative", "slightly_negative") & rt_direction %in% c("positive", "slightly_positive") ~ "disagree",
      trend_direction %in% c("negative", "slightly_negative", "positive", "slightly_positive") & rt_direction == "stable" ~ "disagree, regional NS",
      trend_direction == "stable" & rt_direction == "stable" ~ "agree",
      trend_direction == "stable" & rt_direction != "stable" ~ "disagree, local NS"
    )
    ) |>
    mutate(rt_colors = case_when(
      rt_direction == "negative" ~ "#762a83", #decreasing
      rt_direction == "slightly_negative" ~ "#c2a5cf", #decline supported at 87% confidence interval
      rt_direction == "stable" ~ stable_color, #stable
      rt_direction == "slightly_positive" ~ "#5aae61", #increase supported at 87% confidence interval
      rt_direction == "positive" ~ "#1b7837" #increasing
    ),
    ta_colors = case_when(
      trend_agreement == "agree" ~ "gold",
      trend_agreement == "disagree" ~ "red",
      TRUE ~ "blue" #error color
    )
    )
  
  #sensitivity analysis
  ch1NOBOlinear <- read.csv(paste0(lf_ch1NOBO, "fit_summary.csv")) %>%
    filter(str_detect(.$rownames, "b|gamma|kappa_")) %>%
    mutate(common_name_standard = as.integer(str_extract(.$rownames, "[0-9]([0-9])?"))) %>%
    left_join(read.csv(paste0(lf_ch1NOBO, "species_traits.csv")), by = "common_name_standard") %>%
    left_join(read.csv(paste0(lf_ch1NOBO, "beta_to_common_name.csv"))) |>
    mutate(significant = ifelse(conf_2.5 < 0 & conf_97.5 > 0, FALSE, TRUE),
           conf_6.5 = (mean - (sd * z)),
           conf_93.5 = (mean + (sd * z)),
           sig_87 = ifelse(conf_6.5 < 0 & conf_93.5 > 0, FALSE, TRUE),
           mean_gt01 = ifelse(mean < -0.01 | mean > 0.01, TRUE, FALSE))
  
  #ch1noregionaleffects <- read.csv(paste0(lf_ch1nR, "fit_summary.csv")) %>%
  #  filter(str_detect(.$rownames, "b|gamma|kappa_")) %>%
  #  mutate(common_name_standard = as.integer(str_extract(.$rownames, "[0-9]([0-9])?"))) %>%
  #  left_join(read.csv(paste0(lf_ch1m1, "species_traits.csv")), by = "common_name_standard") %>%
  #  left_join(read.csv(paste0(lf_ch1m1, "beta_to_common_name.csv")))
  
  
  #difference in the beta calculations between these two is minuscule. Up above showing the betas for either is fine.
  #min(ch1lineareffects$mean[1:60] - ch1noregionaleffects$mean[1:60])
  #max(ch1lineareffects$mean[1:60] - ch1noregionaleffects$mean[1:60])
  
  maxColorValue = 100
  
  
  #figure for main publication
  png(filename = "figures/ch1_linear_effects.png", 
      width = 800,
      height = 800,
      units = "px", 
      type = "windows")
  {
    par(mar =c(5.1, 6.1, 4.1, 2.1), 
        cex = 1.3,
        cex.axis = 1.3,
        cex.lab = 1.6,
        cex.main = 1.6,
        cex.sub = 1.4,
        mfrow = c(2,2))
    
    #Temperature Niche
    plot_linear_effects(load_from = lf_ch1m1,
                        variable_of_interest = "scale_ztempwq",
                        variable_kappa = "kappa_temp_pos",
                        xlab = "Scaled Temperature Niche Position",
                        trendline_lty = "dashed",
                        plot_slope = FALSE)
    legend("bottomleft",
           legend = c("NC Colder"),
           fill = c("blue"),
           bty = "n")
    legend("bottomright",
           legend = c("NC Warmer"),
           fill = c("red"),
           bty = "n")
    #and now if I want to add the sensitivity analysis on top.... I want to be able to plot JUST the line without starting a new plot.
    #and I want to re-print the segments and cex with issues.
    plot_linear_effects(load_from = lf_ch1NOBO,
                        fit_summary = ch1lineareffects, #still plots everything but the line off ch1lineareffects
                        variable_of_interest = "scale_ztempwq",
                        variable_kappa = "kappa_temp_pos",
                        xlab = "Scaled Temperature Niche Position",
                        trendline_lty = "solid",
                        new_plot = FALSE#,
                        #polygon_color = "orange"
                        )
    add_outlier(df = ch1lineareffects,
                variable_of_interest = "scale_ztempwq",
                add_legend = TRUE)
   
    #SO when I do the final plot now, I'll JUST plot the orange bar from the sensitivity analysis.

    #Percent Insectivory
    plot_linear_effects(load_from = lf_ch1m1,
                        variable_of_interest = "scale_insect_perc",
                        variable_kappa = "kappa_diet",
                        xlab = "Scaled Percent Insectivory",
                        maxColorValue = 100,
                        palette = colorRampPalette(c("black","black"))(maxColorValue),
                        plot_ylab = FALSE,
                        plot_slope = FALSE)
    plot_linear_effects(load_from = lf_ch1NOBO,
                        fit_summary = ch1lineareffects, #still plots everything but the line off ch1lineareffects
                        variable_of_interest = "scale_insect_perc",
                        variable_kappa = "kappa_diet",
                        xlab = "Scaled Percent Insectivory",
                        trendline_lty = "dashed",
                        palette = colorRampPalette(c("black","black"))(maxColorValue),
                        new_plot = FALSE#,
                        #polygon_color = "orange"
                        )
    add_outlier(df = ch1lineareffects,
                variable_of_interest = "scale_insect_perc",
                add_legend = TRUE)
    
    
    #Habitat selectivity
    plot_linear_effects(load_from = lf_ch1m1,
                        variable_of_interest = "scale_habitat_ssi",
                        variable_kappa = "kappa_habitat_selection",
                        xlab = "Scaled Habitat Selectivity",
                        maxColorValue = 100,
                        palette = colorRampPalette(c("black","black"))(maxColorValue),
                        trendline_lty = "dashed",
                        plot_slope = FALSE)
    plot_linear_effects(load_from = lf_ch1NOBO,
                        fit_summary = ch1lineareffects, #still plots everything but the line off ch1lineareffects
                        variable_of_interest = "scale_habitat_ssi",
                        variable_kappa = "kappa_habitat_selection",
                        xlab = "Scaled Percent Insectivory",
                        trendline_lty = "solid",
                        palette = colorRampPalette(c("black","black"))(maxColorValue),
                        new_plot = FALSE#,
                        #polygon_color = "orange"
                        )
    add_outlier(df = ch1lineareffects,
                variable_of_interest = "scale_habitat_ssi",
                add_legend = TRUE)
    
    #Regional Trend
    ####UNSCALE THESE PLS!
    plot_linear_effects(load_from = lf_ch1m1,
                        variable_of_interest = "scale_usgs_trend",
                        variable_kappa = "kappa_regional",
                        xlab = "Scaled Regional Trend",
                        regional_trend_colors = TRUE,
                        plot_ylab = FALSE,
                        plot_slope = FALSE)
    plot_linear_effects(load_from = lf_ch1NOBO,
                        fit_summary = ch1lineareffects, #still plots everything but the line off ch1lineareffects
                        variable_of_interest = "scale_usgs_trend",
                        variable_kappa = "kappa_regional",
                        xlab = "Scaled Regional Trend",
                        trendline_lty = "solid",
                        regional_trend_colors = TRUE,
                        new_plot = FALSE#,
                        #polygon_color = "orange"
                        )
    abline(v = 0, lty = "dashed", col = "grey")
    abline(h = 0, lty = "dashed", col = "grey")
    legend("topleft",
           legend = c("RT Decreasing", "RT Decreasing [87% CI]", "RT Stable", "RT Increasing [87% CI]", "RT Increasing"),
           fill = c("#762a83", "#c2a5cf", stable_color, "#5aae61", "#1b7837"),
           bty = "n")
    add_outlier(df = ch1lineareffects,
                variable_of_interest = "scale_usgs_trend",
                add_legend = TRUE)
  }
  
  dev.off()
  
  #which ones do and don't agree between regional and local trend?
  disagreement <- ch1lineareffects |>
    filter(trend_agreement != "agree") |>
    dplyr::select(common_name, mean, sig_87, trend_direction, usgs_trend_estimate, scale_usgs_trend, rt_87, rt_direction, trend_agreement)
  
  #rt more or less extreme
  extremeness <- ch1lineareffects |>
    dplyr::select(common_name, mean, trend_direction, usgs_trend_estimate, rt_direction, trend_agreement) |>
    mutate(extreme = case_when(mean > 0 & mean > usgs_trend_estimate ~ "more positive",
                               mean > 0 & mean < usgs_trend_estimate ~ "less positive", 
                               mean < 0 & mean < usgs_trend_estimate ~ "more negative",
                               mean < 0 & mean > usgs_trend_estimate ~ "less negative"))
  
  #supplemental figure
  png(filename = "figures/ch1_SUPPLEMENTAL_linear_effects.png", 
      width = 1100,
      height = 600,
      units = "px", 
      type = "windows")
  {
    par(mar =c(5.1, 6.1, 4.1, 2.1), 
      cex = 1.3,
      cex.axis = 1.3,
      cex.lab = 1.6,
      cex.main = 1.6,
      cex.sub = 1.4,
      mfrow = c(2,4))
    
    new_ylab_distance = -1.6
  
  #Temperature Niche
  plot_linear_effects(load_from = lf_ch1m1,
                      variable_of_interest = "scale_ztempwq",
                      variable_kappa = "kappa_temp_pos",
                      xlab = "Scaled Temperature Niche Position",
                      trendline_lty = "dashed",
                      ylab_distance = new_ylab_distance)
  #should add a color bar legend
  #or text just above x axis with "colder" in blue and "warmer" in red
  #Percent Insectivory
  plot_linear_effects(load_from = lf_ch1m1,
                      variable_of_interest = "scale_insect_perc",
                      variable_kappa = "kappa_diet",
                      xlab = "Scaled Percent Insectivory",
                      maxColorValue = 100,
                      palette = colorRampPalette(c("black","black"))(maxColorValue),
                      plot_ylab = FALSE)
  
  #Habitat selectivity
  plot_linear_effects(load_from = lf_ch1m1,
                      variable_of_interest = "scale_habitat_ssi",
                      variable_kappa = "kappa_habitat_selection",
                      xlab = "Scaled Habitat Selectivity",
                      maxColorValue = 100,
                      palette = colorRampPalette(c("black","black"))(maxColorValue),
                      trendline_lty = "dashed",
                      plot_ylab = FALSE)
  #Regional Trend
  plot_linear_effects(load_from = lf_ch1m1,
                      variable_of_interest = "scale_usgs_trend",
                      variable_kappa = "kappa_regional",
                      xlab = "Scaled Regional Trend",
                      regional_trend_colors = TRUE,
                      plot_ylab = FALSE)
  #should add a color bar legend
  
  
  #Now, plot without regional trends.
  #temp niche
  plot_linear_effects(fit_summary = ch1noregionaleffects,
                      load_from = lf_ch1nR,
                      variable_of_interest = "scale_ztempwq",
                      variable_kappa = "kappa_temp_pos",
                      xlab = "Scaled Temperature Niche Position",
                      ylab_distance = new_ylab_distance)
  
  #Percent Insectivory
  plot_linear_effects(fit_summary = ch1noregionaleffects,
                      load_from = lf_ch1nR,
                      variable_of_interest = "scale_insect_perc",
                      variable_kappa = "kappa_diet",
                      xlab = "Scaled Percent Insectivory",
                      maxColorValue = 100,
                      palette = colorRampPalette(c("black","black"))(maxColorValue),
                      plot_ylab = FALSE)
  
  #Habitat selectivity
  plot_linear_effects(fit_summary = ch1noregionaleffects,
                      load_from = lf_ch1nR,
                      variable_of_interest = "scale_habitat_ssi",
                      variable_kappa = "kappa_habitat_selection",
                      xlab = "Scaled Habitat Selectivity",
                      maxColorValue = 100,
                      palette = colorRampPalette(c("black","black"))(maxColorValue),
                      trendline_lty = "dashed",
                      plot_ylab = FALSE)
  }
  
  dev.off()
  
  
  
###################################
#
# Plot effect of observer effects
#
#
####################################
  
  #get all the count datapoints
  data <- read.csv("data/analysis.df.csv") |>
    dplyr::select(common_name, route, count, observer_ID, observer_quality)
  
  plot(x = data$observer_quality,
       y = data$count,
       xlab = "Observer Quality",
       ylab = "Route-level Species Counts",
       pch = 1)
  
  variable_of_interest = "observer_quality"
  fit_summary = data
  voi_sequence <- seq(min(fit_summary[,variable_of_interest], na.rm = TRUE),
                      max(fit_summary[,variable_of_interest], na.rm = TRUE),
                      length.out = 100)
  
  #use universal intercept and the gamma_b species slope. M you know what I didn't save the posterior samples for the any of the intercepts. Okay, I can't plot this rn unless I re-run the model and let it include the universal intercept in the posterior samples output. Shouldn't do that unless it's asked for by reviewers by from feedback.

  #hum. idk that I really need to plot this so much.
  #alright yeah, let me just report the effect size without doing anything else
  
#############################
#
# Calculate 87% credible intervals for kappa variables.
#
#############################
  load_from = lf_ch1m1
  posterior_draws = read.csv(paste0(load_from, "posterior_draws.csv")) |>
    dplyr::select(kappa_regional, kappa_habitat_selection, kappa_temp_pos, kappa_diet, gamma_b)
  
  hab_6.5 <- as.numeric(quantile(posterior_draws$kappa_habitat_selection, probs = 0.065))  # (1-0.87)/2 = 0.065
  hab_93.5 <- as.numeric(quantile(posterior_draws$kappa_habitat_selection, probs = 0.935))  # 1 - 0.065 = 0.935
  hab_2.5 <- as.numeric(quantile(posterior_draws$kappa_habitat_selection, probs = 0.025))
  hab_97.5 <- as.numeric(quantile(posterior_draws$kappa_habitat_selection, probs = 0.975))
  
  temp_6.5 <- as.numeric(quantile(posterior_draws$kappa_temp_pos, probs = 0.065))  # (1-0.87)/2 = 0.065
  temp_93.5 <- as.numeric(quantile(posterior_draws$kappa_temp_pos, probs = 0.935))  # 1 - 0.065 = 0.935   
  temp_2.5 <- as.numeric(quantile(posterior_draws$kappa_temp_pos, probs = 0.025))
  temp_97.5 <- as.numeric(quantile(posterior_draws$kappa_temp_pos, probs = 0.975))
  
  #and just confirm that the 95% match our actual known values..
  kappas <- read.csv(paste0(lf_ch1m1, "fit_summary.csv")) |>
    filter(str_detect(rownames, "kappa_")) |>
    dplyr::select(rownames, mean, sd, conf_2.5, conf_97.5)

  round(temp_2.5, 4) == round(kappas$conf_2.5[kappas$rownames == "kappa_temp_pos"], 4)
  #-0.0094 vs -0.0092 - off by a bit. I don't htink this lets us confirm temp_93.5 is accurate at 87% credible interval, but magnitude of error suggests that habitat is supported at that margin. m. If I need to provide more details, I can rerun the model and have it calculate that 87% CI with the full fit summary.