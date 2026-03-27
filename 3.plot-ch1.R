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
library(tinytex) #lightweight LaTeX distribution that makes it easy to use in R

#load functions
source("3.plot-functions.R")

#where are we pulling data from?
lf_ch1m1 <- "model/2026.03.12_ch1_m1_final/"
lf_ch1nR <- "model/2026.03.12_ch1_withoutregional_final/"
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
           color = "black",
           id = row_number(),
           significant = ifelse(conf_2.5 < 0 & conf_97.5 > 0, FALSE, TRUE),
           pch = ifelse(significant == TRUE, 16, 1))
  
  rt_model_color = "black"
  wo_model_color = "grey30"
  
  png(filename = "figures/ch1_model_CI_comparison.png", 
      width = 420,
      height = 440,
      units = "px", 
      type = "windows")
  {
  par(mar = c(4, 10, 2, 1))
  plot(x = c1m1$mean,
       y = c1m1$id, 
       pch = c1m1$pch,
       cex = 2,
       xlim = c(-.02,.04),
       xlab = "Effect Size",
       xaxt = "n",
       yaxt = "n",
       ylab = "",
       col = rt_model_color) 
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
           y = c1mNR$id+.75, 
           pch = c1mNR$pch,
           cex = 2,
           col = wo_model_color) +
    segments(x0 = c1mNR$conf_2.5,
             x1 = c1mNR$conf_97.5,
             y0 = c1mNR$id+.75,
             lwd = 4.5,
             col = wo_model_color)
  }
  dev.off()
  #make legend seperately and paste later
  #only needed if plotting both models (with and without regional trend)
  png(filename = "figures/ch1_model_CI_comparison_LEGEND.png", 
      width = 400,
      height = 440,
      units = "px", 
      type = "windows") 
  {
  par(mar = c(2, 10, 2, 1))
  plot(1:10, 
       1:10, 
       pch = NA) 
  legend("center",
         bty = "n",
         legend = c("Model with \nRegional Trend\n", "Model without \nRegional Trend"), 
         fill = c(rt_model_color, wo_model_color)
  )
  }
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
      trend_direction == "slightly_negative" ~ "#c2a5cf", #decline supported at 87% confidence interval
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
         legend = c("Decreasing [95% CI]", "Likely Decreasing [87% CI]", "Stable", "Likely Increasing [87% CI]","Increasing [95% CI]"),
         fill = c("#762a83", "#c2a5cf", stable_color,"#5aae61", "#1b7837"),
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
           legend = c("Decreasing [95% CI]", "Likely Decreasing [87% CI]", "Stable", "Likely Increasing [87% CI]","Increasing [95% CI]"),
           fill = c("#762a83", "#c2a5cf", stable_color,"#5aae61", "#1b7837"),
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
             legend = c("Decreasing [95% CI]", "Likely Decreasing [87% CI]", "Stable", "Likely Increasing [87% CI]","Increasing [95% CI]"),
             fill = c("#762a83", "#c2a5cf", stable_color,"#5aae61", "#1b7837"),
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
                        trendline_lty = "dashed")
    legend("bottomleft",
           legend = c("NC Colder"),
           fill = c("blue"),
           bty = "n")
    legend("bottomright",
           legend = c("NC Warmer"),
           fill = c("red"),
           bty = "n")
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
                        trendline_lty = "dashed")
    #Regional Trend
    plot_linear_effects(load_from = lf_ch1m1,
                        variable_of_interest = "scale_usgs_trend",
                        variable_kappa = "kappa_regional",
                        xlab = "Scaled Regional Trend",
                        regional_trend_colors = TRUE,
                        plot_ylab = FALSE)
    abline(v = 0, lty = "dashed", col = "grey")
    abline(h = 0, lty = "dashed", col = "grey")
    legend("topleft",
           legend = c("RT Decreasing", "RT Decreasing [87% CI]", "RT Stable", "RT Increasing [87% CI]", "RT Increasing"),
           fill = c("#762a83", "#c2a5cf", stable_color, "#5aae61", "#1b7837"),
           bty = "n")
  }
  
  dev.off()
  
  #which ones do and don't agree?
  disagreement <- ch1lineareffects |>
    filter(trend_agreement != "agree") |>
    dplyr::select(common_name, mean, sig_87, trend_direction, usgs_trend_estimate, scale_usgs_trend, rt_87, rt_direction, trend_agreement)
  
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
  
  
  
  