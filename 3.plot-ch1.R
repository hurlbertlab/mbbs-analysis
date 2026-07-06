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
library(sf)
library(terra)
library(ggplot2)
library(scales) #for some color selection from ggplot

#load functions
source("3.plot-functions.R")

#where are we pulling data from?
lf_ch1m1 <- "model/2026.06.27_ch1_m1_2024rt/" #updated to use version with 2024 regional trend information
lf_ch1nR <- "model/2026.03.12_ch1_withoutregional_final/"
lf_ch1NOBO <- "model/2026.06.26_ch1_rmNOBO_2024rt/" #updated to use version with 2024 regional trend information
lf_ch1NOBO_nR <- "model/2026.04.09_ch1_rmNOBO_woRegional_final/"
stable_color <- "#4393c3"
#lf_dieto <- "Z:/Goulden/mbbs-analysis/model/2025.12.1_ch1_m1_diettest/"
#lf_tempo <- "Z:/Goulden/mbbs-analysis/model/2025.11.25_ch1_m1_temponly/"

  
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
             TRUE ~ "unaccounted"),
          trend_direction_nps = case_when(
              conf_2.5 > 0 ~ "positive",
              conf_97.5 < 0 ~ "negative",
              conf_6.5 > 0 ~ "positive",
              conf_93.5 < 0 ~ "negative",
              TRUE ~ "stable"
          ),
          pch = case_when(
            trend_direction == "stable" ~ 0,
            TRUE ~ 16
          ),
          lty = case_when(
            trend_direction == "stable" ~ "solid",
            TRUE ~ "solid"
          ))

  #from 5,000 posterior draws..
  prop_posterior <- read.csv(paste0(lf_ch1m1, "posterior_draws.csv")) |>
    summarize(across(everything(),
                     (prop_gt_0 = ~sum(. > 0)/5000))) |>
    tidyr::pivot_longer(cols = b.1:b.60, 
                        names_to = "variable") |>
    dplyr::select(variable, value) |>
    rename(prop_posterior_gt_0 = value) |>
    mutate(common_name_standard = as.integer(str_extract(variable, "[0-9]([0-9])?"))) |>
    dplyr::select(-variable) 
  
  
  #n sigifnicantly increasing/decreasing species:
  statuser::table2(ch1sp$trend_direction)
  statuser::table2(ch1sp$trend_direction_nps)
  
  maxColorValue = 100
  palette_blue = colorRampPalette(c("#762a83", "#c2a5cf", "#92c5de", "#a6dba0", "#5aae61"))(maxColorValue)
  #palette_blue = colorRampPalette(c("#762a83", "#c2a5cf", stable_color, "#a6dba0", "#5aae61"))(maxColorValue)
  palette_white = colorRampPalette(c("#762a83", "white", "#68f273"))(maxColorValue)
  #"#62e36c" #backup, slightly darker color.
  continous_colors <- data.frame(palette_blue, 
                                 palette_white, 
                                 palette_percent = seq(from = 0, to = 1, length.out = 100)) |>
    mutate(palette_percent = round(palette_percent, digits = 2))
  
  plot_horiz <- ch1sp %>%
    mutate(color = case_when(
      trend_direction == "negative" ~ "#762a83", #decreasing
      #trend_direction == "slightly_negative" ~ "#c2a5cf", #decline supported at 87% confidence interval
      trend_direction == "slightly_negative" ~ "#762a83",
      trend_direction == "stable" ~ stable_color, #stable, not significant at 95 or 87%
      trend_direction == "slightly_positive" ~ "#5aae61", #increase supported at 87% confidence interval
      trend_direction == "positive" ~ "#1b7837" #increasing
    )) %>%
    arrange(mean) %>%
    mutate(sp_id = cur_group_rows()) %>%
    ungroup() |>
    left_join(prop_posterior, by = c("common_name_standard")) |>
    mutate(prop_posterior_color_onetone = case_when(
      prop_posterior_gt_0 > .97 | prop_posterior_gt_0 < .03 ~ "#1b7837",
      (prop_posterior_gt_0 > 0.8) | (prop_posterior_gt_0 < 0.2) ~ "#5aae61",
      (prop_posterior_gt_0 > 0.6) | (prop_posterior_gt_0 < 0.4) ~ "#92c5de",
      prop_posterior_gt_0 >= 0.4 | prop_posterior_gt_0 <= 0.6 ~ stable_color
    ),
    prop_posterior_color_twotone = case_when(
      prop_posterior_gt_0 > .97 ~ "#1b7837",
      prop_posterior_gt_0 < .03 ~ "#762a83",
      (prop_posterior_gt_0 > 0.8) ~ "#5aae61",
      (prop_posterior_gt_0 < 0.2) ~ "#c2a5cf",
      (prop_posterior_gt_0 > 0.6) ~ "#92c5de",
      (prop_posterior_gt_0 < 0.4) ~ "#92c5de",
      prop_posterior_gt_0 >= 0.4 | prop_posterior_gt_0 <= 0.6 ~ stable_color  
    )) |>
    mutate(rounded_prop_posterior = round(prop_posterior_gt_0, digits = 2)) |>
    #put on a continous color ramp
    left_join(continous_colors, by = c("rounded_prop_posterior" = "palette_percent"))
  #no species are in the "decline supported at the 87% CI and mean greater than .01 
  
  table2(plot_horiz$prop_posterior_gt_0 < .065,
         plot_horiz$prop_posterior_gt_0 > .935,
         plot_horiz$trend_direction_nps) #ya, same as trend_direction_nps.
  
  plot_horiz$color <- plot_horiz$palette_blue
  #hm uh. looking at this plot tho like. Blue-grey Gnatcatcher has been colored w grey80 even though it's posterior distribution 
  
#  png(filename = "figures/ch1/ch1_horiz_pop_change.png", 
#    width = 1200,
#    height = 600,
#    units = "px", 
#    type = "windows")
#  {
#  par(mar = c(13.5, 5, 2, 2), 
#      cex = 1.1,
#      cex.axis = 1.3,
#      cex.lab = 1.2,
#      cex.main = 1.6,
#      cex.sub = 1.4,
#      bg = NA)
#  plot_intervals(plot_df = plot_horiz,
#                 species_axis = "x",
#                 ylim_select = c(-.15, .07),
#                 xlim_select = c(0, 61),
#                 title = "North Carolina Mini Breeding Bird Survey Species Population Trends",
#                 yaxt = "n") 
#  axis(side = 2, 
#      #at = seq(-.14, .08, by = 0.02), 
#       #labels = TRUE,
#       at = c(-.14, -.12, -.10, -.08, -.06, -.04, -.02, 0, .02, .04, .06, .08),
#       labels = c("-14%", "-12%", "-10%", "-8%", "-6%", "-4%", 
#                  "-2%", "0", "2%", "4%", "6%", "8%"),
#       las = 1) 
#  axis(side = 2, at = seq(-.13, 0.05, by = 0.02),
#       labels = FALSE,
#       tck = -0.01) 
#  mtext(text = "Percent Population Change per Year", side = 2, line = 4, cex = 1.3) 
#  legend("topleft",
#         legend = c("Decreasing", 
#                   #"Likely Decreasing [87% CI]", 
#                    "Stable", 
#                    #"Likely Increasing [87% CI]",
#                    "Increasing"),
#         fill = c("#762a83",
#                  #"#c2a5cf", 
#                  stable_color,
#                  #"#5aae61",
#                  "#1b7837"),
#         bty = "n")
#  }
#  dev.off()
  
  #turn label coloing on/off
  label_colorful = TRUE
  
  ##try a two-panel version
  png(filename = "figures/ch1/ch1_horiz_pop_change_twopanel_thrublue_stable.png", 
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
        mfrow = c(2,1)
        #bg = NA
        )
    {
    plot_intervals(plot_df = plot_horiz[1:30,],
                   species_axis = "x",
                   ylim_select = c(-.15, .01),
                   xlim_select = c(0, 30.5),
                   title = "North Carolina Mini Breeding Bird Survey Species Population Trends",
                   yaxt = "n",
                   label_colorful = label_colorful) 
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
    plot_intervals_legend("continuous")
    
    par(mar = c(11, 6, .5, 1))
    {
    plot_intervals(plot_df = plot_horiz[31:60,],
                   species_axis = "x",
                   ylim_select = c(-.08, .08),
                   xlim_select = c(30, 60.5),
                   title = "",
                   yaxt = "n",
                   label_colorful = label_colorful) 
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
      plot_intervals_legend("continuous")
    }
    }
  }
  dev.off()
  
  
  #make a color ramp for the figure legend
  png(filename = "figures/ch1/ch1_horiz_pop_change_LEGEND_continous.png", 
      width = 900,
      height = 800,
      units = "px", 
      type = "windows") 
  colors <- palette_blue
  legend_image <- as.raster(matrix(colors, ncol = 1))
  plot(c(1,10), c(1,10))
  rasterImage(legend_image, xleft = 4, ybottom = 2, xright = 5, ytop = 8)
  #text("okay",
  #     x= 5.5,
  #     y = 8.2)
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
    )) |>
    #m, actually.... I want colors on the regional trend graph to reflect the actual trend I think...
    mutate(color = case_when(
      trend_direction == "negative" ~ "#762a83", #decreasing
      #trend_direction == "slightly_negative" ~ "#c2a5cf", #decline supported at 87% confidence interval
      trend_direction == "slightly_negative" ~ stable_color,
      trend_direction == "stable" ~ stable_color, #stable, not significant at 95 or 87%
      trend_direction == "slightly_positive" ~ "#5aae61", #increase supported at 87% confidence interval
      trend_direction == "positive" ~ "#1b7837" #increasing
    )) |>
    mutate(rt_colors = color) |>
    mutate(ta_colors = case_when(
      trend_agreement == "agree" ~ "#5aae61",
      trend_agreement == "disagree" ~ "#762a83",
      trend_agreement == "disagree, regional NS" ~ "#5e4fa2",
      trend_agreement == "disagree, local NS" ~ "#5e4fa2",
      TRUE ~ "blue" #error color
    ),
    no_color = "black"
    ) |>
    #and, let's filter out Bobwhite up here
    filter(!common_name %in% "Northern Bobwhite")
  
  #without bobwhite
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
  #make 1 row. move panels closer together. only need y axis on first one.
  png(filename = "figures/ch1/ch1_linear_effects_3panel.png", 
      width = 900,
      height = 300,
      units = "px", 
      type = "windows")
  {
    par(mar =c(5.1, 6.5, 1.1, 1.1), 
        cex = 1.5,
        cex.axis = 1.5,
        cex.lab = 2,
        cex.main = 2,
        cex.sub = 1.5,
        mfrow = c(1,3) #SHOULD BE 1,3
        #bg = NA
        )
    
    #Temperature Niche
    plot_linear_effects(load_from = lf_ch1m1,
                        variable_of_interest = "scale_ztempwq",
                        variable_kappa = "kappa_temp_pos",
                        xlab = "Scaled Temperature Niche Position",
                        trendline_lty = "dashed",
                        #palette = colorRampPalette(c("black","black"))(maxColorValue),
                        plot_ylab = TRUE,
                        plot_slope = FALSE)
    #legend("bottomleft",
    #       legend = c("NC Colder"),
    #       fill = c("blue"),
    #       bty = "n")
    #legend("bottomright",
    #       legend = c("NC Warmer"),
    #       fill = c("red"),
    #       bty = "n")
    #and now if I want to add the sensitivity analysis on top.... I want to be able to plot JUST the line without starting a new plot.
    #and I want to re-print the segments and cex with issues.
    plot_linear_effects(load_from = lf_ch1NOBO,
                        fit_summary = ch1lineareffects, #still plots everything but the line off ch1lineareffects
                        variable_of_interest = "scale_ztempwq",
                        variable_kappa = "kappa_temp_pos",
                        xlab = "Scaled Temperature Niche Position",
                        trendline_lty = "solid",
                        plot_ylab = FALSE,
                        #palette = colorRampPalette(c("black","black"))(maxColorValue),
                        new_plot = FALSE#,
                        #polygon_color = "orange"
    )
    #add_outlier(df = ch1lineareffects,
    #            variable_of_interest = "scale_ztempwq",
    #            add_legend = FALSE)
    
    #Habitat selectivity
    plot_linear_effects(load_from = lf_ch1m1,
                        variable_of_interest = "scale_habitat_ssi",
                        variable_kappa = "kappa_habitat_selection",
                        xlab = "Scaled Habitat Selectivity",
                        ylab = "Local Population Trend",
                        maxColorValue = 100,
                        palette = colorRampPalette(c("black","black"))(maxColorValue),
                        trendline_lty = "dashed",
                        plot_slope = FALSE,
                        plot_ylab = FALSE)
    plot_linear_effects(load_from = lf_ch1NOBO,
                        fit_summary = ch1lineareffects, #still plots everything but the line off ch1lineareffects
                        variable_of_interest = "scale_habitat_ssi",
                        variable_kappa = "kappa_habitat_selection",
                        xlab = "Scaled Percent Insectivory",
                        trendline_lty = "solid",
                        palette = colorRampPalette(c("black","black"))(maxColorValue),
                        plot_ylab = FALSE,
                        new_plot = FALSE#,
                        #polygon_color = "orange"
    )
    #add_outlier(df = ch1lineareffects,
    #            variable_of_interest = "scale_habitat_ssi",
    #            add_legend = FALSE,
    #            outlier_color = "grey20") #might end up removing the outlier completely in the end, and would rescale the axes.. - DONE

    #Percent Insectivory
    plot_linear_effects(load_from = lf_ch1m1,
                        variable_of_interest = "scale_insect_perc",
                        variable_kappa = "kappa_diet",
                        xlab = "Scaled Percent Insectivory",
                        maxColorValue = 100,
                        palette = colorRampPalette(c("black","black"))(maxColorValue),
                        plot_ylab = FALSE,
                        xlabels = c("-2", "-1.5", "-1", "-0.5", "0", "0.5", "1", "1.5", "2", "2.5"),
                        xlabels_location = c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5),
                        #plot_ylab = TRUE,
                        #ylab_distance = - .7,
                        plot_slope = FALSE)
    plot_linear_effects(load_from = lf_ch1NOBO,
                        fit_summary = ch1lineareffects, #still plots everything but the line off ch1lineareffects
                        variable_of_interest = "scale_insect_perc",
                        variable_kappa = "kappa_diet",
                        xlab = "Scaled Percent Insectivory",
                        trendline_lty = "dashed",
                        palette = colorRampPalette(c("black","black"))(maxColorValue),
                        new_plot = FALSE,#,
                        #polygon_color = "orange"
                        plot_ylab = FALSE)
    #add_outlier(df = ch1lineareffects,
    #            variable_of_interest = "scale_insect_perc",
    #            add_legend = FALSE)

  }
  dev.off()
  
# plot the legend for temperature niche position
# Create color gradient
colors <- colorRampPalette(c("blue","red"))(maxColorValue)
legend_image <- as.raster(matrix(colors, ncol = 1))
plot(c(1,10), c(1,10))
rasterImage(legend_image, xleft = 4, ybottom = 2, xright = 5, ytop = 8)
mtext("NC colder")
mtext("NC warmer", side = 4)
#and then snip to get the images from the box, already in the ch1_figures.ppt
  
  
  #And, for plotting regional trend seperate, I want an unscaled version so it's easy to show the 1:1 lines.
  #trend ~ gamma_b + kappa_regional * scaled_rt
  #trend ~ gamma_b + kappa_regional * (sub in formula for scaled rt = (rt - mean(rt))/sd(rt)
  #trend ~ gamma_b + kappa_rt*(rt/sd_rt) - kappa_rt*(mean(rt)/sd(rt))
  #rearrange:
  # ~ gamma_b - kappa_rt*(mean(rt)/sd(rt)) + kappa_rt*(rt/sd_rt)
  # ~ gamma_b - kappa_rt*(mean(rt)/sd(rt)) + regional_trend*(kappa_rt/sd_rt)
  #so the new unscaled intercept is gamma_b - kappa_rt*(mean(rt)/sd(rt))
  #and the new unscaled slope is (kappa_rt/sd_rt) (*regional_trend)
  unscaled_intercept <- ch1NOBOlinear$mean[ch1NOBOlinear$rownames == "gamma_b"] -
    ch1NOBOlinear$mean[ch1NOBOlinear$rownames == "kappa_regional"]*(mean(ch1lineareffects$usgs_trend_estimate, na.rm = TRUE)/sd(ch1lineareffects$usgs_trend_estimate, na.rm = TRUE)) 
  #scaling is done with all the species so it's trend line w/o bobwhite but mean and sd with bobwhite included
  #and let's unscale things for regional trend
  unscaled_kappa_regional = ch1NOBOlinear$mean[ch1NOBOlinear$rownames == "kappa_regional"] / sd(ch1lineareffects$usgs_trend_estimate, na.rm = TRUE)
  all_rt_sd = sd(ch1lineareffects$usgs_trend_estimate, na.rm = TRUE)
  all_rt_mean = (mean(ch1lineareffects$usgs_trend_estimate, na.rm = TRUE))
  
  png(filename = "figures/ch1/ch1_linear_effects_RT.png", #black, tacol, trendcol
      width = 550,
      height = 500,
      units = "px", 
      type = "windows")
  {
    par(mar =c(5.1, 7.1, 3.1, 2.1), 
        cex = 1.3,
        cex.axis = 1.3,
        cex.lab = 1.3,
        cex.main = 1.6,
        cex.sub = 1.4,
        bg = NA)
    #Regional Trend
    
    #rt_colors_variable = "ta_colors"
    #rt_colors_variable = "color"
    rt_colors_variable = "no_color"

    plot_linear_effects(load_from = lf_ch1m1,
                        variable_of_interest = "usgs_trend_estimate",
                        variable_kappa = "kappa_regional",
                        xlab = "Regional Trend",
                        regional_trend_colors = TRUE,
                        rt_colors_variable = rt_colors_variable,
                        plot_ylab = TRUE,
                        ylab_distance = -0.03,
                        plot_slope = FALSE,
                        color_segments_differently = TRUE,
                        segments_color = "grey10",
                        ylab_cex = 1.4,
                        choose_xlim = c(-0.055, 0.055),
                        xlabels = c('-0.06','-0.04', '-0.02', '0.00', '0.02', '0.04', '0.06'),
                        xlabels_location = c(-0.06, -0.04, -0.02, 0.00, 0.02, 0.04, 0.06))
    #add triangles
    #above the 1:1 line above zero
    polygon(c(0, 0.08, 0), 
            c(0, 0.08, 0.08), 
            col = adjustcolor("grey", alpha.f = 0.3), 
            border = NA)
    #below the 1:1 line below zero
    polygon(c(0, -0.16, 0), 
            c(0, -0.16, -0.16), 
            col = adjustcolor("grey", alpha.f = 0.3), 
            border = NA)
    abline(v = 0, lty = "dashed", col = "grey")
    abline(h = 0, lty = "dashed", col = "grey")
    abline(a = 0, b = 1, lty = "dashed", col = "grey")
    
    #add error bars in x direction from regional trend
    segments(x0 = ch1lineareffects$usgs_2.5CI[!is.na(ch1lineareffects$common_name_standard)]*.01,
             x1 = ch1lineareffects$usgs_97.5CI[!is.na(ch1lineareffects$common_name_standard)]*.01,
             y0 = ch1lineareffects$mean[!is.na(ch1lineareffects$common_name_standard)],
             lwd = 3,
             col = "grey15")
    
    plot_linear_effects(load_from = lf_ch1NOBO,
                        fit_summary = ch1lineareffects, #still plots everything but the line off ch1lineareffects
                        variable_of_interest = "usgs_trend_estimate",
                        variable_kappa = "kappa_regional",
                        xlab = "Regional Trend",
                        xaxt = "n",
                        xlabels = NA,
                        choose_xlim = NA,
                        trendline_lty = "solid",
                        regional_trend_colors = TRUE,
                        rt_colors_variable = rt_colors_variable,
                        new_plot = FALSE,
                        unscale_regional_trend = TRUE,
                        all_rt_sd = all_rt_sd, 
                        all_rt_mean = all_rt_mean,
                        color_segments_differently = TRUE,
                        segments_color = "grey15",
                        ylab_cex = 1.4,
                        base_rt_kappa = ch1NOBOlinear$mean[ch1NOBOlinear$rownames == "kappa_regional"]#,
                        #polygon_color = "orange"
                        )
    #legend("topleft",
    #       legend = c("RT Decreasing", "RT Decreasing [87% CI]", "RT Stable", "RT Increasing [87% CI]", "RT Increasing"),
    #       fill = c("#762a83", "#c2a5cf", stable_color, "#5aae61", "#1b7837"),
    #       bty = "n")
    #add_outlier(df = ch1lineareffects,
    #            variable_of_interest = "usgs_trend_estimate",
    #            add_legend = FALSE)
    legend("topleft",
           legend = c("Local Trends More Extreme"),
           fill = c(adjustcolor("grey", alpha.f = 0.8)
           ),
           #density = 50,
           bty = "n")
    if(rt_colors_variable == "no_color") {
    #legend("topleft",
           #legend = c("Shaded Area Local Trends More Extreme"),
           #fill = c(adjustcolor("grey", alpha.f = 0.8)
          #          ),
          # bty = "n")
      #add_outlier(df = ch1lineareffects,
      #            variable_of_interest = "usgs_trend_estimate",
      #            add_legend = TRUE)
    } else if(rt_colors_variable == "color") {
      legend("bottomright",
             legend = c("Decreasing Locally",
                        "Stable Locally",
                        "Increasing Locally"#,
                        #"Removed Outlier"
                        ),
             fill = c("#762a83",
                      stable_color,
                      "#1b7837"#, 
                      #"orange"
                      ),
             bty= "n")
      legend("topleft",
             legend = c("Local Trends More Extreme"),
             fill = c(adjustcolor("grey", alpha.f = 0.8)
             ),
             density = 50,
             bty = "n")
    } else if(rt_colors_variable == "ta_colors") {
      legend("bottomright",
             legend = c("Agree",
                        "Disagree, stable",
                        "Disagree",
                        "Removed Outlier"),
             fill = c("#5aae61", "#5e4fa2", "#762a83", "orange"),
             bty = "n")
    }
  }
  dev.off()
  
  
  png(filename = "figures/ch1/ch1_linear_effects_S1.png", 
      width = 800,
      height = 800,
      units = "px", 
      type = "windows")
  {
    par(mar =c(5.1, 7.1, 4.1, 2.1), 
        cex = 1.3,
        cex.axis = 1.3,
        cex.lab = 1.6,
        cex.main = 1.6,
        cex.sub = 1.4,
        mfrow = c(2,2))
    
    outlier_df <- read.csv(paste0(lf_ch1m1, "fit_summary.csv")) %>%
      filter(str_detect(.$rownames, "b|gamma|kappa_")) %>%
      mutate(common_name_standard = as.integer(str_extract(.$rownames, "[0-9]([0-9])?"))) %>%
      left_join(read.csv(paste0(lf_ch1m1, "species_traits.csv")), by = "common_name_standard") %>%
      left_join(read.csv(paste0(lf_ch1m1, "beta_to_common_name.csv"))) |>
      filter(common_name == "Northern Bobwhite")
    
    #Temperature Niche
    plot_linear_effects(load_from = lf_ch1m1,
                        variable_of_interest = "scale_ztempwq",
                        variable_kappa = "kappa_temp_pos",
                        xlab = "Scaled Temperature Niche Position",
                        trendline_lty = "dashed",
                        ylim = c(-.15, 0.07),
                        plot_slope = FALSE)
    #legend("bottomleft",
    #       legend = c("NC Colder"),
    #       fill = c("blue"),
    #       bty = "n")
    #legend("bottomright",
    #       legend = c("NC Warmer"),
    #       fill = c("red"),
    #       bty = "n")
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
    add_outlier(df = outlier_df,
                variable_of_interest = "scale_ztempwq",
                add_legend = FALSE)
    
    #Habitat selectivity
    plot_linear_effects(load_from = lf_ch1m1,
                        variable_of_interest = "scale_habitat_ssi",
                        variable_kappa = "kappa_habitat_selection",
                        xlab = "Scaled Habitat Selectivity",
                        maxColorValue = 100,
                        palette = colorRampPalette(c("black","black"))(maxColorValue),
                        trendline_lty = "dashed",
                        plot_slope = FALSE,
                        ylim = c(-.15, 0.07))
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
    add_outlier(df = outlier_df,
                variable_of_interest = "scale_habitat_ssi",
                add_legend = FALSE)
    
    #Percent Insectivory
    plot_linear_effects(load_from = lf_ch1m1,
                        variable_of_interest = "scale_insect_perc",
                        variable_kappa = "kappa_diet",
                        xlab = "Scaled Percent Insectivory",
                        maxColorValue = 100,
                        palette = colorRampPalette(c("black","black"))(maxColorValue),
                        plot_ylab = TRUE,
                        ylim = c(-.15, 0.07),
                        plot_slope = FALSE,
                        ylab_distance = - .7)
    plot_linear_effects(load_from = lf_ch1NOBO,
                        fit_summary = ch1lineareffects, #still plots everything but the line off ch1lineareffects
                        variable_of_interest = "scale_insect_perc",
                        variable_kappa = "kappa_diet",
                        xlab = "Scaled Percent Insectivory",
                        trendline_lty = "dashed",
                        palette = colorRampPalette(c("black","black"))(maxColorValue),
                        new_plot = FALSE,#,
                        #polygon_color = "orange"
                        plot_ylab = FALSE)
    add_outlier(df = outlier_df,
                variable_of_interest = "scale_insect_perc",
                add_legend = FALSE)
    
    #Regional Trend
    plot_linear_effects(load_from = lf_ch1m1,
                        variable_of_interest = "scale_usgs_trend",
                        variable_kappa = "kappa_regional",
                        xlab = "Scaled Regional Trend",
                        #regional_trend_colors = TRUE,
                        palette = colorRampPalette(c("black","black"))(maxColorValue),
                        plot_ylab = FALSE,
                        ylim = c(-.15, 0.07),
                        choose_xlim = c(-3.5, 2),
                        plot_slope = FALSE)
    plot_linear_effects(load_from = lf_ch1NOBO,
                        fit_summary = ch1lineareffects, #still plots everything but the line off ch1lineareffects
                        variable_of_interest = "scale_usgs_trend",
                        variable_kappa = "kappa_regional",
                        xlab = "Scaled Regional Trend",
                        trendline_lty = "solid",
                        #regional_trend_colors = TRUE,
                        palette = colorRampPalette(c("black","black"))(maxColorValue),
                        new_plot = FALSE#,
                        #polygon_color = "orange"
    )
    abline(v = 0, lty = "dashed", col = "grey")
    abline(h = 0, lty = "dashed", col = "grey")
    #legend("topleft",
    #       legend = c("RT Decreasing", "RT Decreasing [87% CI]", "RT Stable", "RT Increasing [87% CI]", "RT Increasing"),
    #       fill = c("#762a83", "#c2a5cf", stable_color, "#5aae61", "#1b7837"),
    #       bty = "n")
    add_outlier(df = outlier_df,
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
    filter(!is.na(common_name)) |>
    mutate(extreme = case_when(mean > 0 & mean > usgs_trend_estimate ~ "more positive",
                               mean > 0 & mean < usgs_trend_estimate ~ "less positive", 
                               mean < 0 & mean < usgs_trend_estimate ~ "more negative",
                               mean < 0 & mean > usgs_trend_estimate ~ "less negative"))
  table(extremeness$extreme)
  (length(extremeness$extreme[extremeness$extreme == "more positive"]) +
   length(extremeness$extreme[extremeness$extreme == "more negative"])) / nrow(extremeness)
  
  # #supplemental figure
  # png(filename = "figures/ch1/ch1_SUPPLEMENTAL_linear_effects.png", 
  #     width = 1100,
  #     height = 600,
  #     units = "px", 
  #     type = "windows")
  # {
  #   par(mar =c(5.1, 6.1, 4.1, 2.1), 
  #     cex = 1.3,
  #     cex.axis = 1.3,
  #     cex.lab = 1.6,
  #     cex.main = 1.6,
  #     cex.sub = 1.4,
  #     mfrow = c(2,4))
  #   
  #   new_ylab_distance = -1.6
  # 
  # #Temperature Niche
  # plot_linear_effects(load_from = lf_ch1m1,
  #                     variable_of_interest = "scale_ztempwq",
  #                     variable_kappa = "kappa_temp_pos",
  #                     xlab = "Scaled Temperature Niche Position",
  #                     trendline_lty = "dashed",
  #                     ylab_distance = new_ylab_distance)
  # #should add a color bar legend
  # #or text just above x axis with "colder" in blue and "warmer" in red
  # #Percent Insectivory
  # plot_linear_effects(load_from = lf_ch1m1,
  #                     variable_of_interest = "scale_insect_perc",
  #                     variable_kappa = "kappa_diet",
  #                     xlab = "Scaled Percent Insectivory",
  #                     maxColorValue = 100,
  #                     palette = colorRampPalette(c("black","black"))(maxColorValue),
  #                     plot_ylab = FALSE)
  # 
  # #Habitat selectivity
  # plot_linear_effects(load_from = lf_ch1m1,
  #                     variable_of_interest = "scale_habitat_ssi",
  #                     variable_kappa = "kappa_habitat_selection",
  #                     xlab = "Scaled Habitat Selectivity",
  #                     maxColorValue = 100,
  #                     palette = colorRampPalette(c("black","black"))(maxColorValue),
  #                     trendline_lty = "dashed",
  #                     plot_ylab = FALSE)
  # #Regional Trend
  # plot_linear_effects(load_from = lf_ch1m1,
  #                     variable_of_interest = "scale_usgs_trend",
  #                     variable_kappa = "kappa_regional",
  #                     xlab = "Scaled Regional Trend",
  #                     regional_trend_colors = TRUE,
  #                     plot_ylab = FALSE)
  # #should add a color bar legend
  # 
  # 
  # #Now, plot without regional trends.
  # #temp niche
  # plot_linear_effects(fit_summary = ch1noregionaleffects,
  #                     load_from = lf_ch1nR,
  #                     variable_of_interest = "scale_ztempwq",
  #                     variable_kappa = "kappa_temp_pos",
  #                     xlab = "Scaled Temperature Niche Position",
  #                     ylab_distance = new_ylab_distance)
  # 
  # #Percent Insectivory
  # plot_linear_effects(fit_summary = ch1noregionaleffects,
  #                     load_from = lf_ch1nR,
  #                     variable_of_interest = "scale_insect_perc",
  #                     variable_kappa = "kappa_diet",
  #                     xlab = "Scaled Percent Insectivory",
  #                     maxColorValue = 100,
  #                     palette = colorRampPalette(c("black","black"))(maxColorValue),
  #                     plot_ylab = FALSE)
  # 
  # #Habitat selectivity
  # plot_linear_effects(fit_summary = ch1noregionaleffects,
  #                     load_from = lf_ch1nR,
  #                     variable_of_interest = "scale_habitat_ssi",
  #                     variable_kappa = "kappa_habitat_selection",
  #                     xlab = "Scaled Habitat Selectivity",
  #                     maxColorValue = 100,
  #                     palette = colorRampPalette(c("black","black"))(maxColorValue),
  #                     trendline_lty = "dashed",
  #                     plot_ylab = FALSE)
  # }
  # 
  # dev.off()
  # 
  
  
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
  wo_model_color = "black"
  
  
  ###!!!!!!!!!!!!!!!!Make a new plotting function for plotting these effects. Have an option where it makes a new plot or not. rmNOBO models should be the main ones.
  png(filename = "figures/ch1/ch1_model_CI_comparison.png", 
      width = 440, # 620 for larger
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
               col = wo_model_color,
               lty = "dotted")
  }
  legend("right",
         bty = "n",
         legend = c("Full Model", "Model without \nRegional Trend\n"), 
         #fill = c(rt_model_color, wo_model_color
         lty = c("solid", "dotted"),
         lwd = c(4.5, 4.5)
         #scientific name is Colinus virginianus, if I want to change text to reflect that.
  )
  #}
  dev.off()
  
  #Supplemental version with all four models
  png(filename = "figures/ch1/ch1_model_CI_comparison_SUP.png", 
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
  
  
  
#------------------------------------
#
# Plot a map of the three counties and the routes. Should include a scale bar 
# in the final version.
#
#------------------------------------



  # NLCD data
  #nlcd <- rast("C:/Users/Ivara/Downloads/devo_1km_east.tif") #this is percent urbanization.
  nlcd <- rast("spatial/nlcd/Annual_NLCD_LndCov_2023_CU_C1V0.tif") #awesome, the 2023 works :)
  #we will want to resample this later probably to have fewer colors. Just water/urban/grassland/forest at maybe the 400m scale instead of 30m
  nlcd_classif <- read.csv("spatial/nlcd_classifications.csv", header = TRUE) %>% mutate(code = as.factor(code))

  # reclassify matrix
  #reclass_matrix <- matrix(c(
  #  # NLCD Code, Your Code
  #  11, 1,  # Water
  #  41, 2,  # Forest
  #  42, 2, 
  #  43, 2,
  #  71, 3,  # Grassland
  #  21, 4,  # Urban
  #  22, 4,
  #  23, 4,
  #  24, 4
  #), ncol = 2, byrow = TRUE)
  
  
  # NC couties:
  NC <- read_sf("spatial/shapefiles/NC_County_Polygons/North_Carolina_State_and_County_Boundary_Polygons.shp")
  counties <- read_sf("spatial/shapefiles/NC_County_Polygons/North_Carolina_State_and_County_Boundary_Polygons.shp") |>
    filter(County %in% c("Orange", "Durham", "Chatham")) |>
    st_transform(crs(nlcd))
  surrounding_counties <- read_sf("spatial/shapefiles/NC_County_Polygons/North_Carolina_State_and_County_Boundary_Polygons.shp") |>
    filter(County %in% c("Alamance", "Caswell", "Person", "Granville", "Wake", "Lee", "Moore", "Randolph")) |>
    st_transform(crs(nlcd))
  #counties_vect <- vect(counties)  # Convert sf to SpatVector for terra::mask
    
    # Verify CRS match (optional - for debugging)
    #cat("NLCD CRS:", crs(nlcd), "\n")
    #cat("Counties CRS:", st_crs(counties)$wkt, "\n")
  
    # Trim NLCD to county boundaries
    trimmed_nlcd <- nlcd |>
      crop(counties) |>
      mask(counties) 
    
    resampled_nlcd <- trimmed_nlcd |>
      aggregate(fact = 10, fun = "modal", na.rm = TRUE)
    # Aggregate from 30m to 300m using the modal (most frequent) value
    # fact = 10 because 300m / 30m = 10
    # eh, honestly it just looks worse aggregated. I think actually leave it at the 30x30 resolution
    # aggregate(fact = 10, fun = "modal", na.rm = TRUE)
    
  #colors for nlcd
  #colors <- read.csv("spatial/")
    
  # routes
  surveys <- read.csv("spatial/route_stop_coordinates.csv") |>
    st_as_sf(coords = c('lon', 'lat'), crs = 4269) |>
    st_transform(crs(nlcd)) # Transform surveys to match the NLCD's CRS) 
  unique_routes <- unique(surveys$route)  # Assuming column name is 'route'
  #get ggplot2 default hex color codes from 1 to 34
  colors <- hue_pal()(34)
  colors <- c("#F8766D",
              "#00BCD6",
              "#F07E4E",
              "#00C19D",
              "#00BE6E",
              "#00C087",
              "#DD8D00",
              "#45B500",
              "#B2A100",
              "#9FA700",
              "#E46DF5",
              "#F166E8",
              "#E7851E",
              "#FA62D9",
              "#9C8DFF",
              "#FF61C7",
              "#D277FF",
              "#00B3F2",
              "#00ABFC",
              "#6DB100",
              "#29A3FF",
              "#BA82FF",
              "#FD6F87",
              "#FF63B4",
              "#89AC00",
              "#D09400",
              "#00B927",
              "#7398FF",
              "#00C0B2",
              "#00BC51",
              "#00B8E5",
              "#FD6F86",
              "#00BFC4",
              "#FF689E")
  names(colors) <- unique_routes
  
  #North American Breeding Bird Survey routes
  bbs_routes <- read_sf("Z:/Databases/BBS/GPS_stoplocations/bbsrte_2012_alb/") |>
    #put in same transformation as everything else
    st_transform(crs(NC)) |>
    #crop to NC
    st_crop(NC) |>
    st_transform(crs(nlcd))
  plot(st_geometry(bbs_routes))
  
  #set all background to transparent
  par(bg = NA)  # or par(bg = "transparent")
  
  
  {
  plot(
    #resampled_nlcd, 
    resampled_nlcd,
       axes = FALSE,
       bg = "transparent") #maybe just re-code all water as blue.
  plot(st_geometry(counties), add = TRUE, border = "black", lwd = 2)
  plot(st_geometry(surrounding_counties), add = TRUE, border = "black", lwd = 1)
  #add a little transparency overtop
  #plot(st_geometry(counties), add = TRUE, col = adjustcolor("white", alpha = ".1"))
  
  #add the NABBS routes
  plot(st_geometry(bbs_routes), add = TRUE, col = "white", lwd = 5)
  plot(st_geometry(bbs_routes), add = TRUE, 
       col = "purple",
       lwd = 3)
  
  #add the NCMBBS routes
  plot(st_geometry(surveys), add = TRUE, col = "black", pch = 16, cex = 1)
  plot(st_geometry(surveys), add = TRUE, col = "white", pch = 16, cex = .8)
  plot(st_geometry(surveys), add = TRUE, col = "black", pch = 16, cex = .6)
  
  }
  
  {
  plot(x = 1, y = 1, xlim = c(1,10), ylim = c(1,10))
  legend("center",
         legend = c("developed"),
         fill = c("red"),
         bty = "n")
  }
  
  # Add each route with different color
  #for(route in unique_routes) {
  #  route_points <- surveys[surveys$route == route, ]
  #  plot(st_geometry(route_points), 
  #       add = TRUE, 
  #       col = colors[route], 
  #       pch = 16, 
  #      cex = 0.6)
  #}
   #needs a scale bar
  #and to remove the legends
  #but yay! moving towards a map ^u^
  
  #inset
  plot(st_geometry(NC), border = "grey20") 
  plot(st_geometry(counties |> st_transform(crs(NC))), add = TRUE, col = "#68AA63")
  dev.off()
  