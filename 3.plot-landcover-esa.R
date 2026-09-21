library(dplyr)
library(stringr)
library(ggplot2)
library(bayesplot)
library(cowplot) #used to make multi-panel figures
library(reshape2) #for the correlation matrix heatmap
library(vioplot) #for violin plots
#load functions
source("3.plot-functions.R")

#load-from:
load_from_full_lag <- "model/ch2/2026.09.21.full-lag-rm0spqrts-uaiforest/"

load_from_short_term <- "model/ch2/2026.09.21.three-year-rm0spqrts/"

#dev-long-term 
landcover <- c("dev+barren", "forest_positive", "forest_negative")
for(a in 1:2) {
  
  if(a == 1) {
    load_from = load_from_full_lag
    run_type = "full_lag"
  } else if (a == 2) {
    load_from = load_from_short_term
    run_type = "3yr"
  }
  
  for (i in 1:length(landcover)) {
  maxColorValue = 101
  color = colorRampPalette(c("black", "grey80", "#92c5de", "#a6dba0", "#5aae61"))(maxColorValue)
  continous_colors <- data.frame(color, 
                                 palette_percent = seq(from = 0, to = 1, length.out = 101)) |>
    mutate(palette_percent = round(palette_percent, digits = 2))
  if(landcover[i] == "forest_negative") {
    continous_colors <- data.frame(color, 
                                   palette_percent = seq(from = 1, to = 0, length.out = 101)) |>
      mutate(palette_percent = round(palette_percent, digits = 2))
  }
  
  
  #from 5,000 posterior draws..
  #prop_posterior <- read.csv(paste0(load_from_full_lag, "dev+barren_posterior_samples.csv")) |>
  #  select(starts_with("b_landcover_change")) |>
  #  summarize(across(everything(),
  #                   (prop_gt_0 = ~sum(. > 0)/5000))) |>
  #  tidyr::pivot_longer(cols = b_landcover_change.1.:b_landcover_change.67., 
  #                      names_to = "variable") |>
  #  dplyr::select(variable, value) |>
  #  rename(prop_posterior_gt_0 = value) |>
  #  mutate(sp_id = as.integer(str_extract(variable, "[0-9]([0-9])?"))) |>
  #  dplyr::select(-variable) |>
  #  left_join(read.csv(paste0(load_from_full_lag, "traits.csv")), by = 'sp_id') |>
  #  mutate(palette_percent = round(prop_posterior_gt_0, digits = 2)) |>
  #  left_join(continous_colors, by = c("palette_percent")) |>
  #  dplyr::select(-sp_id)
  
  prop_posterior <- read.csv(paste0(load_from, landcover[i], "_prop_posterior_gt0.csv")) |>
    filter(kappa == "_landcover") |>
    rename(sp_id = common_name_standard)  |>
      left_join(read.csv(paste0(load_from, "traits.csv")), by = 'sp_id') |>
      mutate(palette_percent = round(prop_posterior_gt_0, digits = 2)) |>
      left_join(continous_colors, by = c("palette_percent")) 
  
  
  #z <- qnorm((1+0.87)/2) #confidence interval 87%
  
  full_lag_dev <- read.csv(paste0(load_from, landcover[i], "_fit_summaries.csv")) %>%
    #filter out the individual quarter route a[] fits, just keep the variables we're most interested in.
    # filter(rownames %in% c("a_bar", "sig_a", "b_landcover_change", "b_landcover_base", "c_obs", "sigma")) %>%
    #and really.. all we want is b_landcover_change here in plotting
    # filter(rownames %in% c("b_landcover_change")) %>%
    filter(!is.na(slope)) %>%
    left_join(prop_posterior, by = c("sp_id")) |>
    group_by(mean, sp_id) %>%
    arrange(desc(mean)) %>% 
    #redo sp_id to rank by mean
    mutate(sp_id = cur_group_rows()) %>% #sweet, indigio bunting with the most negative mean effect is at ID 66, Carolina Wren with the least negative effect is at ID 1. 
    ungroup() |>
    mutate(pch = 19)
  
  if(landcover[i] == "forest_negative") {
    # we want to reverse the plotting order b/c we still want species that are declining to be plotted on the other side.
    full_lag_dev <- full_lag_dev |>
      group_by(mean, sp_id) |>
      arrange(mean) |>
      mutate(sp_id = cur_group_rows())
  }
  
  #set plotting label
  if(landcover[i] == "dev+barren"){
    lab = "Change in species count with change in % urban"
  } else if (landcover[i] == "forest_negative") {
    lab = "Change in species count with forest loss"
  } else if(landcover[i] == "forest_positive") {
    lab = "Change in species count with forest gain"
  }
  
  png(filename = paste0("figures/ch2/BOU_", run_type, "_", landcover[i], ".png"), 
      width = 1200,
      height = 600,
      units = "px", 
      type = "windows")
  par(mar = c(4, 16, 1, 1), cex.axis = 1, mfrow = c(1,2))
  plot_intervals(plot_df = full_lag_dev[34:63,],
                 xlab = lab, 
                 ylim_select = c(33.5,63.5),
                 xlim_select = c(-8, 8.5),
                 xaxt = "n")
  plot_intervals(plot_df = full_lag_dev[1:33,], 
                 xlab = "", 
                 ylim_select = c(.5,33.5),
                 xlim_select = c(-8, 8.5),
                 xaxt = "n")
  dev.off()
  }
}

#dev short-term
{
  maxColorValue = 100
  color = colorRampPalette(c("black", "grey80", "#92c5de", "#a6dba0", "#5aae61"))(maxColorValue)
  continous_colors <- data.frame(color, 
                                 palette_percent = seq(from = 0, to = 1, length.out = 100)) |>
    mutate(palette_percent = round(palette_percent, digits = 2))
  
  
  #from 5,000 posterior draws..
  prop_posterior <- read.csv(paste0(load_from_short_term, "dev+barren_posterior_samples.csv")) |>
    select(starts_with("b_landcover_change")) |>
    summarize(across(everything(),
                     (prop_gt_0 = ~sum(. > 0)/24000))) |>
    tidyr::pivot_longer(cols = b_landcover_change.1.:b_landcover_change.67., 
                        names_to = "variable") |>
    dplyr::select(variable, value) |>
    rename(prop_posterior_gt_0 = value) |>
    mutate(sp_id = as.integer(str_extract(variable, "[0-9]([0-9])?"))) |>
    dplyr::select(-variable) |>
    left_join(read.csv(paste0(load_from_full_lag, "traits.csv")), by = 'sp_id') |>
    mutate(palette_percent = round(prop_posterior_gt_0, digits = 2)) |>
    left_join(continous_colors, by = c("palette_percent")) |>
    dplyr::select(-sp_id)
  
  
  z <- qnorm((1+0.87)/2) #confidence interval 87%
  
  short_lag_dev <- read.csv(paste0(load_from_short_term, "dev+barren_fit_summaries.csv")) %>%
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
    left_join(prop_posterior, by = c("common_name")) |>
    mutate(significant = ifelse(conf_2.5 < 0 & conf_97.5 > 0, FALSE, TRUE),
           pch = ifelse(palette_percent < .07, 19, 1)) 
  
  png(filename = "figures/ch2/esa_short_lag_dev.png", 
      width = 1200,
      height = 600,
      units = "px", 
      type = "windows")
  par(mar = c(4, 16, 1, 1), cex.axis = 1, mfrow = c(1,2))
  plot_intervals(plot_df = short_lag_dev[34:67,],
                 xlab = "Change in species count with change % urban", 
                 ylim_select = c(33.5,66.5),
                 xlim_select = c(-.06, .065))
  plot_intervals(plot_df = short_lag_dev[1:33,], 
                 xlab = "", 
                 ylim_select = c(.5,33.5),
                 xlim_select = c(-.06, .065))
  dev.off()
}


png(filename = "figures/ch2/dev_LEGEND_continous.png", 
    width = 900,
    height = 800,
    units = "px", 
    type = "windows") 
colors <- color
legend_image <- as.raster(matrix(colors, ncol = 1))
plot(c(1,10), c(1,10))
rasterImage(legend_image, xleft = 4, ybottom = 2, xright = 5, ytop = 8)
#text("okay",
#     x= 5.5,
#     y = 8.2)
dev.off()

#plot the effects of uai and landcover change

le <- read.csv(paste0(load_from_full_lag, "dev+barren_fit_summaries.csv")) |>
  filter(str_detect(rownames, "kappa")) |>
  mutate(model = "full_lag",
         color = "black",
         rownames = case_when(
         rownames == "kappa_forest" ~ "Forest\n Association\n (20+ years)",
         rownames == "kappa_uai" ~ "Urban\n Association\n (20+ years)")) |>
  mutate(id = row_number())

le2 <- read.csv(paste0(load_from_short_term, "dev+barrentraits_fit_summaries.csv")) |>
  filter(str_detect(rownames, "b")) |>
  mutate(model = "one_year_lag",
         color = "grey") |>
  distinct() |>
  mutate(rownames = case_when(
    rownames == "b_forest" ~ "Forest\n Association\n (1-year)",
    rownames == "b_uai" ~ "Urban\n Association\n (1-year)"
  ),
  mean = mean*100,
  conf_2.5 = conf_2.5*100,
  conf_97.5 = conf_97.5*100) |>
  mutate(id = row_number())

ledf <- bind_rows(le, le2) |>
  mutate(id = row_number())

png(filename = "figures/ch2/full_lag_le.png", 
    width = 440, # 620 for larger
    height = 440, # 640 for larger
    units = "px", 
    type = "windows")
#honestly, might want to think about increasing the width and height and text sizes.
{
  par(mar = c(4, 10, 2, 1),
      cex = 1.1) # 1.5 for larger
  plot(x = le$mean,
       y = le$id, 
       pch = le$pch,
       cex = 2,
       xlim = c(-2.5,2),
       xlab = "Effect Size",
       xaxt = "s",
       yaxt = "n",
       ylab = "",
       col = color,
       ylim = c(.5, 2.5)) 
  abline(v = 0, lty = "dashed") 
  #axis(side = 1, at = seq(-0.04, 0.04, by = 0.01), 
  #     labels = TRUE) 
  segments(x0 = le$conf_2.5,
           x1 = le$conf_97.5,
           y0 = le$id,
           lwd = 4.5,
           col = color) 
  axis(side = 2,           # Side 2 is the left side (y-axis)
       at = le$id,     # Specify the locations of the tick marks
       labels =  le$rownames, # Specify the labels for those locations
       las = 2
  ) 
}
dev.off()


png(filename = "figures/ch2/short_lag_le.png", 
    width = 440, # 620 for larger
    height = 440, # 640 for larger
    units = "px", 
    type = "windows")
#honestly, might want to think about increasing the width and height and text sizes.
{
  par(mar = c(4, 10, 2, 1),
      cex = 1.1) # 1.5 for larger
  plot(x = le2$mean,
       y = le2$id, 
       pch = le2$pch,
       cex = 2,
       xlim = c(-2.5,2),
       xlab = "Effect Size",
       xaxt = "s",
       yaxt = "n",
       ylab = "",
       col = color,
       ylim = c(.5, 2.5)) 
  abline(v = 0, lty = "dashed") 
  #axis(side = 1, at = seq(-0.04, 0.04, by = 0.01), 
  #     labels = TRUE) 
  segments(x0 = le2$conf_2.5,
           x1 = le2$conf_97.5,
           y0 = le2$id,
           lwd = 4.5,
           col = color) 
  axis(side = 2,           # Side 2 is the left side (y-axis)
       at = le2$id,     # Specify the locations of the tick marks
       labels =  le2$rownames, # Specify the labels for those locations
       las = 2
  ) 
}
dev.off()



