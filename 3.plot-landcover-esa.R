## actually now plots for BOU plots

library(dplyr)
library(stringr)
library(ggplot2)
library(bayesplot)
library(cowplot) #used to make multi-panel figures
library(reshape2) #for the correlation matrix heatmap
#load functions
source("3.plot-functions.R")

plot_landcover_results <- function(load_from = "NA",
                                   save_to = load_from,
                                   run_type_title = "3yr", 
                                   landcover = c("dev+barren", "forest_positive", "forest_negative", "grassland_positive", "grassland_negative")) {
  
  for (i in 1:length(landcover)) {
    maxColorValue = 101
    color = colorRampPalette(c("black", "grey80", "#92c5de", "#a6dba0", "#5aae61"))(maxColorValue)
    continous_colors <- data.frame(color, 
                                   palette_percent = seq(from = 0, to = 1, length.out = 101)) |>
      mutate(palette_percent = base::round(palette_percent, digits = 2))
    #for re-arranging the ordering of forest_negative results. Right now, not in use.
    #if(landcover[i] == "forest_negative") {
    #  continous_colors <- data.frame(color, 
    #                                 palette_percent = seq(from = 1, to = 0, length.out = 101)) |>
    #    mutate(palette_percent = base::round(palette_percent, digits = 2))
    #}
    
    #read in the prop_posterior
    prop_posterior <- read.csv(paste0(load_from, landcover[i], "_prop_posterior_gt0.csv")) |>
      filter(kappa == "_landcover") |>
      rename(sp_id = common_name_standard)  |>
      left_join(read.csv(paste0(load_from, "traits.csv")), by = 'sp_id') |>
      mutate(palette_percent = round(prop_posterior_gt_0, digits = 2)) |>
      left_join(continous_colors, by = c("palette_percent")) 
    
    fit_summary <- read.csv(paste0(load_from, landcover[i], "_fit_summaries.csv")) %>%
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
      #here's where you can change the shape to be different based on significance if so desired
      mutate(pch = 19)
    
    #variables we'll need for plotting to stay standard if n_sp changes.
    n_sp <- n_distinct(fit_summary$sp_id)
    half_sp <- round(n_sp/2)
    
    # Alternate plotting for forest_negative, removed for rn.
    #if(landcover[i] == "forest_negative") {
    #  # we want to reverse the plotting order b/c we still want species that are declining to be plotted on the other side.
    #  fit_summary <- fit_summary |>
    #    group_by(mean, sp_id) |>
    #    arrange(mean) |>
    #    mutate(sp_id = cur_group_rows())
    #}
    
    #set plotting label
    if(landcover[i] == "dev+barren"){
      lab = "Change in species count with change in % urban"
    } else if (landcover[i] == "forest_negative") {
      lab = "Change in species count with forest loss"
    } else if(landcover[i] == "forest_positive") {
      lab = "Change in species count with forest gain"
    } else if(landcover[i] == "grassland_negative") {
      lab = "Change in species count with grassland loss"
    } else if(landcover[i] == "grassland_positive") {
      lab = "Change in species count with grassland gain"
    }
    
    png(filename = paste0(save_to, run_type_title, "_", landcover[i], ".png"), 
        width = 1200,
        height = 600,
        units = "px", 
        type = "windows")
    par(mar = c(4, 16, 1, 1), cex.axis = 1, mfrow = c(1,2))
    plot_intervals(plot_df = fit_summary[half_sp:n_sp,],
                   xlab = lab, 
                   ylim_select = c(half_sp + .5, n_sp + .5),
                   xlim_select = c(-2, 2.5),
                   xaxt = "n")
    plot_intervals(plot_df = fit_summary[1:half_sp,], 
                   xlab = "", 
                   ylim_select = c(.5,half_sp + .5),
                   xlim_select = c(-2, 2.5),
                   xaxt = "n")
    dev.off()
    
    
    #plot linear kappa effects
    le <- read.csv(paste0(load_from, landcover[i], "_fit_summaries.csv")) |>
      filter(str_detect(rownames, "kappa")) |>
      mutate(model = run_type_title,
             color = "black") |>
      distinct() |>
      mutate(rownames = case_when(
        rownames == "kappa_forest" ~ paste0("Forest\n Association\n", run_type_title),
        rownames == "kappa_uai" ~ paste0("Urban\n Association\n", run_type_title),
        rownames == "kappa_grassland" ~ paste0("Grassland\n Association\n", run_type_title)
      )
      ) 
    if(nrow(le) == 2) {
      le <- le |>
        mutate(id = c(2,1)) |>
        arrange(id)
    } else if (nrow(le) == 3) {
      le <- le |>
        mutate(id = c(3,2,1)) |>
        arrange(id)
    }

    
    png(filename = paste0(save_to, landcover[i], "_linear_effects.png"), 
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
           col = le$color,
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
    
  } #end landcover loop
  
  #plot a scale legend.
  png(filename = paste0(save_to, "plot_LEGEND_continous.png"), 
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
  
} #end plot_landcover_results function.


  

  
  
  # if(a == 1 & landcover[i] == "dev+barren") {
  #   
  #   color = colorRampPalette(c("red", "blue"))(maxColorValue)
  #   continous_colors <- data.frame(color, 
  #                                  palette_percent = seq(from = -2.8, to = 2.8, length.out = 560)) |>
  #     mutate(palette_percent = round(palette_percent, digits = 2))
  #   
  #   fit_summary$palette_percent <- round(fit_summary$scale_UAI, digits = 1)
  #   
  #   fit_summary <- fit_summary |>
  #     left_join(continous_colors,
  #              by = c("palette_percent"))
  # 
  #   plot(x = fit_summary$scale_UAI,
  #        y = fit_summary$mean,
  #        ylim = c(-6, 6),
  #        pch = 16,
  #        col = fit_summary$color.y)
  #   segments(y0 = fit_summary$conf_2.5,
  #            y1 = fit_summary$conf_97.5,
  #            x0 = fit_summary$scale_UAI,
  #            x1 = fit_summary$scale_UAI,
  #            lwd = 2,
  #            col = fit_summary$color.y)
  # }

#plot the effects of uai and landcover change




