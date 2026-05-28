#########################################
#
# Functions developed for plotting
#
#
#########################################

#######Bayesplot helper functions

  #separate out into two dfs, pivot both back to assumed structure of columns with 4000 rows bayesplot wants
  seperate_betas_pivot <- function(posterior_samples, column_select_list, values_from_column) {
    x <- posterior_samples %>%
      dplyr::select(all_of(column_select_list)) %>%
      tidyr::pivot_wider(names_from = common_name, 
                         values_from = {{values_from_column}}) %>%
      dplyr::select(-row_id)
  }  
  
  #sort columns parameter means
  return_sorted_params <- function(pivoted_samples) {
    param_means <- colMeans(pivoted_samples)
    sorted_params <- names(sort(param_means))
    sorted_pivoted_samples <- pivoted_samples[,sorted_params]
    
  }
##############
  
# plot all intervals
  plot_intervals <- function(plot_df,
                             xlab = "",
                             ylab = "",
                             species_axis = "y",
                             first_overlay = NA, 
                             second_overlay = NA, 
                             xlim_select = c(-0.4, 0.2), 
                             ylim_select = c(1, 66), 
                             title = NA, 
                             xaxt = "s", 
                             yaxt = "s") {
  if(species_axis == "y") { #species ID on the y axis
    plot(y = plot_df$sp_id,
         x = plot_df$mean,
         xlim = xlim_select,
         col = plot_df$color,
         ylim = ylim_select,
         yaxt = "n",
         xlab = xlab,
         ylab = ylab,
         pch = 16, 
         cex = 2,
         yaxs = "i",
         main = title,
         xaxt = xaxt
    ) 
      segments(x0 = plot_df$conf_2.5,
               x1 = plot_df$conf_97.5,
               y0 = plot_df$sp_id,
               col = plot_df$color,
               lwd = 5) 
      abline(v = 0, lty = "dashed") 
      axis(2, at = seq(round(min(plot_df$sp_id)),
                       round(max(plot_df$sp_id)), by = 1),
           labels = plot_df$common_name,
           las = 1,
           cex.axis = 1.25)
    
    if(any(is.na(first_overlay)) == FALSE) {
      segments(x0 = first_overlay$conf_2.5,
               x1 = first_overlay$conf_97.5,
               y0 = first_overlay$sp_id,
               col = first_overlay$color)
      points(x = first_overlay$mean,
             y = first_overlay$sp_id,
             col = first_overlay$color)
    }
    
    if(any(is.na(first_overlay)) == FALSE) {
      segments(x0 = second_overlay$conf_2.5,
               x1 = second_overlay$conf_97.5,
               y0 = second_overlay$sp_id,
               col = second_overlay$color)
      points(x = second_overlay$mean,
             y = second_overlay$sp_id,
             col = second_overlay$color)
    }
  } else if (species_axis == "x") { #species ID on the x axis
    plot(y = plot_df$mean,
         x = plot_df$sp_id,
         xlim = xlim_select,
         col = plot_df$color,
         ylim = ylim_select,
         xaxt = "n",
         xlab = xlab,
         ylab = ylab,
         pch = 16, 
         cex = 2,
         xaxs = "i",
         main = title,
         yaxt = yaxt
    ) 
      abline(h = c(seq(-.14,0.08, by = .02)),
             col = "grey",
             lty = "dashed") 
      abline(h = 0, lty = "dashed") 
      points(y = plot_df$mean,
             x = plot_df$sp_id,
             col = plot_df$color)
      segments(y0 = plot_df$conf_2.5,
               y1 = plot_df$conf_97.5,
               x0 = plot_df$sp_id,
               x1 = plot_df$sp_id,
               col = plot_df$color,
               lwd = 5) 
                #plot a slightly wider border around any white segments
                border_segments <- plot_df |>
                  filter(color == "white")
                segments(y0 = border_segments$conf_2.5,
                         y1 = border_segments$conf_97.5,
                         x0 = border_segments$sp_id,
                         col = "black",
                         lwd = 5.5)
                segments(y0 = border_segments$conf_2.5,
                         y1 = border_segments$conf_97.5,
                         x0= border_segments$sp_id,
                         col = border_segments$color,
                         lwd = 4.5)
      axis(1, at = seq(round(min(plot_df$sp_id)),
                       round(max(plot_df$sp_id)), by = 1),
           #labels = plot_df$common_name,
           labels = FALSE,
           las = 2,
           cex.axis = 1.25) 
        text(x = seq(round(min(plot_df$sp_id)),
                     round(max(plot_df$sp_id)), by = 1),
             y = par("usr")[3] - 0.05 * diff(par("usr")[3:4]),  # Position below x-axis
             labels = plot_df$common_name,
             srt = 50,  # 45-degree rotation
             adj = 1,   # Adjust alignment (1 = right-aligned)
             xpd = TRUE,  # Allow plotting outside plot region
             cex = 1.25,
             col = plot_df$color)
  }
  }
  
  
# plot only significant results, plots with segments
  plot_only_sig <- function(plot_df, xlim_sig = c(-0.4, 0.2), point_cex = 2, segment_lwd = 6){
    
    assertthat::assert_that(any(is.na(plot_df$significant)),
                            msg = "Provided plot_df does not have $significant column")
    assertthat::assert_that(any(is.na(plot_df$common_name)),
                            msg = "At least one $common_name is NA")
    
    #confirm we only have significant results
    plot_df <- plot_df %>%
      dplyr::filter(significant == TRUE)
    
    plot(y = plot_df$sp_id,
         x = plot_df$mean,
         xlim = xlim_sig,
         col = plot_df$color,
         yaxt = "n",
         xlab = "sig only dev",
         ylab = "",
         pch = 16,
         cex = point_cex
    ) +
      segments(x0 = plot_df$X2.50.,
               x1 = plot_df$X97.50.,
               y0 = plot_df$sp_id,
               col = plot_df$color,
               lwd = segment_lwd) +
      abline(v = 0, lty = "dashed") + 
      axis(2, at = seq(round(min(plot_df$sp_id)),
                       round(max(plot_df$sp_id)), by = 1),
           labels = plot_df$common_name,
           las = 1,
           cex.axis = 1)
  }

  
########### plot bayesian linear effects on top of a scatterplot
  plot_linear_effects <- function(
    fit_summary = ch1lineareffects,
    load_from,
    new_plot = TRUE,
    variable_of_interest,
    variable_kappa,
    xaxt = "s",
    abline_at_zero = FALSE,
    ylim = c(-.1, 0.065), #with bobwhite, ylim = c(-.15, 0.07)
    xlab = "",
    ylab = "Local Population Trend",
    plot_ylab = TRUE,
    ylab_distance = - 1.2,
    maxColorValue = 100,
    palette = colorRampPalette(c("blue","red"))(maxColorValue),
    regional_trend_colors = FALSE,
    rt_colors_variable = "rt_colors",
    secondary_tck = -0.02,
    trendline_lty = "solid",
    white_lwd = 2.5,
    lwd = 2,
    outer_bound_lwd = 1.5,
    plot_slope = TRUE,
    polygon_color = "steelblue",
    unscale_regional_trend = FALSE,
    all_rt_sd = NA,
    all_rt_mean = NA,
    base_rt_kappa = NA,
    rm_species = NA
  ) {
    
    posterior_draws = read.csv(paste0(load_from, "posterior_draws.csv"))
    
    #scatterplot the variable of interest
    if(new_plot == TRUE) {
    plot(x = fit_summary[,variable_of_interest],
         y = fit_summary$mean,
         yaxt = "n",
         xaxt = xaxt,
         ylim = ylim,
         col = palette[cut(fit_summary[,variable_of_interest], maxColorValue)],
         pch = 16,
         cex = 2,
         xlab = xlab,
         ylab = "",
         )
    } 
    #plot ylab or not
    if(plot_ylab == TRUE) {
      text(x = par("usr")[1] + ylab_distance,  # Position left of plot region
           y = mean(par("usr")[3:4]), # Center vertically within plot region
           labels = "Population Trend",
           srt = 90,                  # Rotate 90 degrees for vertical orientation
           xpd = NA,                  # Allow plotting outside plot region
           cex = 1.6) 
    }
    #abline at 0
    if(abline_at_zero == TRUE) {
    abline(h = 0, lty = "dashed")
    }
    #add segments for uncertainty in population trends
    segments(x0 = fit_summary[,variable_of_interest],
             y0 = fit_summary$conf_2.5,
             y1 = fit_summary$conf_97.5,
             lwd = 3, 
             col = palette[cut(fit_summary[,variable_of_interest], maxColorValue)]
             )
    #re-add the axes
    #y
    axis(2,          
         at = c(-.14, -.12, -.10, -.08, -.06, -.04, -.02, 0, .02, .04, .06, .08),
         labels = c("-0.14", "-0.12", "-0.10", "-0.08", "-0.06", "-0.04", 
                    "-0.02", "0", "0.02", "0.04", "0.06", "0.08"),
         las = 2,
         #cex.axis = 1.25
         ) 
    #x
    #axis(1,
    #     at = c(-3, -2, -1, 0, 1, 2, 3),
    #     labels = TRUE)
    #axis(1, 
    #     at = c(-3.5, -2.5, -1.5, -0.5, .5, 1.5, 2.5, 3.5),
    #     labels = FALSE,
    #     tck = secondary_tck)
    
    #if we're doing the regional trend, then:
    #reprint the dots and segments using coded regional trend colors.
    if(regional_trend_colors == TRUE){
      #population trend segments
      segments(x0 = fit_summary[,variable_of_interest],
               y0 = fit_summary$conf_2.5,
               y1 = fit_summary$conf_97.5,
               lwd = 3, 
               col = fit_summary[,rt_colors_variable]
      )
      #regional trend segments - this means I have to calculate like the 97.5 and 2.5 intervals again from the scale_usgs_trend and scale_usgs_sd
      #segments(x0 = fit_summary[,"usgs_2.5CI"],
      #         x1 = fit_summary[,"usgs_97.5CI"],
      #         y0 = fit_summary$mean,
      #         col = fit_summary$rt_colors,
      #         lwd = 3)
      #points
      points(x = fit_summary[,variable_of_interest],
             y = fit_summary$mean,
             col = fit_summary[,rt_colors_variable],
             pch = 16,
             cex = 2)
      
      #and then reprint the disagreements so they come out on top
      disagreements <- fit_summary |>
        filter(trend_agreement == "disagree")
      segments(x0 = disagreements[,variable_of_interest],
               y0 = disagreements$conf_2.5,
               y1 = disagreements$conf_97.5,
               lwd = 3, 
               col = disagreements[,rt_colors_variable]
      )
      points(x = disagreements[,variable_of_interest],
             y = disagreements$mean,
             col = disagreements[,rt_colors_variable],
             pch = 16,
             cex = 2)
    }
    
    #create a sequence of the variable of interest that we can then use to predict what the betas would be, this will give us the main slope line
    voi_sequence <- seq(min(fit_summary[,variable_of_interest], na.rm = TRUE),
                        max(fit_summary[,variable_of_interest], na.rm = TRUE),
                        length.out = 100)
    
    #get posteriors for the kappa we want
    post_kappa <- posterior_draws[,variable_kappa]
    post_average_beta <- posterior_draws[,"gamma_b"]
    
    if(unscale_regional_trend == TRUE) {
      #transform post_kappas
      post_kappa <- post_kappa / all_rt_sd
      #transform post_average_betas
      post_average_beta <- post_average_beta - (base_rt_kappa * (all_rt_mean/all_rt_sd))
    }
    
    #predict the population trend using the posterior kappas and betas for the entire sequence of the variable of interest
    predicted_results <- sapply(
      voi_sequence,
      function(voi_sequence) {
        post_average_beta +
          post_kappa * voi_sequence
        #all the variables are scaled so their means are 0. This means this formula for estimating betas ONLY needs the variable of interest. Holding things steady for the other variables at the mean, they are =0
      }
    )
    
    predicted_mean_effect <- apply(predicted_results, 2, mean)
    predicted_voi_ci <- apply(predicted_results, 2, quantile, probs = c(0.025, 0.975))
    
    predicted_df <- data.frame(variable_of_interest = voi_sequence,
                               mean = predicted_mean_effect,
                               lower = predicted_voi_ci[1,],
                               upper = predicted_voi_ci[2,])
    
    #plot the mean line, first in white then overtop in black (for contrast)
    if(plot_slope == TRUE) {
    lines(x = predicted_df$variable_of_interest,
          y = predicted_df$mean,
          lwd = white_lwd, 
          col = "white",
          lty = trendline_lty)
    lines(x = predicted_df$variable_of_interest,
          y = predicted_df$mean,
          lwd = lwd,
          lty = trendline_lty)
    #outer confidence bounds
    lines(x = predicted_df$variable_of_interest,
          y = predicted_df$lower,
          lwd = white_lwd,
          col = "white")
    lines(x = predicted_df$variable_of_interest,
          y = predicted_df$lower,
          lwd = outer_bound_lwd)
    lines(x = predicted_df$variable_of_interest,
          y = predicted_df$upper,
          lwd = white_lwd,
          col = "white")
    lines(x = predicted_df$variable_of_interest,
          y = predicted_df$upper,
          lwd = outer_bound_lwd)
    
    #fill
    polygon(c(predicted_df$variable_of_interest, rev(predicted_df$variable_of_interest)), 
            c(predicted_df$lower, rev(predicted_df$upper)), 
            col = adjustcolor(polygon_color, alpha.f = 0.3), 
            border = NA)
    }
  }
  
#######################Add an outlier species to plot in outlier color
  add_outlier <- function(df,
                          variable_of_interest,
                          rm_species_list = c("Northern Bobwhite"),
                          outlier_color = "orange",
                          add_legend = TRUE) {
    #filter to just the species you want to remove
    rm_species <- df |>
      filter(common_name %in% rm_species_list)
    
    #plot
    segments(x0 = rm_species[,variable_of_interest],
             y0 = rm_species$conf_2.5,
             y1 = rm_species$conf_97.5,
             lwd = 3, 
             col = outlier_color
    )
    points(x = rm_species[,variable_of_interest],
           y = rm_species$mean,
           cex = 2, 
           pch = 16,
           col = outlier_color)
    
    #add legend
    if(add_legend == TRUE) {
    legend("bottom",
           legend = c("Removed Outlier"),
           fill = c(outlier_color),
           bty = "n")
    }
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  