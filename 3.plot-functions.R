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
    ) +
      segments(x0 = plot_df$conf_2.5,
               x1 = plot_df$conf_97.5,
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
    posterior_draws = read.csv(paste0(lf_ch1m1, "posterior_draws.csv")),
    variable_of_interest,
    variable_kappa,
    abline_at_zero = FALSE,
    ylim = c(-.14, 0.07),
    xlab = "",
    ylab = "Population Trend",
    maxColorValue = 100,
    palette = colorRampPalette(c("blue","red"))(maxColorValue),
    white_lwd = 2.5,
    lwd = 2,
    outer_bound_lwd = 1.5,
    polygon_color = "steelblue"
  ) {
    
    #scatterplot the variable of interest
    plot(x = fit_summary[,variable_of_interest],
         y = fit_summary$mean,
         ylim = ylim,
         col = palette[cut(fit_summary[,variable_of_interest], maxColorValue)],
         pch = 16,
         xlab = xlab,
         ylab = ylab
         )
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
    
    #create a sequence of the variable of interest that we can then use to predict what the betas would be, this will give us the main slope line
    voi_sequence <- seq(min(fit_summary[,variable_of_interest], na.rm = TRUE),
                        max(fit_summary[,variable_of_interest], na.rm = TRUE),
                        length.out = 100)
    
    #get posteriors for the kappa we want
    post_kappa <- posterior_draws[,variable_kappa]
    post_average_beta <- posterior_draws[,"gamma_b"]
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
    lines(x = predicted_df$variable_of_interest,
          y = predicted_df$mean,
          lwd = white_lwd, 
          col = "white")
    lines(x = predicted_df$variable_of_interest,
          y = predicted_df$mean,
          lwd = lwd)
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  