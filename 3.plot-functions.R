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
  if(species_axis == "y") {
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
  } else if (species_axis == "x") {
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
    ) +
      abline(h = c(seq(-.14,0.08, by = .02)),
             col = "grey",
             lty = "dashed") +
      abline(h = 0, lty = "dashed") + 
      points(y = plot_df$mean,
             x = plot_df$sp_id,
             col = plot_df$color)
      segments(y0 = plot_df$conf_2.5,
               y1 = plot_df$conf_97.5,
               x0 = plot_df$sp_id,
               x1 = plot_df$sp_id,
               col = plot_df$color,
               lwd = 5) +
      axis(1, at = seq(round(min(plot_df$sp_id)),
                       round(max(plot_df$sp_id)), by = 1),
           labels = plot_df$common_name,
           las = 2,
           cex.axis = 1.25)
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
  