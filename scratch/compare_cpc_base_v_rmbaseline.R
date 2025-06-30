######################
#
# Compare the confidence intervals on results
# where we had change count ~ change landscape + base landscape
# vs only
# change count ~ change landscape
#
######################

wb <- read.csv("Z:/Goulden/mbbs-analysis/model_landcover/2025.06.26_cpc_forestndev_pm_obs/dev+barren_fit_summaries.csv") %>%
  filter(rownames %in% c("b_landcover_change")) %>%
  group_by(mean, common_name) %>%
  arrange(desc(mean)) %>% #
  mutate(sp_id = cur_group_rows()) %>%
  ungroup() %>%
  arrange(sp_id)

sp_ids <- wb %>%
  dplyr::select(common_name, sp_id)

wob <- read.csv("Z:/Goulden/mbbs-analysis/model_landcover/2025.06.26_cpc_rmbaseline_obsqual/dev+barren_fit_summaries.csv") %>%
  filter(rownames %in% c("b_landcover_change")) %>%
  left_join(sp_ids, by = "common_name") %>%
  arrange(sp_id)

leftover_lower_bound <- wb$X2.50. - wob$X2.50. #k really these are almost exactly the same
leftover_upper_bound <- wb$X97.50. - wob$X97.50. #k really these are almost exactly the same

  plot_df <- wb
  first_overlay <- wob

plot(y = plot_df$sp_id,
     x = plot_df$mean,
     xlim = c(-0.6, 0.4),
     col = "black",
     yaxt = "n",
     xlab = "W baseline in black, WO baseline in red",
     ylab = "",
     pch = 16,
) +
  segments(x0 = plot_df$X2.50.,
           x1 = plot_df$X97.50.,
           y0 = plot_df$sp_id,
           col = "black") +
  abline(v = 0, lty = "dashed") + 
  axis(2, at = seq(round(min(plot_df$sp_id)),
                   round(max(plot_df$sp_id)), by = 1),
       labels = plot_df$common_name,
       las = 1,
       cex.axis = .6) +
  segments(x0 = first_overlay$X2.50.,
           x1 = first_overlay$X97.50.,
           y0 = first_overlay$sp_id,
           col = "red") +
points(x = first_overlay$mean,
       y = first_overlay$sp_id,
       col = "red")

#ya the means just shift a little bit, minor changes in confidence interval but nothing noticeably big or anything.

#check observer out while we're here
obsef <- read.csv("Z:/Goulden/mbbs-analysis/model_landcover/2025.06.26_cpc_rmbaseline_obsqual/dev+barren_fit_summaries.csv") %>%
  filter(rownames %in% c("c_obs")) %>%
  group_by(common_name) %>%
  mutate(sp_id = cur_group_id()) %>%
  ungroup()

plot(x = obsef$mean, 
     y = obsef$sp_id) +
  segments(x0 = obsef$X2.50.,
           x1 = obsef$X97.50.,
           y0 = obsef$sp_id)+
  abline(v = 0) +
  abline(h = 0)
#only some species have an effect of observer quality, and for all those species it's a positive effect eg. when observer quality increases, the change in count is positive
#Northern Parula, American Crow, Blue Grosbeak, Chipping Sparrow, Great Crested Flycatcher, Carolina Chickadee
