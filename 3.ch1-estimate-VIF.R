#######################
#
# Script to estimate VIF
# can't do it within the Bayes model
# but can replicate outside of it.
#
#
#######################

library(car) #has VIF function.
library(dplyr)
library(stringr)

# Pull means from our model outputs.
fit_summary <- read.csv("model/2026.04.09_ch1_rmNOBO_final/fit_summary.csv") |>
  mutate(select_for = str_detect(rownames, "^b")) |> #just need betas
  filter(select_for == TRUE) |>
  mutate(common_name_standard = as.integer(str_extract(rownames, "[0-9]([0-9])?"))) |>
  left_join(read.csv("model/2026.04.09_ch1_rmNOBO_final/beta_to_common_name.csv"), by = c( "common_name_standard")) |>
  #add species traits
  left_join(read.csv("model/2026.04.09_ch1_rmNOBO_final/species_traits.csv"), by = c("common_name_standard"))

# 'Replicate' our Bsp model structure
model <- glm(mean ~ scale_habitat_ssi + scale_ztempwq + scale_insect_perc + scale_usgs_trend, data = fit_summary)

vif_values <- car::vif(model)
vif_values
#awesome. All the VIFs are just over 1, so they're not correlated.
#scale_habitat_ssi     scale_ztempwq scale_insect_perc 
#1.028719          1.145933          1.072807 
#scale_usgs_trend 
#1.128701
