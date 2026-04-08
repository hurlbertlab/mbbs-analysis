##########################
#
# Use the output of the final model
# to pick which species should be removed
# when running a sensitivity analysis.
#
#
##########################

library(dplyr)
library(stringr)

#final model location
lf_ch1m1 <- "model/2026.03.12_ch1_m1_final/"

fit_summary <- read.csv(paste0(lf_ch1m1, "fit_summary.csv")) |>
  #filter to just the species slopes
  filter(str_detect(rownames, "b\\[")) |>
  #left join trait information
  mutate(common_name_standard = as.integer(str_extract(rownames, "[0-9]([0-9])?"))) |>
  left_join(read.csv(paste0(lf_ch1m1, "beta_to_common_name.csv"))) |>
  left_join(read.csv(paste0(lf_ch1m1, "species_traits.csv")), by = c("common_name_standard"))

#species outliers for temperature niche position
temp_niche_outliers <- fit_summary |>
  #filter out species that are more than three standard deviations away from the mean scaled temperature niche position
  filter(abs(scale_ztempwq) > 3) |>
  dplyr::select(common_name) |>
  mutate(reason = ">3sd scaled_ztempwq")

#exclude extremes in population change, so declines more extreme than the highest increase with adjust for +1% per year. Basically, by declining so much these species have more weight than others in deciding trait slopes.
population_change_extremes <- fit_summary |>
  filter(abs(mean) > max(mean) + 0.01) |>
  dplyr::select(common_name) |>
  mutate(reason = "population loss more extreme than max population increase + 1%")
#as expected, house sparrow, common grackle, northern bobwhite


#and I suppose we should treat all the other trait variables the same as temp_niche and also remove their outliers, although I don't think there's really an much outside influence in those (just from visual check of the graphs)
habitat_selectivity_outliers <- fit_summary |>
  filter(abs(scale_habitat_ssi) > 3) |>
  dplyr::select(common_name) |>
  mutate(reason = ">3sd scaled_habitat_ssi")
#yup, no species. max(fit_summary$scale_habitat_ssi = 2.9)

#and I know there are no insect percent outliers.
{
min(fit_summary$scale_insect_perc) #-2.1
max(fit_summary$scale_insect_perc) #1.19
#but we'll run the code to be equal to it.
perc_insectivory_outliers <- fit_summary |>
  filter(abs(scale_insect_perc) > 3) |>
  dplyr::select(common_name) |>
  mutate(reason = ">3sd scaled_insect_perc")
}

#join together the list of species needing removal.
sensitivity_remove_species <- 
  bind_rows(
  temp_niche_outliers,
  population_change_extremes,
  habitat_selectivity_outliers,
  perc_insectivory_outliers
  )

write.csv(sensitivity_remove_species, "data/species-traits/ch1-sensitivity-analysis/species_to_remove.csv", row.names = FALSE)
