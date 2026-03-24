########
#
# Make datasets for any supplemental tables
#
#######

library(dplyr)
library(stringr)

final_data_location <- "model/2026.03.12_ch1_m1_final/"

z <- qnorm((1+0.87)/2) #confidence interval 87%

#abundance
  mbbs <- read.csv("data/analysis.df.csv") |>
    group_by(common_name) |>
    summarize(total_abundance = sum(count)) |>
    filter(common_name != "Eastern Whip-poor-will") |>
    ungroup()
  #total n
  mbbs |> summarize(n = sum(total_abundance))

#Species change over time
species_trends <- read.csv(paste0(final_data_location, "fit_summary.csv")) |>
  filter(str_detect(rownames, "b\\[")) |>
  mutate(common_name_standard = as.integer(str_extract(rownames, "[0-9]([0-9])?"))) %>%
  left_join(read.csv(paste0(final_data_location, "beta_to_common_name.csv")), by = c("common_name_standard")) |>
  dplyr::select(common_name, mean, sd, conf_2.5, conf_97.5) %>%
  mutate(significant = ifelse(conf_2.5 < 0 & conf_97.5 > 0, FALSE, TRUE),
         conf_6.5 = (mean - (sd * z)),
         conf_93.5 = (mean + (sd * z)),
         sig_87 = ifelse(conf_6.5 < 0 & conf_93.5 > 0, FALSE, TRUE),
         mean_gt01 = ifelse(mean < -0.01 | mean > 0.01, TRUE, FALSE),
         trend_direction = ifelse(conf_2.5 > 0, "positive", "negative")) |>
  mutate(trend_direction = case_when(
    significant == TRUE & trend_direction == "negative" ~ "Declining", #decreasing
    significant == FALSE & sig_87 == TRUE & mean_gt01 == TRUE ~ "Likely Declining [87% CI, |mean| > 0.01]", #decline supported at 87% confidence interval
    significant == FALSE & sig_87 == TRUE & mean_gt01 == FALSE ~ "Stable", #stable
    significant == FALSE & sig_87 == FALSE ~ "Stable", #stable
    significant == TRUE & trend_direction == "positive" ~ "Increasing" #increasing
  )) |>
  dplyr::select(common_name, mean, sd, conf_2.5, conf_6.5, conf_93.5, conf_97.5, trend_direction) |>
  dplyr::rename(trend_estimate = mean,
                cred_2.5 = conf_2.5,
                cred_6.5 = conf_6.5,
                cred_93.5 = conf_93.5,
                cred_97.5 = conf_97.5) |>
  arrange(common_name) |> #just make sure alphabetical as last step
  #need to add in scientific name for taxonomy
  left_join((read.csv("data/species-traits/eBird_taxonomy_v2024.csv") |>
               dplyr::select(PRIMARY_COM_NAME, SCI_NAME)), by = c("common_name" = "PRIMARY_COM_NAME")) |>
  dplyr::relocate(SCI_NAME, .after = common_name) |>
  dplyr::rename(scientific_name = SCI_NAME) |>
  dplyr::relocate(trend_direction, .after = scientific_name) |>
  #ah, and you know what, it would be pretty nice to include abundance here as well.
  left_join(mbbs, by = c("common_name")) |>
  dplyr::relocate(total_abundance, .after = scientific_name)

write.csv(species_trends, "data/ch1-supplemental-tables/species_trends.csv", row.names = FALSE)  
