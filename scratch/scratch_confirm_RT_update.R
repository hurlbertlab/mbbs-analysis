########################
#
# Confirm regional trend is working as expected
# Fit a stan model so I can ID if regional trend is working
# and that the scaled regional trends I get out match my expectations.
# BC the difficulty with scaling before hand is like. How do I also scale the standard deviations??
# Unless I ummmmmmmmmmmm. scale both the minimum and maximum values? and then take the standard deviation there?
#
#
#
########################

source("2.analysis-functions.R")

#libraries
library(beepr) #beeps
library(dplyr) #data manip
library(tidyr) #data manip
library(rstan) #stan
library(stringr) #data manip
library(StanHeaders) #stan helper

#prevent scientific notation to make a trend table easier to read
options(scipen=999)

mbbs <- read.csv("data/analysis.df.csv", header = TRUE) %>%
  #do everything we need to do! 
  #remove eastern whip-por-will because it lacks ssi habitat selectivity data
  filter(!common_name == "Eastern Whip-poor-will") %>%
  #remove hummingbird because it's the only one of it's diet group
  filter(!common_name == "Ruby-throated Hummingbird") %>% 
  group_by(common_name) %>%
  mutate(common_name_standard = dplyr::cur_group_id()) %>%
  ungroup() %>%
  arrange(year, route_standard, common_name_standard) %>%
  #add traits
  #habitat niche
  left_join(y = (read.csv("data/species-traits/gdicecco-avian-range-shifts/habitat_niche_ssi_true_zeroes.csv") %>%
                   dplyr::select(english_common_name, ssi)),
            by = c("common_name" = "english_common_name")) %>%
  dplyr::rename(habitat_ssi = ssi) %>%
  #temperature niche position
  left_join(y = 
              (read.csv("data/species-traits/climate_position.csv") %>%
                 dplyr::select(common_name, z_tempwq)),
            by = "common_name") %>%
  #diet category
  left_join(y = 
              read.csv("data/species-traits/avonet_filtered.csv") %>%
              dplyr::select(common_name, Trophic.Niche) %>%
              dplyr::rename(avonet_diet = Trophic.Niche), 
            by = "common_name") %>%
  group_by(avonet_diet) %>%
  mutate(diet_category_standard = cur_group_id()) %>%
  ungroup() %>%
  #regional trend
  left_join(read.csv("data/bbs-regional/species-list-usgs-regional-trend.csv"), by = "common_name") %>%
  dplyr::mutate(usgs_sd = (.$usgs_97.5CI - .$usgs_2.5CI)/(2 * qnorm(0.95))) %>%
  #regional trend is on a different scale from our result, so we'll re-scale by dividing by 100 (current 2.01 = 2% change, want .02 = 2% change in pop per year)
  dplyr::mutate(usgs_trend_estimate = usgs_trend_estimate/100,
                usgs_sd = usgs_sd/100) %>%
  #remove extra columns
  dplyr::select(-usgs_note, -county, -route_num, -sci_name, -standardized_observers, -nstops) %>%
  #center variables as possible :)
  mutate(scale_habitat_ssi = (habitat_ssi - mean(habitat_ssi))/sd(habitat_ssi),
         scale_ztempwq = (z_tempwq - mean(z_tempwq))/sd(z_tempwq),
         scale_obs_quality = (observer_quality - mean(observer_quality))/sd(observer_quality)
  )

rt_checks <- mbbs %>% 
  select(common_name, common_name_standard, usgs_trend_estimate, usgs_2.5CI, usgs_97.5CI, usgs_sd) %>%
  group_by(common_name, common_name_standard, usgs_trend_estimate, usgs_2.5CI, usgs_97.5CI, usgs_sd) %>%
  summarize() %>%
  ungroup() %>%
  mutate(scaled_trend = (usgs_trend_estimate - mean(usgs_trend_estimate))/sd(usgs_trend_estimate),
         scaled_sd = usgs_sd/sd(usgs_trend_estimate))


#specify stan model
stan_model_file <- "scratch/scratch_confirm_RT_update.stan"

#compile
stan_model <- stan_model(file = stan_model_file)
beepr::beep()

#declare datstan
datstan <- list(
  N = nrow(rt_checks), #number of observations
  Nsp = length(unique(rt_checks$common_name_standard)), #n species
  sp = rt_checks$common_name_standard, #species indices for each observation
  regional_trend_mean = rt_checks$scaled_trend,
  regional_trend_sd = rt_checks$scaled_sd
)

#fit
fit <- sampling(stan_model, 
                data = datstan, 
                chains = 4,
                cores = 4, 
                iter = 3000, 
                warmup = 1000
)
beepr::beep()

fit_summary <- summary(fit)  
rownames <- row.names(fit_summary$summary)
fit_final <- as.data.frame(fit_summary$summary)
fit_final$rownames <- rownames  
fit_final <- fit_final %>%
  relocate(rownames, .before = mean) %>%
  #rename numeric columns
  rename_with(~ paste0("conf_", .), .cols = matches("^[0-9]")) %>%
  #remove %s in column names
  rename_with(~ str_remove(., "%"), .cols = everything()) %>%
  filter(!stringr::str_detect(.$rownames, "intercept"))

rt_scaled <- fit_final %>%
  filter(str_detect(.$rownames, "^regional_trend")) %>%
  mutate(common_name_standard = as.numeric(str_extract(.$rownames, "[0-9]+")),
         scaled_rt = mean) %>%
  select(common_name_standard, scaled_rt) %>%
  left_join(rt_checks, by = "common_name_standard") %>%
  select(common_name_standard, scaled_rt, scaled_trend)

plot(x = -3:3, y = -3:3)
points(x = rt_scaled$scaled_rt, y = rt_scaled$scaled_trend, pch = 16)
#these are so close to the 1:1 line I think the little bit of variation this introduces is really really really fine. 

m <- lm(rt_scaled$scaled_trend ~ rt_scaled$scaled_rt)
summary(m) #r-squared is 1
cor(rt_scaled$scaled_rt, rt_scaled$scaled_trend) #the correlation is .9999883
#yeah, the itty bit of variation this introduces is really really really fine. This is getting sampled correctly.

#save_stan_traceplot_pdf(fit,
#                        file = paste0("scratch_RT_update_traceplots.pdf"),
#                        pars = NULL,
#                        n_per_page = 6,
#                        remove_pars = "eta\\[[0-9]+\\]|lp__|sprt_intercept\\[[0-9]+\\]")
