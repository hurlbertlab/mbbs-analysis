#######################
#
# Plots to look at the effect of removing 0-to-0 cpc
# on confidence intervals and mean estimates
#
#######################

library(dplyr)
library(stringr)
library(ggplot2)
library(bayesplot)
  bayesplot_theme_update(text = element_text(size = 20, family = "sans")) 
  #families are "serif" for times new roman, "sans" for TT Arial, and "mono" for very typewriter
  bayesplot_theme_set(theme_minimal())
  color_scheme_set(scheme = "purple")

#load functions
source("3.plot-functions.R")

lf_0to0 <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.09.09_cpc_allspin1_rm0to0_halfnormalsig_sp/"
lf_all0s <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.07.25_cpc_allsp_in_one/"
lf_sprt0s <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.09.15_cpc_allspin1_rm0sprtsONLY/"
lf_randomsubsample <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.09.15_cpc_allspin1_rmrandomsubsample/"

list.files(lf_0to0)
list.files(lf_all0s)
list.files(lf_randomsubsample)

landcover <- c("dev+barren", "forest_negative", "forest_positive")
landcover <- "dev+barren"

for(i in 1:length(landcover)) {
  
  sample_size <- 
    read.csv(paste0(lf_0to0, "sample_size.csv"))
  
  rm0to0 <- 
    read.csv(paste0(lf_0to0, landcover[i], "_fit_summaries.csv")) %>%
    filter(!is.na(slope)) %>%
    mutate(rm = ifelse(str_detect(.$rownames, "raw"), FALSE, TRUE)) %>%
    filter(rm == TRUE) %>%
    left_join(sample_size) %>%
    mutate(conf_width = conf_97.5 - conf_2.5) %>%
    arrange(sp_id)
  
  all0s <- 
    read.csv(paste0(lf_all0s, landcover[i], "_fit_summaries.csv")) %>%
    filter(!is.na(slope)) %>%
    mutate(rm = ifelse(str_detect(.$rownames, "raw"), FALSE, TRUE)) %>%
    filter(rm == TRUE) %>%
    mutate(conf_width = conf_97.5 - conf_2.5) %>%
    arrange(sp_id)
   
  #plot changes in the confidence interval width
  plot(
    x = all0s$conf_width,
    y = rm0to0$conf_width, 
    #pch = 16,
    cex = rm0to0$pch_scale - 3,
    xlim = c(0, 0.6),
    ylim = c(0, 0.6),
    ylab = "0 to 0s removed Confidence Interval Width",
    xlab = "All 0s retained Confidence Interval Width",
    main = paste(landcover[i], "Confidence Interval Width Change")
  ) 
  abline(a = 0, b = 1)
  
  #plot changes in the means
  plot(
    x = all0s$mean,
    y = rm0to0$mean,
    cex = rm0to0$pch_scale - 3,
    xlim = c(-.35,.15),
    ylim = c(-.35,.15),
    ylab = "0 to 0s removed Means",
    xlab = "All 0s retained Means",
    main = paste(landcover[i], "Change in Means")
  )
  abline(a = 0, b = 1)
  
}


#plot changes in intervals
post_samples_all0s <- read.csv(paste0(lf_all0s, "dev+barren_", "posterior_samples.csv")) %>%
  select(-row_id, -landcover) %>%
  select(b_landcover_change.37.) %>%
  dplyr::rename(Bobwhite_all0s = b_landcover_change.37.)
post_samples_rm0to0 <- read.csv(paste0(lf_0to0, "dev+barren_posterior_samples.csv")) %>%
  select(-row_id, -landcover) %>%
  select(b_landcover_change.37.) %>%
  dplyr::rename(Bobwhite_rm0to0 = b_landcover_change.37.)
post_samples_sprt0s <- read.csv(paste0(lf_sprt0s, "dev+barren_posterior_samples.csv")) %>%
  select(-row_id, -landcover) %>%
  select(b_landcover_change.37.) %>%
  dplyr::rename(Bobwhite_sprt0s = b_landcover_change.37.) 
post_samples_sprt0s <- rbind(post_samples_sprt0s, post_samples_sprt0s)
post_samples_random <- read.csv(paste0(lf_randomsubsample, "dev+barren_posterior_samples.csv")) %>%
  select(-row_id, -landcover) %>%
  select(b_landcover_change.37.) %>%
  dplyr::rename(Bobwhite_random_subsample = b_landcover_change.37.) 

postsamples <- bind_cols(post_samples_all0s, post_samples_rm0to0, post_samples_sprt0s, post_samples_random)

#full range of posterior samples is indeed different between these two. I want to test this with, removing routes where a species never shows up, but keeping all the rest. Seems a reasonable test. 

mcmc_intervals(postsamples,
               prob = .95,
               prob_outer = 1)

mcmc_areas(postsamples,
               prob = .95,
               prob_outer = 1)

#so, some of the expansion of the CI is indeed from lower sample size
#but this is still the same sample size as removing all the 0to0s! not removing the sprt0s.
#so most of the expansion and the change in means really is from less confidence in the strength of the effect - mostly that without zeros, the effect of landcover change COULD be much stronger than we've been calculating it.


# mcmc_intervals(post_samples_all0s,
#                prob = 0.01,
#                prob_outer = 1)
# mcmc_intervals(post_samples_rm0to0, 
#                prob = 0.01,
#                prob_outer = .95)
#northern bobwhite is #37