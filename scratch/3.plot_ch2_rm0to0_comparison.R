#######################
#
# Plots to look at the effect of removing 0-to-0 cpc
# on confidence intervals and mean estimates
#
#######################

library(dplyr)
library(stringr)

#load functions
source("3.plot-functions.R")

lf_0to0 <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.09.09_cpc_allspin1_rm0to0_halfnormalsig_sp/"
lf_all0s <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.07.25_cpc_allsp_in_one/"

list.files(lf_0to0)
list.files(lf_all0s)

landcover <- c("dev+barren", "forest_negative", "forest_positive")
#landcover <- "dev+barren"

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
