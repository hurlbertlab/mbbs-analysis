#---------
# graph regional trends vs trends from the mbbs
#---------

library(dplyr)
library(stringr)

#get regional trends from the usgs data instead of the regional_trendWIP
reg_trends <- read.csv("regional_trends_prep/species-list.csv")
mbbs_trends <- read.csv("data/bayes_hierarchical_year_results_61sp.csv")

trends <- left_join(mbbs_trends, reg_trends, by = "common_name") %>%
  mutate(mean = mean*100,
         X5.5. = X5.5.*100,
         X94.5. = X94.5.*100) %>%
  filter(parameter != "a_bar",
         parameter != "sigma")

plot(trends$mean, trends$usgs_trend_estimate,
     xlab = "mbbs trends",
     ylab = "regional trends",
     xlim = c(-10,12),
     ylim = c(-10,12),
     main = "Mbbs Trends v Regional Trends", 
     col = "black",
     pch = 16)
abline(1,1, col = "red4", lty = "dotted")
#add segment for the regional trend variation
segments(trends$mean, 
         trends$usgs_2.5CI,
         trends$mean,
         trends$usgs_97.5CI)
#add segment for the mbbs variation
segments(trends$X5.5.,
         trends$usgs_trend_estimate,
         trends$X94.5., 
         trends$usgs_trend_estimate)

