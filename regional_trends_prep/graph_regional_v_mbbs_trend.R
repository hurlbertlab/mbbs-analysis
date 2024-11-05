#---------
# graph regional trends vs trends from the mbbs
#---------

library(dplyr)
library(stringr)

reg_trends <- read.csv("regional_trends_prep/regional_trendWIP.csv")
mbbs_trends <- read.csv("data/bayes_hierarchical_year_results_61sp.csv")

trends <- left_join(mbbs_trends, reg_trends, by = "common_name") %>%
  mutate(mean = mean*100) %>%
  filter(parameter != "a_bar",
         parameter != "sigma")

plot(trends$mean, trends$trend,
     xlab = "mbbs trends",
     ylab = "regional trends",
     main = "Mbbs Trends v Regional Trends")
abline(1,1)

