#---------
# graph regional trends vs trends from the mbbs
#---------

library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(vioplot)

#get regional trends from the usgs data instead of the regional_trendWIP
reg_trends <- read.csv("regional_trends_prep/species-list.csv")
mbbs_trends <- read.csv("data/bayes_hierarchical_year_results_61sp.csv")
birdcode <- read.csv("data/bird_code_to_common_name.csv") %>%
  select(common_name, species_code)

trends <- left_join(mbbs_trends, reg_trends, by = "common_name") %>%
  mutate(mean = mean*100,
         X5.5. = X5.5.*100,
         X94.5. = X94.5.*100,
         difference = mean-usgs_trend_estimate) %>%
  filter(parameter != "a_bar",
         parameter != "sigma") %>%
  left_join(birdcode, by = "common_name")

plot(trends$mean, trends$usgs_trend_estimate,
     xlab = "mbbs trends",
     ylab = "regional trends",
     xlim = c(-15,8),
     ylim = c(-10,8),
     main = "Mbbs Trends v Regional Trends", 
     col = "black",
     pch = 16)
abline(1,1, col = "red4", lty = "dotted")
#add segment for the regional trend variation
segments(trends$mean, 
         trends$usgs_2.5CI,
         trends$mean,
         trends$usgs_97.5CI,
         col = "salmon")
#add segment for the mbbs variation
segments(trends$X5.5.,
         trends$usgs_trend_estimate,
         trends$X94.5., 
         trends$usgs_trend_estimate, 
         col = "purple")
#add text - going to not do this for now bc it's really messy - I know allen has a way to do this. Maybe it should be at the bottom of the usgs trend 2.5?
#text(x = trends$mean,
#     y = trends$usgs_2.5CI -.5, 
#     labels = trends$species_code, 
#     cex = .5)

#trying with ggplot, which will make it easier to add the names of the points
ggplot(trends, aes(mean,usgs_trend_estimate)) +
  geom_point() +
  geom_text(aes(label = species_code), hjust = - 0.25) 




###### Violin plots
violin_trends <- trends %>%
  pivot_longer(cols = c(mean, usgs_trend_estimate, difference), 
                              names_to = "source",
                              values_to = "trend")


with(violin_trends, vioplot(
  trend[source == "mean"], trend[source == "usgs_trend_estimate"], trend[source == "difference"], col=c("purple", "salmon", "lightblue"), names = c("MBBS", "Regional Trend", "Difference (M-R)")
))


#parmfrow and then plot both side by side