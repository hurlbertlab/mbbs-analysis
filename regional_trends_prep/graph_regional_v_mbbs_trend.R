#---------
# graph regional trends vs trends from the mbbs
#---------

library(tidyr) #for pivot
library(dplyr)
library(stringr)
library(ggplot2) #for plotting
library(ggrepel) #for adding text labels that are in the right places
library(vioplot) #for the violin plot

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


#plot
plot_baser_scatter <- function(){
plot(trends$mean, trends$usgs_trend_estimate,
     xlab = "Mini Breeding Bird Survey",
     ylab = "Regional Trends",
     xlim = c(-15,8),
     ylim = c(-10,8),
     col = "black",
     pch = 16,
     type = "n")
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
text(x = trends$mean,
     y = trends$usgs_trend_estimate+.28, 
     labels = trends$species_code, 
     cex = .6)
}



#trying with ggplot, which will make it easier to add the names of the points
ggscatter <- 
  ggplot(trends, aes(mean,usgs_trend_estimate)) +
  geom_point() +
  geom_text_repel(aes(label = trends$species_code), max.overlaps = 10) +
  geom_linerange(aes(x = mean, ymin = usgs_2.5CI, ymax = usgs_97.5CI), colour = "salmon") +
  geom_linerange(aes(y = usgs_trend_estimate, xmin = X5.5., xmax = X94.5.), color = "purple") +
  theme_minimal() +
  labs(x = "Mini Breeding Bird Survey", y = "Regional Trend") +
  geom_abline(intercept = 1, lty = "dotted", color = "darkred")


ggscatter




###### Violin plots
violin_trends <- trends %>%
  pivot_longer(cols = c(mean, usgs_trend_estimate, difference), 
                              names_to = "source",
                              values_to = "trend")

plot_violin <- function() {
with(violin_trends, vioplot(
  trend[source == "mean"], trend[source == "usgs_trend_estimate"], trend[source == "difference"], col=c("purple", "salmon", "lightblue"), names = c("MBBS", "Regional Trend", "Difference (M-R)")
))
}


#parmfrow and then plot both side by side
par(mfrow = c(2, 1))
violin
baserscatter

layout.matrix <- matrix(c(1,2), nrow = 1, ncol = 2)
layout(mat = layout.matrix,
       widths = c(1.5, 2)) #second panel wider
layout.show(2)
#plot violin
plot_violin()
plot_baser_scatter()
