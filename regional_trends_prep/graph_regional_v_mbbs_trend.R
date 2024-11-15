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
  dplyr::select(common_name, species_code)

trends <- left_join(mbbs_trends, reg_trends, by = "common_name") %>%
  mutate(mean = mean*100,
         X5.5. = X5.5.*100,
         X94.5. = X94.5.*100,
         difference = mean-usgs_trend_estimate) %>%
  filter(parameter != "a_bar",
         parameter != "sigma") %>%
  left_join(birdcode, by = "common_name")

#run a linear model
mvrlm <- lm(mean ~ usgs_trend_estimate, data = trends)
summary(mvrlm)

#set limits for all plots
set_xlim <- c(-7,7)
set_ylim <- c(-15,8)


#plot
plot_baser_scatter <- function(){
plot(trends$usgs_trend_estimate, trends$mean,
     ylab = "Mini Breeding Bird Survey",
     xlab = "Regional Trends",
     xlim = set_xlim,
     ylim = set_ylim,
     col = "black",
     pch = 16,
     type = "n")
abline(1,1, col = "red4")
abline(h = 0, lty = "dotted", col = "gray60")
abline(v = 0, lty = "dotted", col = "gray60")
#abline(mvrlm) don't need to graph the regression line, ppl can see the trend visually
#add segment for the mbbs variation
segments(trends$usgs_trend_estimate,
         trends$X5.5.,
         trends$usgs_trend_estimate,
         trends$X94.5.,
         col = "purple")
#add segment for the regional trend variation
segments(trends$usgs_2.5CI, 
         trends$mean,
         trends$usgs_97.5CI,
         trends$mean,
         col = "salmon")
#add text - going to not do this for now bc it's really messy - I know allen has a way to do this. Maybe it should be at the bottom of the usgs trend 2.5?
text(y = trends$mean + .35,
     x = trends$usgs_trend_estimate, 
     labels = trends$species_code, 
     cex = .6)
#add r2
text(x = 6,
     y = -15,
     labels = paste("r2 =", signif(summary(mvrlm)$r.squared, 4)))
}



#trying with ggplot, which will make it easier to add the names of the points
ggscatter <- 
  ggplot(trends, aes(mean,usgs_trend_estimate)) +
  geom_point() +
  geom_text_repel(aes(label = species_code), max.overlaps = 10) +
  geom_linerange(aes(x = mean, ymin = usgs_2.5CI, ymax = usgs_97.5CI), colour = "salmon") +
  geom_linerange(aes(y = usgs_trend_estimate, xmin = X5.5., xmax = X94.5.), color = "purple") +
  theme_minimal() +
  labs(x = "Mini Breeding Bird Survey", y = "Regional Trend") +
  geom_abline(intercept = 1, lty = "dotted", color = "darkred") 
#need to add a linear regression line

ggscatter




###### Violin plots
violin_trends <- trends %>%
  pivot_longer(cols = c(mean, usgs_trend_estimate, difference), 
                              names_to = "source",
                              values_to = "trend")


plot_violin_mbbs <- function() {
  
  plot(0:1,0:1,
       type = "n",
       xlim = c(.5,1.5),
       ylim = c(-15, 10),
       axes = FALSE,
       ann = FALSE)
  
  vioplot(violin_trends$trend[violin_trends$source  == "mean"],
          col = "purple",  
          add = TRUE, 
          names = "Mini Breeding Bird Survey")
  axis(side=2,at=c(-15, -10, -5, 0, 5, 10),labels=c(-15, -10, -5, 0, 5, 10))
  abline(h = 0, lty = "dotted", col = "grey60")
  
  
}


plot_violin_regional <- function() {
  
  plot(0:1,0:1,
       type = "n",
       xlim = set_ylim,
       ylim = c(.5,1.5),
       axes = FALSE,
       ann = FALSE)
  
  vioplot(violin_trends$trend[violin_trends$source  == "usgs_trend_estimate"],
          col = "salmon",
          horizontal = TRUE,
          add = TRUE)
          #xlab = "Regional Trend"
  axis(side=1,at=c(-6, -4, -2, 0, 2, 4, 6),labels=c(-6, -4, -2, 0, 2, 4, 6))
  abline(v = 0, lty = "dotted", col = "grey60")

}


#layout guide: https://bookdown.org/ndphillips/YaRrr/arranging-plots-with-parmfrow-and-layout.html
layout.matrix <- matrix(c(2,0,1,3), nrow = 2, ncol = 2)
layout.matrix
layout(mat = layout.matrix,
       widths = c(.75, 1.5), #second column wider
       heights = c(1.75, 1)) #first row taller
layout.show(3)
#improvement would be to have 1 be more square...
#but I think also, want to remove the text bc it's adding a lot to the plot?
#plot violin
plot_baser_scatter()
plot_violin_mbbs()
plot_violin_regional()

dev.off()

#so, some work to do to get everything lined up nicely, but yeah! :D
#alternative methods that may work better for arranging plots more tightly together could be par(mfrow), par(mfcol), and split.screen

  par(bg = "white")           # default is likely to be transparent
  split.screen(c(2, 2))       # split display into two screens
  screen(2)
  plot_baser_scatter()
  screen(1)
  plot_violin_mbbs()
  screen(4)
  plot_violin_regional()
  close.screen(all = TRUE)
dev.off()

#not sure how this is supposed to work, but it helps set the area
#My only issue is that I want to split the screen into unequal sizes. I would like panel 1 (screen 1) to occupy 70% of the final image and screen 2/panel 2 to occupy the rest. Any suggestions on how do I make changes to the split.screen command at the beginning ?
coord <- matrix(c(c(0, 1, 0, 0.7), c(0, 1, 0.7, 1)), byrow=T, ncol=4)
split.screen(coord)
screen(1)
plot(1:10)
screen(2)
plot(1:10)









#--------------------------
# Old plots, kept in case of need for reference
#--------------------------

plot_violin <- function() {
  with(violin_trends, vioplot(
    trend[source == "mean"], trend[source == "usgs_trend_estimate"], trend[source == "difference"], col=c("purple", "salmon", "lightblue"), names = c("MBBS", "Regional Trend", "Difference (M-R)")
  ))
}
plot_violin()
