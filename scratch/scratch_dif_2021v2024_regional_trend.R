############
#
# Testing differences between 2021/2 and 2024 regional trend calculations from the USGS
#
###########

twotwo <- read.csv("data/bbs-regional/species-list-usgs-regional-trend.csv")
twofour <- read.csv("data/bbs-regional/species-list-usgs-regional-trend-2024.csv")
#they're in the same order :)

plot(x = twotwo$usgs_trend_estimate,
     y = twofour$usgs_trend_estimate)

model <- lm(twotwo$usgs_trend_estimate ~ twofour$usgs_trend_estimate)
summary(model) #r2 is 0.97. 
