trend.table <- read.csv("data/trend-table.csv", header = T)
trend.table22 <- read.csv("data/trend-table2022.csv", header = T)

hist(trend.table$trend, col = "blue")
hist(trend.table22$trend, add = TRUE)
#in the last year, some species have moved from being around 0 to having more of a trend

plot(trend.table$trend, trend.table22$trend) #so some change, but not a huge amount, are more significant now?

table(trend.table$significant, trend.table22$significant)
#So for the most part both years agree on the trend, there's 3 that have beome significant and 1 that has become not significant, numberwise, there may be more switches than that). 