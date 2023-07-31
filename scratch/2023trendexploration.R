trend.table <- read.csv("data/trend-table.csv", header = T)
trend.table22 <- read.csv("data/trend-table2022.csv", header = T)

hist(trend.table$trend, col = "blue")
hist(trend.table22$trend, add = TRUE)
#in the last year, some species have moved from being around 0 to having more of a trend

plot(trend.table$trend, trend.table22$trend) #so some change, but not a huge amount, are more significant now?

table(trend.table$significant, trend.table22$significant)
#So for the most part both years agree on the trend, there's 3 that have beome significant and 1 that has become not significant, numberwise, there may be more switches than that). 

#why don't we try, making a model that includes observers as a random effect now? Or getting the habitat data for the stops?

#whats red eyed viro and white eyed vireo doing in terms of numbers
vireos <- mbbs %>% filter(common_name == "White-eyed Vireo" | common_name == "Red-eyed Vireo")
survey.events <- read.csv("data/survey.events.csv")
n.routes <- survey.events %>% group_by(year) %>% summarize(n.routes = n())
REVI <- vireos %>% filter(common_name == 'Red-eyed Vireo') %>%  group_by(year) %>% summarize(sum = sum(count)) %>% left_join(n.routes, by = "year") %>%
  mutate(mean = sum/n.routes)#average is mean by year divided by the number of routes run in that year
WEVI <- vireos %>% filter(common_name == 'White-eyed Vireo') %>% group_by(year) %>% summarize(sum = sum(count)) %>% left_join(n.routes, by = "year") %>%
  mutate(mean = sum/n.routes)

plot(REVI$year, REVI$mean) 
plot(WEVI$year, WEVI$mean)
#still a lot more REVI than WEVI but yes trends are switched. Really surprising jump in 2018/2019 in REVI numbers, back up to a sudden high. Wonder if we could track down a reason for the positive increase,maybe fewer routes surveyed and those that were surveyed had high REVI? robust tho bc then later numbers are coming back down from that sudden peak
#REVI counts vs survey dates, is Chat 7 consistent timing
