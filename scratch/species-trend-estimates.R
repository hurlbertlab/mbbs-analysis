#---
#Generate a species trend table
#Re: Sauer 2011, the current best practices with bbs data is hierarchical modeling
#GOAL: have a list of trends for all the species on the mbbs DONE
#Things for the future: change from a simple lm to perhaps a poisson? or more complicated modeling methods, and then compare in a trend(lm):trend(poisson) x/y plot how different the two are - are they the same? are there some species that are much better modeled by other methods?
#Other things for the future: change to funciton, add functionality to filter to a given county - ie: function(df, county) and if statement for the n.routes where survey event gets filtered to a county if county is specified 
#---
library(dplyr)
library(lme4)
library(beepr)

#prevent scientific notation to make the trend table easier to read
options(scipen=999)

#read in data
mbbs <- read.csv("data/analysis.df.csv", header = T)
survey.events <- read.csv("data/survey.events.csv", header = T)

#create trend table to store results in
trend_table <- as.data.frame(matrix(ncol = 5, nrow = 0))
colnames(trend_table) <- c("species", "trend", "pvalue", "R2", "significant")

#get species list to filter through
species.list <- unique(mbbs$common_name)

#create variables
current.species <- "NULL BIRD"
filtered.mbbs <- mbbs %>% filter(common_name == "Eastern Bluebird")
ave <- as.data.frame(matrix(ncol = 2, nrow = 0))

#we don't care about county here (right now), just about overall number of routes run in each year
n.routes <- survey.events %>% group_by(year) %>% summarize(n.routes = n())

#for lop to filter to species, then add species + trend + pvalue + R2 to trend table
for (s in 1:length(species.list)) {
  #get species
  current.species <- species.list[s]
  #filter mbbs
  filtered.mbbs <- mbbs %>% filter(common_name == current.species)
  #get average number of the species seen accross all routes in each year
  #ave <- filtered.mbbs %>% group_by(year, route_num) %>% summarize(sum = sum(count)) %>% group_by(year) %>% inner_join(n.routes, by = "year") %>% summarize(mean = sum(sum)/first(n.routes)) #average is mean by year divided by the number of routes run in that year
  ave <- filtered.mbbs %>% group_by(year) %>% summarize(sum = sum(count)) %>% left_join(n.routes, by = "year") %>%
    mutate(mean = sum/n.routes)
  #use first(n.routes) b/c without it the repeated n.routes values cause it to not come out right. spot checked and it's working as expected
  
  #run model - right now a simple linear regression of average count ~ year
  lme <- lm(ave$mean ~ ave$year)
  
  #store model results in trend_table
  trend_table[s, 1] <- current.species
  trend_table$trend[s] <- summary(lme)$coefficients[2,1] #trend estimate
  trend_table$pvalue[s] <- summary(lme)$coefficients[2,4] #pvalue for year
  trend_table$R2[s]<- summary(lme)$r.squared #r2
  if(trend_table$pvalue[s] < 0.05) {
    trend_table$significant[s] <- 1
  } else {
    trend_table$significant[s] <- 0
  }
  
}
beep()

#save species trend table
write.csv(trend_table, "data/trend-table.csv", row.names = F)
#save to rda file
save(trend_table, file = "data/trend-table.rda")












#-----------practice/testing
prac <- mbbs %>% filter(common_name == "Eastern Bluebird")

#get the mean number seen across all routes, this code accounts for that there are multiple stops per route 2020 onwards with the [summarize(sum = sum(count)) %>% group_by(year) %>% summarize(mean = mean(sum))] rather than just using [%>% summarize(mean = mean(count)] 

#we don't care about county here, just about overall number of routes run in each year
n.routes <- survey.events %>% group_by(year) %>% summarize(n.routes = n())

#We do need to also add a check for the number of routes run each year, bc if a route wasn't run that needs to be incorporated 
ave <- mbbs %>% filter(common_name == "Eastern Bluebird") %>% group_by(year, route_num) %>% summarize(sum = sum(count)) %>% group_by(year) %>% inner_join(n.routes, by = "year") %>% summarize(mean = sum(sum)/first(n.routes)) #average is mean by year divided by the number of routes run in that year
#use first(n.routes) b/c without it the repeated n.routes values cause it to not come out right. spot checked and it's working as expected

lme <- lm(ave$mean ~ ave$year)
#summary(lme)
summary(lme)$coefficients[2,1] #trend estimate
summary(lme)$coefficients[2,4] #pvalue for year
summary(lme)$r.squared #r2

#mean = sum/survey.events
#we don't care about county here, just about overall number of routes run in each year
n.routes <- survey.events %>% group_by(year) %>% summarize(n.routes = n())

route_totals <- mbbs_county %>% filter(common_name == species) %>% group_by(year, route_num) %>% summarize(sum = sum(count)) #count in each route each year