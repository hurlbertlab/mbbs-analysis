#---
#Generate a species trend table
#GOAL: have a list of trends for all the species on the mbbs
#Things for the future: change from a simple lm to perhaps a poisson? or more complicated modeling methods, and then compare in a trend(lm):trend(poisson) x/y plot how different the two are - are they the same? are there some species that are much better modeled by other methods?
#---

mbbs <- read.csv("data/analysis.df.csv", header = T)
survey.events <- read.csv("data/survey.events.csv", header = T)


#make species trend estimate for each species - table like was on the website before

#create trend table to store results in
trend_table <- as.data.frame(matrix(ncol = 4, nrow = 0))
colnames(trend_table) <- c("species", "trend", "R2", "p-value")

#for lop to filter to species, then get trend plus r2 and p-value whire ur at it
for (s in length(unique(mbbs$common_name))) {
  
}

#save species trend table
write.csv(trend_table, "data/trend-table.csv", row.names = F)


#-----------practice
prac <- mbbs %>% filter(common_name == "Eastern Bluebird")

#get the mean number seen across all routes, this code accounts for that there are multiple stops per route 2020 onwards with the [summarize(sum = sum(count)) %>% group_by(year) %>% summarize(mean = mean(sum))] rather than just using [%>% summarize(mean = mean(count)] 

#We do need to also add a check for the number of routes run each year, bc if a route wasn't run that needs to be incorporated 
ave <- mbbs %>% filter(common_name == "Eastern Bluebird") %>% group_by(year, route_num) %>% summarize(sum = sum(count)) %>% group_by(year) %>% inner_join(n.routes, by = "year") %>% summarize(mean = sum(sum)/first(n.routes)) #average is mean by year divided by the number of routes run in that year
#use first(n.routes) b/c without it the repeated n.routes values cause it to not come out right. spot checked and it's working as expected

lme <- lm(ave$mean ~ ave$year)
summary(lme)
summary(lme)$r.squared
summary(lme)$pr

#mean = sum/survey.events
#we don't care about county here, just about overall number of routes run in each year
n.routes <- survey.events %>% group_by(year) %>% summarize(n.routes = n())

route_totals <- mbbs_county %>% filter(common_name == species) %>% group_by(year, route_num) %>% summarize(sum = sum(count)) #count in each route each year