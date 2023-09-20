#Generate a species trend table
#Re: Sauer 2011, the current best practices with bbs data is hierarchical modeling
#GOAL: have a list of trends for all the species on the mbbs DONE
#Things for the future: change from a simple lm to perhaps a poisson? or more complicated modeling methods, and then compare in a trend(lm):trend(poisson) x/y plot how different the two are - are they the same? are there some species that are much better modeled by other methods?
#Other things for the future: change to funciton, add functionality to filter to a given county - ie: function(df, county) and if statement for the n.routes where survey event gets filtered to a county if county is specified 
#---
library(dplyr)
library(lme4)
library(beepr)
library(MASS)

#prevent scientific notation to make the trend table easier to read
options(scipen=999)

#read in data
mbbs <- read.csv("data/analysis.df.csv", header = T)
survey.events <- read.csv("data/survey.events.csv", header = T)

#read in data messier before the observer branch is merged in git and I can update everything straight from package
load("C:/git/mbbs_orange.rda")
load("C:/git/mbbs_durham.rda")
load("C:/git/mbbs_chatham.rda")
load("C:/git/mbbs_survey_events.rda")
mbbs <- bind_rows(mbbs_orange, mbbs_chatham, mbbs_durham) %>%
  left_join(mbbs_survey_events, by = c("mbbs_county", "year", "route_num")) %>%
  mutate(route_ID = route_num + case_when(
    mbbs_county == "orange" ~ 100L,
    mbbs_county == "durham" ~ 200L,
    mbbs_county == "chatham" ~ 300L,)) %>%
  filter(count > 0)

#filter out species that haven't been seen more than the min number of sightings (currently 10)
occurances <- mbbs %>% count(common_name) %>% arrange(n) 
min_sightings <- 10
allspecies <- unique(mbbs$common_name)

for (s in 1:length(allspecies)) {
  if (occurances$n[s] < min_sightings) {
    mbbs <- mbbs %>% filter(common_name != occurances$common_name[s])
  }
}

#create a histogram of the data to see if it can be modeled using a poisson distribution 
hist(mbbs$count)
plot(count ~ year, data = mbbs)
abline(lm(count ~ year, data = mbbs))
#data does not fit that least squares regression very well, probably yeah poisson is a better option

#create trend table to store results in
trend_table <- as.data.frame(matrix(ncol = 9, nrow = 0))
colnames(trend_table) <- c("species", "trend", "pvalue", "error", "significant", "routevariation", "observervariation", "routesignificant", "observersignificant")

#get species list to filter through
species.list <- unique(mbbs$common_name)

#create variables
current.species <- "NULL BIRD"
filtered.mbbs <- mbbs %>% filter(common_name == "Eastern Bluebird")
ave <- as.data.frame(matrix(ncol = 2, nrow = 0))

#we don't care about county here (right now), just about overall number of routes run in each year
n.routes <- survey.events %>% group_by(year) %>% summarize(n.routes = n())

#negative binomials are not working beautifully. Perhaps we should try poissons and THEN determine if a negative binomial is the proper modeling approach for this data.

#for lop to filter to species, then add species + trend + pvalue + R2 to trend table
for (s in 1:length(species.list)) {
  #get species
  current.species <- species.list[s]
  #filter mbbs
  filtered.mbbs <- mbbs %>% filter(common_name == current.species) %>% #and survey events is already left joined
    #account for eBird data where we have multiple records per route and yet need full-route count info
    group_by(route_ID, year) %>%
    mutate(count = sum(count)) %>% #redo count to have the sum of all counts from that year on that route_ID
    distinct(route_ID, year, .keep_all = TRUE) #remove duplicate data points so there's one record per route_ID and year for each species

  model <- glm.nb(count ~ year, data = filtered.mbbs) #should +n.routes b/c thats able to account for survey effort then
  
  #store model results in trend_table
  trend_table[s, 1] <- current.species
  trend_table$trend[s] <- summary(model)$coefficients[2,1] #trend estimate
  trend_table$pvalue[s] <- summary(model)$coefficients[2,4] #pvalue for year
  trend_table$error[s] <- summary(model)$coefficients[2,2] #error for year
  trend_table$significant[s]<- case_when(trend_table$pvalue[s] <= 0.05 ~ 1,
                                         trend_table$pvalue[s] > 0.05 ~ 0)
  # trend_table$routevariation[s] <- summary(model)$coefficients[3,1]
  # trend_table$routesignificant[s] <- summary(model)$coefficients[3,4]
  # trend_table$observervariation[s] <- summary(model)$coefficients[4,1]
  # trend_table$observersignificant[s] <- summary(model)$coefficients[4,4]
  }
beep()

#save species trend table
write.csv(trend_table, "data/trend-table-negbinpoisson.csv", row.names = F)
#save to rda file
save(trend_table, file = "data/trend-table-negbinpoisson.rda")












#-----------practice/testing
#accounting for eBird differences
test <- mbbs %>% filter(common_name == "Red-eyed Vireo", route_ID == 101) %>%
  group_by(route_ID, year) %>%
  mutate(count = sum(count)) %>% #redo count to have the sum of all counts from that year on that route_ID
  #and therefore accommodate the ebird data that includes counts at the stop level
  distinct(route_ID, year, .keep_all = TRUE) #remove duplicate data points so there's one record per route_ID and year for each species
