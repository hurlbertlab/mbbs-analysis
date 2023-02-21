#----
#script to create a 0-inflated version of the dataset
#not finished
#----

#first, read in mbbs.all dataset

#create list of all species seen in any of the counties
allspecies <- unique(mbbs_all$common_name)

#create list of all possible routes
allroutes <- sort(unique(mbbs_all$route_num)) #remember: route_num has been modified to differ between countys (+20 to Durham, +40 to Chatham)

#create list of all possible years
allyears <- sort(unique(mbbs_all$year))

#y number of years
y <- length(allyears)
#r number of routes
r <- length(allroutes)
#s number of species
s <- length(allspecies)

#now let's create a dataframe of unique combinations of route and year
routeyear <- unique(mbbs_all[,c('year', 'route_num')]) %>% arrange(year)

#create dataframe with species repeated in every year and route that's been run.
routeyearspecies <- data.frame(route_num = rep(routeyear$route_num, s), year = rep(routeyear$year, s), species = rep(allspecies, times = nrow(routeyear)))

#lets arrange that nicely so that we can check for problems easier
routeyearspecies <- routeyearspecies %>% arrange(year, route_num, species)

#Now, we'll join together the routeyearspecies dataset with the mbbs_all in order to create those 0's where the route was run, but the species was not seen
df <- left_join(routeyearspecies, mbbs_all, by = c("species" = "common_name", "route_num" = 'route_num', "year" = "year"))

#change NA's to 0s - not working yet 
df <- df %>% replace(is.na(count), 0)

#save dataset [0-inflated]
#write.csv(df, "data/0.inflated.mbbs")
#save backup dataset - example below
#write.csv(mbbs_orange, file = sprintf(
# "inst/analysis_data/mbbs_orange_%s.csv",
#format(Sys.Date(), "%Y%m%d")
#))