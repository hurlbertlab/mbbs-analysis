#---
#Todo: create dataset
#file to create the analysis dataset with all the routes run in each year and that includes the 0 values for when species r not seen
#now let's work on creating a dataset from which we can incorporate 0's [where a route was run but the species was not counted on the route at all) into our averages
#---

#load mbbs library
library(mbbs)
library(dplyr)
library(stringr)

#bring datasets into the local environment
mbbs_orange <- mbbs_orange
mbbs_durham <- mbbs_durham
mbbs_chatham <- mbbs_chatham

#create mbbs_all
mbbs_all <- bind_rows(mbbs_orange, mbbs_chatham, mbbs_durham)

#Fix potential error sources
mbbs_all <- mbbs_all %>%
  #16 observations in Wake county instead of Durham, lump those back into durham
  mutate(county = str_replace(county, "Wake", "Durham"))  %>%
  #route number is not unique within study, only country - lets create distinct route ID across county
  mutate(
    county_factor = case_when(
      mbbs_county == "orange" ~ 0,
      mbbs_county == "durham" ~ 20,
      mbbs_county == "chatham" ~ 40,),
    route_num = route_num + county_factor #keep as route_num to be consistent with format and column names in the individual county plots
  )

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

#filter out species with not enough sightings

#filter out routes where not consistent ppl 
#orange in 2011 

#save dataset
#save backup dataset
#write.csv(mbbs_orange, file = sprintf(
 # "inst/analysis_data/mbbs_orange_%s.csv",
  #format(Sys.Date(), "%Y%m%d")
#))