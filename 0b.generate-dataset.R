#---
#Todo: create dataset

#what this does
#merges mbbs counties to mbbs all
#creates survey.event dataset
#removes species with less than 10 sightings 
#FUTURE: removes observations flaged by the mbbs for like, observer only participated once, too high/too low species counts, other things we talked about flagging etc. 
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


#now let's create a dataframe of unique combinations of route and year, we'll use this in later modeling with group_by() to divide by the number of routes run in that year (averages) or to add 0's when iterating on individual species
routeyear <- unique(mbbs_all[,c('year', 'route_num', 'mbbs_county')]) %>% arrange(year) %>% rename('county' = 'mbbs_county')
#save this csv
write.csv(routeyear, "data/survey.events.csv", row.names = F)

#filter out species that don't show up in enough years

#filter out species that haven't been seen more than the min number of sightings (currently 10)
occurances <- mbbs_all %>% count(common_name) %>% arrange(n) 
min_sightings <- 10
allspecies <- unique(mbbs_all$common_name)

for (s in 1:length(allspecies)) {
  if (occurances$n[s] < min_sightings) {
    mbbs_all <- mbbs_all %>% filter(common_name != occurances$common_name[s])
  }
}
#to check it worked you can rerun:
#occurances <- mbbs_all %>% count(common_name) %>% arrange(n) 
#and now there shouldn't be any species with less than 10 observations that make the list


#now apply the species and observer filters...
#remove routes with problems
#orange rt 11 in 2012


#save dataset 
write.csv(mbbs_all, "data/analysis.df.csv", row.names = F)
#save backup dataset
write.csv(mbbs_all, file = sprintf(
  "data/versioncontrol/analysis.df_%s.csv",
  format(Sys.Date(), "%Y%m%d")
))
