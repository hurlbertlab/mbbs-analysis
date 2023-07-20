#after any edits to the Googlemaps.csv, which contains stop level locations for the minibbs routes, this file will split the csv into Chatham/Durham/Orange seperate csvs, in case you need to update the googlemap that surveyors can use
#https://www.google.com/maps/d/u/0/edit?mid=1NRAImHBzXvdQGpsUzcCPb6tqxUkxMx0&usp=sharing

google <- read.csv("spatial/Googlemaps.csv", header = T)
library(stringr)
library(dplyr)


google %>% filter(mbbs_county == "Chatham") %>% write.csv("spatial/ChathamRoutes.csv", row.names = F)
google %>% filter(mbbs_county == "Durham") %>% write.csv("spatial/DurhamRoutes.csv", row.names = F)
google %>% filter(mbbs_county == "Orange") %>% write.csv("spatial/OrangeRoutes.csv", row.names = F)
