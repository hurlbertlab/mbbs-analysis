#for making a violin plot with change in urbanization along routes.
library(dplyr)
library(vioplot)

#read in the data by route
nlcd <- read.csv("data/landtype_byroute.csv", header = TRUE) %>%
  #just want start and end nlcds
  filter(year == 2001 | year == 2021) %>%
  #filter to just the developed landcovers
  dplyr::filter(ijbg_class == "developed") %>%
  #group_by each route, grouping by "ijbg_class and totpix" so these say in the transmuted dataset
  group_by(mbbs_county, year, route_num, ijbg_class, totpix) %>%
  transmute(frequency = sum(frequency)) %>%
  #remove extraneous rows
  distinct() %>%
  mutate(percent_developed = (frequency/totpix)*100,
         year = as.integer(year)) %>%
  ungroup() %>%
  #group to each route
  group_by(mbbs_county, route_num) %>%
  mutate(dif_urbanization = percent_developed[year==2021] - percent_developed[year==2001]) %>%
  ungroup() 

#remove extraneous rows
nlcd <- nlcd %>% filter(year == 2001)

with(nlcd, vioplot(
  dif_urbanization[mbbs_county == "durham"], dif_urbanization[mbbs_county=="orange"], dif_urbanization[mbbs_county == "chatham"], col="lightgrey", names = c("Durham", "Orange", "Chatham")
))




#read in the data by routequarter
nlcdquarter <- read.csv("data/landtype_byroutequarter.csv", header = TRUE) %>%
  #just want start and end nlcds
  filter(year == 2001 | year == 2021) %>%
  #filter to just the developed landcovers
  dplyr::filter(ijbg_class == "developed") %>%
  #group_by each route, grouping by "ijbg_class and totpix" so these say in the transmuted dataset
  group_by(County_Route, year, routequarter, ijbg_class, totpix) %>%
  transmute(frequency = sum(frequency)) %>%
  #remove extraneous rows
  distinct() %>%
  mutate(percent_developed = (frequency/totpix)*100,
         year = as.integer(year)) %>%
  ungroup() %>%
  #group to each route
  group_by(County_Route, routequarter) %>%
  mutate(dif_urbanization = percent_developed[year==2021] - percent_developed[year==2001]) %>%
  ungroup() %>%
  #add mbbs_county information
  mutate(mbbs_county = case_when(
    str_starts(County_Route, "Chatham") ~ "chatham",
    str_starts(County_Route, "Durham") ~ "durham",
    str_starts(County_Route, "Orange") ~ "orange"
  ))

#remove extraneous rows
nlcdquarter <- nlcdquarter %>% filter(year == 2001)

with(nlcdquarter, vioplot(
  dif_urbanization[mbbs_county == "durham"], dif_urbanization[mbbs_county=="orange"], dif_urbanization[mbbs_county == "chatham"], col="lightgrey", names = c("Durham", "Orange", "Chatham")
))


filter(ijbg_class == "developed") %>%
  #group_by, because calc_freq_remove_rows requested an already grouped dataset
  group_by(mbbs_county, year, route_num, ijbg_class, totpix) %>%
  transmute(frequency = sum(frequency)) %>%
  distinct() %>% #remove now extraneous rows
  mutate(percent_developed = (frequency/totpix) * 100,
         year = as.integer(year)) %>%
  ungroup() 
