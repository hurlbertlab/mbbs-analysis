###previous landfire code
#!!!!!HAS SOME ERRORS CALCULATING TOTPIXELS. Saving for posterity so I can feel secure moving on.

#add back in the route information
landfire_raw <- left_join(extracted, mbbs_buffers, by = "ID") %>%
  #get the total number of pixels on each route_stop
  group_by(ID) %>%
  mutate(numpix = n()) %>%
  ungroup() %>%
  #get the frequency of each raw_landtype by stop
  pivot_longer(cols = landfire_2001evh:landfire_2022evh, names_to = "year", names_prefix = "landfire_", values_to = "raw_landtype") %>% #pivot
  mutate(year = as.numeric(str_extract(year, "[0-9]+"))) %>% #make year a number
  group_by(County_Route_Stop, year, raw_landtype) %>%
  mutate(frequency = n()) %>%
  ungroup() %>%
  #add in route stop information
  mutate(routequarter = case_when(stop_num < 6 ~ 1,
                                  stop_num < 11 ~ 2,
                                  stop_num < 16 ~ 3,
                                  stop_num > 15 ~ 4),
         routehalf = case_when(routequarter < 3 ~ 1,
                               routequarter > 2 ~ 2)) %>% 
  #convert landtypes to 2001 style
  mutate(converted_landtype = case_when(
    year > 2014 & 
      raw_landtype > 99 & raw_landtype < 105 ~ 108,
    year > 2014 & 
      raw_landtype > 104 & raw_landtype < 111 ~ 109,
    year > 2014 & 
      raw_landtype > 110 & raw_landtype < 126 ~ 110,
    year > 2014 & 
      raw_landtype > 125 & raw_landtype < 151 ~ 111,
    year > 2014 & 
      raw_landtype > 150 & raw_landtype < 200 ~ 112,
    TRUE ~ raw_landtype
  )) %>%
  arrange(ID, year, converted_landtype) %>%
  #plenty of landtypes that we don't care about. Let's filter them out. For now, let's just focus on trees. If we want to change this later to include herbs and shrubs, filter to landtype > 100 For now, we can do more than just trees and include shrub and herb vegetation in this. 
  filter(raw_landtype > 100 & raw_landtype < 200) %>% 
  #2014 has shrub information included in the low 100s information, needs to be 108 or greater for those years
  filter(year != 2014 | (year == 2014 & raw_landtype >= 108)) %>%
  #add in total pixels for each route
  group_by(County_Route, year) %>%
  mutate(totpix_route = sum(numpix)) %>%
  ungroup() %>%
  #add in total pixels for each route quarter
  group_by(County_Route, year, routequarter) %>%
  mutate(totpix_quarter = sum(numpix)) %>%
  ungroup() %>%
  #add in route_level summaries
  group_by(County_Route, year, converted_landtype) %>%
  mutate(frequency_route = sum(frequency),
         percent_route = (frequency_route/totpix_route)*100) %>%
  ungroup() %>%
  group_by(County_Route, year) %>% 
  mutate(median_route = median(raw_landtype),
         mean_route = mean(raw_landtype),
         q3_route = quantile(raw_landtype, 0.75)) %>%
  ungroup() %>%
  #add in quarter route summaries
  group_by(County_Route, year, converted_landtype, routequarter) %>%
  mutate(frequency_quarter = sum(frequency),
         percent_quarter = (frequency_quarter/totpix_quarter)*100) %>%
  ungroup() %>%
  group_by(County_Route, year, routequarter) %>%
  mutate(median_quarter = median(raw_landtype),
         mean_quarter = mean(raw_landtype),
         q3_quarter = quantile(raw_landtype, 0.75)) %>%
  ungroup()

#next step is to get the 2022-2016 differences. We'll do this first by route
values_year <- function(landfire_raw, select_year) {
  values_year <- landfire_raw %>% 
    group_by(County_Route) %>%
    filter(year == select_year) %>%
    distinct(County_Route, .keep_all = TRUE)
}

values_year_quarter <- function(landfire_raw, select_year) {
  values_year <- landfire_raw %>% 
    group_by(County_Route, routequarter) %>%
    filter(year == select_year) %>%
    distinct(County_Route, .keep_all = TRUE)
}

v2016 <- values_year(landfire_raw, 2016) %>%
  rename(y16mean_route = mean_route) %>%
  rename(y16median_route = median_route) %>%
  select(County_Route, y16mean_route, y16median_route)

v2022 <- values_year(landfire_raw, 2022) %>%
  rename(y22mean_route = mean_route) %>%
  rename(y22median_route = median_route) %>%
  select(County_Route, y22mean_route, y22median_route)

#okay good! comparing between those two, values get higher. 
dif <- left_join(v2016, v2022, by = "County_Route") %>%
  mutate(difmean_22_16 = y22mean_route - y16mean_route,
         difmedian_22_16 = y22median_route - y16median_route)

#by routequarter
v2016 <- values_year_quarter(landfire_raw, 2016) %>%
  rename(y16mean_quarter = mean_quarter) %>%
  rename(y16median_quarter = median_quarter) %>%
  select(County_Route, routequarter, y16mean_quarter, y16median_quarter)

v2022 <- values_year_quarter(landfire_raw, 2022) %>%
  rename(y22mean_quarter = mean_quarter) %>%
  rename(y22median_quarter = median_quarter) %>%
  select(County_Route, routequarter, y22mean_quarter, y22median_quarter)

#okay good! comparing between those two, values get higher. 
dif_quarter <- left_join(v2016, v2022, by = c("County_Route", "routequarter")) %>%
  mutate(difmeanquarter_22_16 = y22mean_quarter - y16mean_quarter,
         difmedianquarter_22_16 = y22median_quarter - y16median_quarter)