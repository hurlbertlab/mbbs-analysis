#Filter to only % developed
nlcd <- read.csv("data/landtype_byroutestop.csv", header = TRUE) %>%
  #filter to nlcd class of interest, here, my classification of all 'developed'
  filter(ijbg_class == "developed") %>%
  #group_by, because calc_freq_remove_rows requested an already grouped dataset
  group_by(mbbs_county, year, route_num, stop_num, ijbg_class, numpix) %>%
  transmute(frequency = sum(frequency)) %>%
  distinct() %>% #remove now extraneous rows
  mutate(percent_developed = (frequency/numpix) * 100,
         year = as.integer(year)) %>%
  ungroup() 

#write.csv(nlcd, "scratch/perc_developed_bystop.csv", row.names = FALSE)

#need to now combine all the % developed land into one row for each county/route/year
##heads up, nlcd data is missing years, assigns it the last value, only change when a new yr that has new data happens. 
add_nlcd <- function(mbbs, nlcd) {
  mbbs <- mbbs %>%
    left_join(nlcd, by = c("mbbs_county", "year", "route_num")) %>%
    group_by(route_ID, common_name) %>%
    arrange(common_name, route_ID, year) %>%
    tidyr::fill(percent_developed, .direction = "downup")
  #check <- mbbs %>% filter(route_ID == 106)
  return(mbbs)
}