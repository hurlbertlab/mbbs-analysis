#####################################
# 
# Adding in landcover information (nlcd and landfire)
# with leftjoining to mbbs data.
#
####################################

#---------------------------------
#Add in landcover information
#---------------------------------

#leftjoin for landcover information
#read in nlcd data, filter to just what we're interested in. Otherwise it's a many-to-many join relationship. Right now, just the % developed land. Workflow similar to "calc_freq_remove_rows()" from the generate_percent_change_+_map.. code, but altered for this use.
nlcd <- read.csv("data/landtype_byroute.csv", header = TRUE) %>%
  #filter to nlcd class of interest, here, my classification of all 'developed'
  filter(ijbg_class == "developed") %>%
  #group_by, because calc_freq_remove_rows requested an already grouped dataset
  group_by(mbbs_county, year, route_num, ijbg_class, totpix) %>%
  transmute(frequency = sum(frequency)) %>%
  distinct() %>% #remove now extraneous rows
  mutate(percent_developed = (frequency/totpix) * 100,
         year = as.integer(year)) %>%
  ungroup()

#read in landfire information
landfire <- read.csv("data/landfire_byroute.csv", header = TRUE) %>%
  #right now, I'm only concerned here with means. So I want to just have one bit of information for each route, we don't need to keep all the different percent landtypes. If we wanted to do that, we could pivot wider based on landtype and get the % frequency for each category.
  distinct(mbbs_county, route_num, lf_mean_route, lf_median_route, lf_year, lf_q3_route, lf_difmean_22_16)

landfire$mbbs_county <- str_to_lower(landfire$mbbs_county)


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
mbbs <- add_nlcd(mbbs, nlcd)
mbbs_nozero <- add_nlcd(mbbs_nozero, nlcd)

add_landfire <- function(mbbs, landfire) {
  mbbs <- mbbs %>%
    left_join(landfire, by = c("mbbs_county", "route_num", "year" = "lf_year")) %>%
    group_by(route_ID, common_name) %>%
    arrange(common_name, route_ID, year) %>%
    tidyr::fill(lf_mean_route, lf_median_route, lf_q3_route, lf_difmean_22_16, .direction = "downup")
}

mbbs <- add_landfire(mbbs,landfire)
mbbs_nozero <- add_landfire(mbbs_nozero, landfire)
