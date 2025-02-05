#
#
#filter out species that haven't been seen more than the min number of times on the min number of routes.
#
#
filter_to_min_sightings <- function(mbbs, min_sightings_per_route, min_num_routes) {
  
  #filter mbbs so we only have records where count is not 0
  mbbs <- mbbs %>%
    dplyr::filter(count > 0)
  
  #set up for for loop
  occurances <- mbbs %>% ungroup() %>% count(common_name, route) %>% arrange(n)
  allspecies <- unique(mbbs$common_name)
  temp_occurances <- occurances %>% filter(common_name == "Northern Bobwhite") #temp for use in for loop
  temp_num <- n_distinct(temp_occurances$route) #for use in for loop, really this is also the nrow(temp_occurances) but that's ok. 
  
  #for loop to filter species that haven't been seen enough, the minimum number of times on a minimum number of routes 
  for (s in 1:length(allspecies)) {
    
    temp_occurances <- occurances %>% filter(common_name == allspecies[s])
    temp_num <- n_distinct(temp_occurances$route)
    
    if(temp_num >= min_num_routes) { #this species has been seen on the minimum number of routes
      #check that the species has been seen the minimum number of TIMES on those routes
      #so, count the n values over min_sightings_per_route
      temp_num <- sum(temp_occurances$n >= min_sightings_per_route)
      if(temp_num >= min_num_routes) {
        #do nothing, the species meet the minimum sighting requirements and should stay in the route
      } else {
        #the species does not meet the minimum sighting requirements and should be removed from analysis
        mbbs <- mbbs %>% filter(common_name != temp_occurances$common_name[1]) #remove species from datatable
      }
      
    } else { #this species hasn't been seen on the minimum number of routes required for analysis
      mbbs <- mbbs %>% filter(common_name != temp_occurances$common_name[1]) #remove species from datatable
    }
  }
  
  beepr::beep()
  return(mbbs)
  
}


#
#
# Function to create and initialize a trend table, based off a list of the names of columns
#
#
make_trend_table <- function(cols_list, rows_list = c("NA")) {
  #make trend table
  trend_table <- as.data.frame(matrix(ncol = length(cols_list), nrow = 0))
  #name the columns
  colnames(trend_table) <- cols_list
  #initiate trend table with first row, fill with NAs
  trend_table[1,] <- NA
  
  #if first column (rows_list) is not NA, fill it in
  if(rows_list[1] == "NA") {
    #do nothing
  } else {
    #add in the list that makes up the data in the first column
      for(s in 1:length(rows_list)) {
        
        trend_table[s,1] <- rows_list[s]
        
      }
  }
  
  return(trend_table)
  
}

#
#
# Function to left_join mbbs_survey events to a mbbs dataframe
#
#
add_survey_events <- function(mbbs, mbbs_survey_events) {
  mbbs <- mbbs %>% 
    left_join(mbbs_survey_events, by = c("route", "year")) %>%
    dplyr::select(-route_num.y) %>%
    rename(route_num = route_num.x)
  
  return(mbbs)
} 



# Function to transform year to a variable where year 1 (1999, 2000, or 2002) is 1
# rather than the raw year itself. 
# 
standardize_year <- function(mbbs, starting_year = 1999) {
  
  mbbs <- mbbs %>%
    mutate(year_standard = year - (starting_year - 1))
  
}


# Function to leftjoin the species_trait variables
# Right now, these are dummy variables made within the function for testing purposes.
#add_test_traits <- function(mbbs) {
  
#  common_name <- (unique(mbbs$common_name))
  
#  mock_diet
  
#} pause on this for now. I like the idea, but my testing stuff I'm just going to set things for WOTH and ACFL. 

# variables
excluded_species <- c("Red-shouldered Hawk", "Killdeer", "Great Blue Heron", "Canada Goose", "Turkey Vulture", "Black Vulture")



#' Create a unique route ID for a county/route
#'
#' @importFrom dplyr if_else
make_route <- function(county, route_num) {

  f <- \(x) paste(x, sprintf("%02d", route_num), sep = "-")
  
  dplyr::if_else(
    county == "orange",
    f("orng"),
    dplyr::if_else(county == "chatham",
                   f("cthm"),
                   f("drhm")
    )
  )
}
