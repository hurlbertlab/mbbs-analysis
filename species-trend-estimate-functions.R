#
#
#filter out species that haven't been seen more than the min number of times on the min number of routes.
#
#
filter_to_min_sightings <- function(mbbs, min_sightings_per_route, min_num_routes) {
  
  #set up for for loop
  occurances <- mbbs %>% ungroup() %>% count(common_name, route_ID) %>% arrange(n)
  allspecies <- unique(mbbs$common_name)
  temp_occurances <- occurances %>% filter(common_name == "Acadian Flycatcher") #temp for use in for loop
  temp_num <- n_distinct(temp_occurances$route_ID) #for use in for loop, really this is also the nrow(temp_occurances) but that's ok. 
  
  #for loop to filter species that haven't been seen enough, the minimum number of times on a minimum number of routes 
  for (s in 1:length(allspecies)) {
    
    temp_occurances <- occurances %>% filter(common_name == allspecies[s])
    temp_num <- n_distinct(temp_occurances$route_ID)
    
    if(temp_num >= min_num_routes) { #this species has been seen on the minimum number of routes
      #check that the species has been seen the minimum number of TIMES on those routes
      #so, count the n values over min_sightings_per_route
      temp_num <- sum(temp_occurances$n >= min_sightings_per_route)
      if(temp_num > min_num_routes) {
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
make_trend_table <- function(cols_list) {
  #make trend table
  trend_table <- as.data.frame(matrix(ncol = length(cols_list), nrow = 0))
  #name the columns
  colnames(trend_table) <- cols_list
  #initiate trend table with first row, fill with NAs
  trend_table[1,] <- NA
  
  return(trend_table)
  
}
