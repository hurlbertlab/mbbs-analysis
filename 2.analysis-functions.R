#
#
#filter out species that haven't been seen more than the min number of times on the min number of routes.
#
#
filter_to_min_sightings <- function(mbbs, min_sightings_per_route = 9, min_num_routes = 5) {
  
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


###filter to min sightings but for quarter route observations
filter_to_min_qrts <- function(mbbs, min_quarter_routes) {
  
  species_select <- mbbs %>%
    group_by(common_name, quarter_route) %>% 
    summarize(nyears = sum(q_rt_count > 0)) %>% #mid step that can be examined if interested
    ungroup() %>% 
    group_by(common_name) %>% 
    filter(quarter_route > 0) %>% 
    summarize(nqr = sum(nyears > 0)) %>%
    #filter to species with the minimum number of quarter routes where they've been observed at least once
    filter(nqr >= min_quarter_routes)
  
  #filter the mbbs to only the species that meet the minimum, return the mbbs
  mbbs <- mbbs %>% filter(common_name %in% species_select$common_name)
  
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
    left_join(mbbs_survey_events, by = c("route", "year")) #%>%
    #dplyr::select(-route_num.y) %>%
    #rename(route_num = route_num.x)
  
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
#Exclude hawks and owls, waterbirds, and categories that are not species-specific.
excluded_species <- c("Red-shouldered Hawk", "Killdeer", "Great Blue Heron", "Canada Goose", "Turkey Vulture", "Black Vulture", "crow sp.","duck sp.","hawk sp.","passerine sp.", "swallow sp.","waterfowl sp.","woodpecker sp.", "Summer/Scarlet Tanager", "Sharp-shinned/Cooper's Hawk", "Mute Swan", "Mississippi Kite", "Mallard", "Green Heron","Great Horned Owl", "Great Egret", "Eastern Screech-Owl", "Double-crested Cormorant", "Cooper's Hawk" , "Sharp-shinned Hawk", "Broad-winged Hawk", "Belted Kingfisher", "Barred Owl", "American/Fish Crow", "Accipitrine hawk sp.", "Yellow-crowned Night Heron", "Wood Duck", "Osprey", "Bald Eagle", "Red-tailed Hawk")



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



#' Add trait data
#' @returns the imput dataset with all the traits listed in the function left_joined in
add_all_traits <- function(mbbs) {
  
  #read in trait files
  ##########################################
  gdic_diet <- read.csv("data/species-traits/gdicecco-avian-range-shifts/diet_niche_breadth_mbbs.csv") %>%
    dplyr::select(english_common_name, shannonE_diet)
  
  gdic_climate <- read.csv("data/species-traits/gdicecco-avian-range-shifts/climate_niche_breadth_mbbs.csv") %>%
    dplyr::select(english_common_name, climate_vol_2.1)
  
  gdic_habitat <- read.csv("data/species-traits/gdicecco-avian-range-shifts/habitat_niche_ssi_true_zeroes.csv") %>%
    dplyr::select(english_common_name, ssi) %>%
    #for now, we're kinda doing a mock ssi bc we don't have all the species. Stan does not support NAs in data, so let's change all the ssi to 1. It's not interpretable with only half the data and half mock data anyway
    mutate(habitat_ssi = ssi) %>% #change name for interpret-ability 
    dplyr::select(-ssi)
  
  regional <- read.csv("data/bbs-regional/species-list-usgs-regional-trend.csv") %>%
    dplyr::select(-done, -running, -notes) 
  
  #NOTE: might need to *.001 regional to make it more interpretable? mayybeeee.. just bc output is eg. 0.06 for a 6% change and for regional that same change would be 6.00
  climate_position <- read.csv("data/species-traits/climate_position.csv") %>%
    dplyr::select(common_name, climate_position)
  
  habitat_selection <- read.csv("data/species-traits/ndvi_habitat_selection.csv")
  #############################################
  
  
  #left join trait files
  mbbs_traits <- mbbs %>%
    #shannonE_diet
    left_join(gdic_diet, by = c("common_name" = "english_common_name" )) %>%
    #climate_vol_2.1
    left_join(gdic_climate, by = c("common_name" = "english_common_name")) %>%
    #habitat_ssi
    left_join(gdic_habitat, by = c("common_name" = "english_common_name")) %>%
    #climate_position
    left_join(climate_position, by = "common_name") %>%
    #local habitat selection (mean ndvi of stops observed on)
    left_join(habitat_selection, by = "common_name") %>%
    left_join(regional, by = "common_name") %>%
    #Recreate IDs for common name
    group_by(common_name) %>%
    mutate(common_name_standard = cur_group_id()) %>%
    ungroup() 
  
  #return
  mbbs_traits
  
}


#' make_testing_df
#' Function to trim down the mbbs to just a few test species
#' @returns a trimmed down version of the mbbs, filtered to just a few species
make_testing_df <- function(mbbs, obs_only = FALSE) {
  
  filtered_mbbs <- mbbs %>% filter(common_name %in% c("Wood Thrush", "Acadian Flycatcher", "Northern Bobwhite", "White-eyed Vireo", "Tufted Titmouse")) %>%
    #Recreate IDs for common name
    group_by(common_name) %>%
    mutate(common_name_standard = cur_group_id()) %>%
    ungroup() %>%
    #Recreate IDs for primary observer
    group_by(primary_observer) %>%
    mutate(observer_ID = cur_group_id()) %>%
    ungroup() 
  
  if (obs_only == FALSE) {
    filtered_mbbs <- filtered_mbbs %>%
      #recreate IDs for diet
      group_by(avonet_diet) %>%
      mutate(diet_category_standard = cur_group_id()) %>%
      ungroup() 
  }

  filtered_mbbs

}


#' Creates a fixed effect (numeric value) of observer quality, which reflects
#' (observer's mean on this route - mean richness of years they are not one of the obs1-3)/
#' (mean richness of years they are not one of the obs1-3) ie:
#' (x-y)/y
#' observer_quality = max(obs1_quality, obs2_quality, obs3_quality, na.rm = TRUE)
#' Corrects for cases where a one-time observer accompanied a more experienced observer
#' and saw a high number of species in a particularly good year (putting their quality
#' above that of the more experienced observer)
#' @importFrom dplyr
#'  group_by summarize filter ungroup left_join first
#'  relocate mutate rename select rowwise case_when n
#' @importFrom tidyr pivot_longer
#' @param mbbs_survey_events a dataframe with the list of survey events, importantly needs to include information about number of species and the observers for each survey
get_observer_quality <- function(mbbs_survey_events) {
  # goal is to create a fixed effect of observer quality
  
  # table of average n species seen on each route
  S_average_route <-
    mbbs_survey_events %>%
    group_by(route) %>%
    summarize(
      route_meanS = mean(total_species),
      n_surveys_route = n()
    ) %>%
    ungroup()
  
  # summary of number of mean(S) across routes for each observer,
  # + n surveys they've done
  observer_average <- mbbs_survey_events %>%
    # obs1/obs2/obs3 don't matter now
    tidyr::pivot_longer(obs1:obs3, values_to = "obs") %>%
    filter(!is.na(obs)) %>%
    # group by just how many times the observer has surveyed at all
    group_by(obs) %>%
    summarize(
      obs_meanS = mean(total_species),
      n_surveys_obs = n()
    ) %>%
    ungroup()
  
  # Calculate proportion deviation from mean species of other observers
  # on the route for each observer
  observer_average_route <-
    mbbs_survey_events %>%
    tidyr::pivot_longer(obs1:obs3, values_to = "obs") %>%
    filter(!is.na(obs)) %>%
    group_by(route, obs) %>%
    summarize(
      obsroute_meanS = mean(total_species),
      n_surveys_obsroute = n()
    ) %>%
    ungroup() %>%
    # left join dfs we created above
    left_join(S_average_route, by = c("route")) %>%
    left_join(observer_average, by = c("obs")) %>%
    relocate(
      n_surveys_obsroute,
      n_surveys_obs,
      .after = "obs_meanS"
    ) %>%
    # meanS on route in years not run by that row's observer,
    # back calculated with means,
    # essentially
    # removing the observer's proportion of contribution towards the route_meanS
    mutate(
      non_focal_obsroute_meanS =
        ((route_meanS * n_surveys_route) -
           (obsroute_meanS * n_surveys_obsroute)) /
        (n_surveys_route - n_surveys_obsroute)
    )
  
  # use all the information we have about an observer to calculate how they do
  # compared to all other observers on the mbbs.
  # On each survey they've done, calculate their performance as follows:
  # (obsroute_year_S(route, year, observer) - non_focal_obsroute_meanS)
  # / non_focal_obsroute_meanS
  # If an observer has 5 route_years they've run, they will have 5 rows of data
  # and each row will contain a comparison score of how well they did that year
  # on that route
  # compared to every other year that route has been done by someone else.
  # To get their total performance or observer quality across all years and routes
  # group by observer and average their route_year observer qualities.
  # This method also produces a value where routes the observer has run more
  # frequently have more impact on their observer quality.
  # and we have more information about their performance than a single datapoint
  # for each route they've participated in.
  observer_year_average_route <-
    mbbs_survey_events %>%
    tidyr::pivot_longer(obs1:obs3, values_to = "obs") %>%
    filter(!is.na(obs)) %>%
    group_by(route, obs, year) %>%
    summarize(
      obsroute_year_S = total_species
    ) %>%
    # left join the above dataframes bc we need some of that info
    left_join(observer_average_route, by = c("obs", "route")) %>%
    mutate( # create comparison score within the route year
      obs_yr_rt_quality =
        (obsroute_year_S - non_focal_obsroute_meanS) / non_focal_obsroute_meanS,
      obs_yr_rt_quality =
        ifelse(is.nan(obs_yr_rt_quality), 0, obs_yr_rt_quality)
    )
  
  # group by observer and take the mean quality from all their route-years
  # to get the average quality of each observer
  # proportion of species each observer observes relative
  # to what other people observe on their route(s)
  # using data from ALL their years of observations (each route-year combo)
  # instead of taking the average in each route and then averaging that (prev way)
  # which (prev way) gave equal influence to a route run once and a route run 15x
  # in (prev way) determining obs_quality
  # Interpretation is as follows:
  # a -0.08 observer quality means that observer on average sees 8% fewer
  # species on their route(s) compared to the average number of species
  # seen on their route(s) in years run by other observers
  observer_quality <-
    observer_year_average_route %>%
    group_by(obs) %>%
    summarize(
      obs_quality = mean(obs_yr_rt_quality),
      n_surveys_obs = first(n_surveys_obs)
    ) %>%
    ungroup()
  
  # assign observer_quality based on the performance of the top observer
  mbbs_survey_events <-
    mbbs_survey_events %>%
    # add obs1_deviation
    left_join(
      observer_quality,
      by = c("obs1" = "obs")
    ) %>%
    mutate(
      obs1_quality = obs_quality,
      obs1_nsurveys = n_surveys_obs
    ) %>%
    select(-c(obs_quality, n_surveys_obs)) %>%
    # add obs2_deviation
    left_join(observer_quality, by = c("obs2" = "obs")) %>%
    mutate(
      obs2_quality = obs_quality,
      obs2_nsurveys = n_surveys_obs
    ) %>%
    select(-c(obs_quality, n_surveys_obs)) %>%
    # add obs3 deviation
    left_join(observer_quality, by = c("obs3" = "obs")) %>%
    mutate(
      obs3_quality = obs_quality,
      obs3_nsurveys = n_surveys_obs
    ) %>%
    select(-c(obs_quality, n_surveys_obs)) %>%
    rowwise() %>%
    # Get the maximum observer_quality between obs1, obs2, obs,
    # and which column it comes from
    mutate(
      # observer quality is max btwn the three obs deviations
      observer_quality =
        max(obs1_quality, obs2_quality, obs3_quality,
            na.rm = TRUE
        ),
      # record which observer was the best
      max_qual_observer =
        which.max(c(obs1_quality, obs2_quality, obs3_quality)),
      observer_quality = case_when(
        # if there's only one observer
        # don't change obs_quality
        (sum(is.na(c(obs1, obs2, obs3)) == FALSE) == 1) ~ observer_quality,
        # if obs 1 is the best observer but only has one survey across all routes
        # take max of obs2 and obs3
        (max_qual_observer == 1 & obs1_nsurveys == 1) ~ suppressWarnings(max(obs2_quality, obs3_quality, na.rm = TRUE)),
        # if obs 2 is the best observer but only has one survey across all routes
        # take max of obs1 and obs3
        (max_qual_observer == 2 & obs2_nsurveys == 1) ~ suppressWarnings(max(obs1_quality, obs3_quality, na.rm = TRUE)),
        # if obs 3 is the best observer but only has one survey across all routes
        # take max of obs1 and obs2
        (max_qual_observer == 3 & obs3_nsurveys == 1) ~ suppressWarnings(max(obs1_quality, obs2_quality, na.rm = TRUE)),
        # if none of the other statements are true, leave obs_quality the same
        TRUE ~ observer_quality
      )
    ) %>%
    ungroup() %>%
    # assign primary observer and observer ID based on the top observer
    mutate(primary_observer = case_when(
      max_qual_observer == 1 ~ obs1,
      max_qual_observer == 2 ~ obs2,
      max_qual_observer == 3 ~ obs3
    )) %>%
    dplyr::relocate(primary_observer, .before = "obs1") %>%
    group_by(primary_observer) %>%
    mutate(observer_ID = cur_group_id()) %>%
    ungroup()
  
  # return
  mbbs_survey_events
}
