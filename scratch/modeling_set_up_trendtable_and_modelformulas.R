#########################################
#
# How to set up a trend table,
# scrap from prev. versions of species-trend-estimates-allmodels
# when using poisson/gee models.
#
#########################################

#------------------------------------------------------------------------------
# Set up for modeling
#------------------------------------------------------------------------------
# 
# #read in analysis df
# mbbs <- read.csv("data/analysis.df.csv", header = TRUE)
# 
# #set up for modeling
# species_list <- unique(mbbs$common_name)
# filtered_mbbs <- mbbs %>% filter(common_name == species_list[1])
# 
# #create trend table to store results in
# cols_list <- c("common_name")
# trend_table <- make_trend_table(cols_list, species_list)
# 
# #make route_ID a factor
# mbbs <- mbbs %>% mutate(route_ID = as.factor(route_ID))
# mbbs_nozero <- mbbs_nozero %>% mutate(route_ID = as.factor(route_ID))
# 
# #add in UAI
# uai <- read.csv("data/species-traits/UAI-NateCleg-etall.csv") %>%
#   filter(City == "Charlotte_US")
# 
# mbbs <- mbbs %>%
#   left_join(uai, by = c("common_name" = "Species"))
# 
# #formulas to plug into the models
#   f_base <- count ~ year  
#   f_pd <- update(f_base, ~ . + percent_developed)
#   f_obs <- update(f_base, ~ . + observer_quality)
#   f_pdobs <- update(f_base, ~ . + percent_developed + observer_quality)
#   f_wlandfire <- update(f_pdobs, ~. + lf_mean_route)