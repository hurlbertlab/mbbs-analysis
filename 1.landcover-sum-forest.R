##############################
#
# Script to take the nlcd output
# and sum across forest types 
# this is not the "most" ideal as the different
# categories of forests have different effects on 
# birds eg. deudicious supports more food compared
# to pine or mixed forest
# but
# the nlcd's clasification abilities come with a decent degree
# of error as it relates to forest type identification
# and attempts to truth it against satelite data for a couple
# of stops showed that at this scale
# those errors would probably affect conclusions
# therefore all forest types are summed.
#
# after summing
# this script also creates
# visual plots of change over time for each stop or quarter stop
#
##################################

library(dplyr)

forest <- read.csv("data/nlcd-landcover/nlcd_annual_landtype_bystop.csv") %>%
  group_by(route, stop_num, year) %>%
  filter(ijbg_class %in% 
           c("deciduous forest", "mixed forest", "evergreen forest")) %>%
  mutate(perc_forest_stop = sum(percent)) %>%
  distinct(route, stop_num, year, perc_forest_stop) %>%
  mutate(quarter_route = case_when(stop_num > 15 ~ 4,
                                   stop_num > 10 ~ 3,
                                   stop_num > 5 ~ 2,
                                   stop_num > 0 ~ 1)) %>%
  group_by(route, quarter_route, year) %>%
  mutate(perc_forest_quarter = mean(perc_forest_stop)) %>%
  ungroup()

  write.csv(forest, "data/nlcd-landcover/nlcd_annual_sum_forest.csv", row.names = FALSE)

#now plot
#the thing to look out for is dips and then peaks for several years, indicative of an overall pixel categorization / some kinda error. There, we ..mayy.. wind up applying some kind of smoothing

