##############################
#
# Script to take the nlcd output
# and sum across grassland/agriculture
# this is not the "most" ideal as the different
# categories of grassland have different effects on 
# birds eg. agriculture may come with different effects
# but
# the nlcd's clasification abilities come with a decent degree
# of error as it relates to landcover identification
# therefore we sum these.
#
##################################

library(dplyr)

grassland <- read.csv("data/nlcd-landcover/nlcd_annual_landtype_bystop.csv") %>%
  group_by(route, stop_num, year) %>%
  filter(ijbg_class %in% 
           c("cultivated crops", "herbaceous", "shrubland", "pasture")) %>%
  mutate(perc_grassland_stop = sum(percent)) %>%
  distinct(route, stop_num, year, perc_grassland_stop) %>%
  mutate(quarter_route = case_when(stop_num > 15 ~ 4,
                                   stop_num > 10 ~ 3,
                                   stop_num > 5 ~ 2,
                                   stop_num > 0 ~ 1)) %>%
  group_by(route, quarter_route, year) %>%
  mutate(perc_grassland_quarter = mean(perc_grassland_stop)) %>%
  ungroup()

write.csv(grassland, "data/nlcd-landcover/nlcd_annual_sum_grassland.csv", row.names = FALSE)
