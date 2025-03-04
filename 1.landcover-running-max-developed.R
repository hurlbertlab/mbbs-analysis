##############################
#
# Take the nlcd annual landcover categories
# and create a running max for each stop/year of the % developed pixels.
# This removes small dips in percent developed in the data
# that are more representative of measure error than a true 
# 'decline' in the percent of area around each stop that has been developed.
# With knowledge of this area, the research triangle of NC, it's safe to say
# that over the last 25 years developed has only increased.
#
###############################

library(dplyr)

landcover <- read.csv("data/nlcd-landcover/nlcd_annual_landtype_bystop.csv") %>%
  group_by(route, stop_num, year) %>%
  filter(ijbg_class == "developed") %>%
  #sum all the developed percents into one group
  mutate(perc_ijbg_developed = sum(percent)) %>%
  #keep only one value for each route/stop/year
  distinct(route, stop_num, year, perc_ijbg_developed) %>%
  ungroup() %>%
  group_by(route, stop_num) %>%
  #ought to be already arranged by year but make sure just in case
  arrange(year) %>% 
  mutate(running_max_perc_developed = cummax(perc_ijbg_developed)) %>%
  ungroup()

write.csv(landcover, "data/nlcd-landcover/nlcd_annual_running_max_developed.csv", row.names = FALSE)
