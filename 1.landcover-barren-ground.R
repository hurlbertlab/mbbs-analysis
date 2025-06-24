################################
#
# Calculate the amount of barren ground at each stop
# we are going to add this on top of our running_max_developed
# as the typical path to development is forest -> clearcut (bare ground) -> developed
# but sometimes it does, also, go forest -> clearcut (bare ground) -> back to forest.
# but this clearcut land is not great for the birds in that year and represents the start of the edge effects
# that accompany development
#
#
##################################

library(dplyr)

barren <- read.csv("data/nlcd-landcover/nlcd_annual_landtype_bystop.csv") %>%
  group_by(route, stop_num, year) %>%
  filter(ijbg_class == "barren") %>%
  mutate(perc_barren = sum(percent)) %>%
  #keep only one value for each route/stop/year
  distinct(route, stop_num, year, perc_barren) %>%
  ungroup() %>%
  group_by(route, stop_num) %>%
  arrange(year)

#not all year-route-stop_num combos have barren ground, it really is just a sometimes landcover type.

write.csv(barren, "data/nlcd-landcover/nlcd_annual_barren.csv", row.names = FALSE)
