##########################
# run a quick lm on trends predicted by traits
#########################

library(tidyr) #for pivot
library(dplyr)
library(stringr)


#get regional trends from the usgs data instead of the regional_trendWIP
reg_trends <- read.csv("regional_trends_prep/species-list.csv")
#trends from the bayes model
mbbs_trends <- read.csv("data/bayes_hierarchical_year_results_61sp.csv")
#translate bird codes
birdcode <- read.csv("data/bird_code_to_common_name.csv") %>%
  dplyr::select(common_name, species_code)
#add in species traits
diet <- read.csv("data/species-traits/gdicecco-avian-range-shifts/diet_niche_breadth.csv")
habitat <- read.csv("data/species-traits/gdicecco-avian-range-shifts/habitat_niche_ssi.csv")
climate <- read.csv("data/species-traits/gdicecco-avian-range-shifts/spp_allvars.csv")

trendstraits <- left_join(mbbs_trends, reg_trends, by = "common_name") %>%
  mutate(mean = mean*100,
         X5.5. = X5.5.*100,
         X94.5. = X94.5.*100,
         difference = mean-usgs_trend_estimate) %>%
  filter(parameter != "a_bar",
         parameter != "sigma") %>%
  left_join(birdcode, by = "common_name") %>%
  left_join(diet, by = c("common_name" = "english_common_name")) %>%
  left_join(climate, by = c("aou", "shannonE_diet"))

#okay so, gdicecco avian range shifts data only has 30 species out of the ones I was expecting to have. So, there's some work for me to do in yes, running these stats myself. Should get the same answers for the species we already have, that's the double check, and otherwise yeah need to create it for the species don't have it for yet. 

#ssi is habitat niche
test <- lm(mean ~ usgs_trend_estimate + shannonE_diet + climate_vol + ssi, data = trendstraits)
summary(test)  

