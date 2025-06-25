##########################
#
# Testing the effects of species traits
# on how strongly a species' count changes
# in response to change in perc. developed
# AND percent forest. Test both here.
#
#
## traits:
# temperature niche position (where NC is in the species temperature range)
# ebird-derived forest selectivity
# ebird-derived grasslands selectivity
# Neate-Clegg et al. Urban Association Index
#
#########################

library(dplyr)
library(rstan)
library(stringr)
unloadNamespace("rethinking") #just in case, can interfere


#load in dataframes
###################################
#Get the b_dev data from the last run of the first step of the landcover modeling
load_from <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.06.20_cpc_forestndev/"
species_list <- read.csv(paste0(load_from, "species_list.csv")) %>%
  dplyr::select(-X)


#climate 
climate <- read.csv("data/species-traits/climate_position.csv") %>%
  dplyr::select(common_name, z_tempwq)

#UAI
uai <- read.csv("data/species-traits/UAI-NateCleg-etall.csv") %>%
  dplyr::filter(City == "Charlotte_US")

#ebird-habitat data
#habitat_select <- read.csv("data/species-traits/ebird-habitat-association/forest-grass-habitat-associations.csv")
habitat_select <- read.csv("data/species-traits/species_list.csv") %>%
  dplyr::select(common_name, ebird_code, ebirdst_association_forest, ebirdst_association_grassland)

#add all to species_list
df <- species_list %>%
  left_join(climate, by = "common_name") %>%
  left_join(habitat_select, by = "common_name") %>%
  left_join(uai, by = c("common_name" = "Species"))

#need to fix Northern House Wren in UAI

