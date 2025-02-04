#----
#
# Merge NC species traits csv with cat predation csv from Cooper et al. 2012
#
#----

traits <- read.csv("data/NC_species_traits.csv", header = T)
cats <- read.csv("data/Cooper_2012_catpredation.csv", header = T)
colnames(cats)

library(dplyr)

traits_wcats <- left_join(traits, cats, by = c('english_common_name'='common.name'))

traits_wcats <- traits_wcats %>% select(!genus.species)

write.csv(traits_wcats, "data/NC_species_traits_wcats.csv", row.names = F)
