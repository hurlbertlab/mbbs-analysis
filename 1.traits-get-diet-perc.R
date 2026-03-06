####################
#
# Using the avian diet database
# Get the percent of diet that is
# insects for every species on the MBBS
#
# We use the Wt_or_Vol metric in the following order:
# 1) Data is available for Wt_or_Vol for the species in spring/summer/fall. Because of a quirk in the sources for avian diet database, we combine "Arthropods" and "Unid. Animalia" which is *also* arthropods
# 2) Data is available for Wt_or_Vol for sp. in s/s/f and the species is a corvid or thrush, for which we use only "Arthropods" as "Unid. Animalia" for these species could be mammalian or worms
# 3) Data is available for Wt_or_Vol for the sp. in any season
# 4) Wt_or_Vol is not available for the species, we use the mean of the Wt_or_Vol metric for the family in s/s/f
# 5) Mean Wt_or_Vol metric for the family in any season
#
#####################

library(dplyr)
library(stringr)

#prevent scientific notation 
options(scipen=999)

# install.packages("devtools")
#devtools::install_github("ahhurlbert/aviandietdb")

#load avian diet database
library(aviandietdb)
data(dietdb)

#get taxonomy
taxonomy <- read.csv("data/species-traits/eBird_taxonomy_v2024.csv") |>
  dplyr::select(PRIMARY_COM_NAME, SCI_NAME, ORDER, FAMILY) |>
  mutate(Family = str_remove(FAMILY, "\\([^)]*\\)")) |>
  mutate(Family = str_remove_all(Family, " ")) |>
  dplyr::select(-FAMILY)

#get mbbs species list
mbbs <- read.csv("data/analysis.df.csv") |>
  group_by(common_name) |>
  summarize() |>
  left_join(taxonomy, by = c("common_name" = "PRIMARY_COM_NAME"))

#let's get the averages for each family from the wt. We'll use this for species when we have no other data. Only spring/summer/fall is prefered
family_Wt <- dietSummaryByPrey("Arthropoda", 
                               preyLevel = "Phylum", 
                               dietType = "Wt_or_Vol",
                               speciesMean = TRUE,
                               season = c("spring", "summer", "fall")) |>
  group_by(Family) %>%
  summarize(Family_Fraction_Diet_Wt = mean(Fraction_Diet),
            method = "family s/s/f")

#Hirundinidae don't have only spring/summer/fall data on weight or volume so we have to use the full time range.
hirundinidae_apodidae_Wt <- dietSummaryByPrey("Arthropoda",
                                              preyLevel = "Phylum",
                                              dietType = "Wt_or_Vol",
                                              speciesMean = TRUE) %>%
  filter(Family %in% c("Hirundinidae", "Apodidae")) %>%
  group_by(Family) %>%
  summarize(Family_Fraction_Diet_Wt = mean(Fraction_Diet),
            method = "family all seasons")

family_Wt <- bind_rows(family_Wt, hirundinidae_apodidae_Wt)

###
#okay, now to get the summary for each species in the mbbs
prey_summaries <- as.data.frame(NULL)
#no chimney swift i = 14
for(i in 1:nrow(mbbs)) {
  
  if(mbbs$common_name[i] %in% c("Chimney Swift", "Hooded Warbler", "Indigo Bunting", "Yellow-throated Warbler")) {
    #Chimney swift breaks b/c no information about them in the database at all
    #Hooded Warbler, Indigo Bunting, and Yellow-throated Warbler breaks bc doesn't have Wt or Vol measurement
  } else if (mbbs$common_name[i] %in% c("Barn Swallow", "Brown-headed Nuthatch", "Eastern Whip-poor-will", "Purple Martin", "Ruby-throated Hummingbird", "Yellow-throated Vireo")) {
    #Breaks b/c no information about them in the database for only "summer", "spring", and "fall". get diet information available from any season
    tmp <- dietSummary(mbbs$common_name[i],
                       by = "Phylum", 
                       dietType = "Wt_or_Vol") |>
      mutate(common_name = mbbs$common_name[i],
             method = "all_seasons")
    prey_summaries <- bind_rows(prey_summaries, tmp)
    
  } else {
    tmp <- dietSummary(mbbs$common_name[i],
                       by = "Phylum", 
                       season = c("summer", "spring", "fall"), 
                       dietType = "Wt_or_Vol") |>
      mutate(common_name = mbbs$common_name[i],
             method = "s/s/f")
    prey_summaries <- bind_rows(prey_summaries, tmp)
  }
  
}

#There's a key source in the avian diet database that uses only %plant food vs %animal food and doesn't break it down further taxonomically. These are sorted into Unid. Animalia which isn't part of arthropods, BUT 100% of the food in unid. animalia is actually arthropods. 
account_for_unid_animalia <- prey_summaries |>
  dplyr::select(-Prey_Part) |>
  filter(Taxon %in% c("Arthropoda", "Unid. Animalia")) |>
  #remove any Unid. Animalia for corvidae (eat animals) or thrushes (eat worms)
  left_join(taxonomy, by = c("common_name" = "PRIMARY_COM_NAME")) |>
  filter(
    !(Taxon == "Unid. Animalia" & Family %in% c("Corvidae", "Turdidae"))
  ) |>
  mutate(method = case_when(
    Family %in% c("Corvidae", "Turdidae") ~ "s/s/f Corvid.Thrush",
    TRUE ~ method
  )
  ) |>
  group_by(common_name, method) |>
  summarize(Fraction_Diet_Wt = sum(Frac_Diet))


#join everything together
diet <- mbbs |>
  left_join(family_Wt, by = c("Family")) |>
  left_join(account_for_unid_animalia, by = c("common_name")) |>
  mutate(method = case_when(
    !is.na(method.y) ~ method.y,
    TRUE ~ method.x
  )) |>
  dplyr::select(-method.x, -method.y) |>
  mutate(Final_Fraction_Diet_Wt = case_when(
    str_detect(method, "family") ~ Family_Fraction_Diet_Wt,
    TRUE ~ Fraction_Diet_Wt
    ),
    #round anything above 1 down to 1
    Final_Fraction_Diet_Wt = ifelse(Final_Fraction_Diet_Wt > 1, 1, Final_Fraction_Diet_Wt)
  ) |>
  #remove unneeded columns
  dplyr::select(-SCI_NAME, -ORDER)

hist(diet$Final_Fraction_Diet_Wt)
#awesome :)

write.csv(diet, "data/species-traits/fraction_diet_arthropods.csv", row.names = FALSE)






#################### SCRATCH
#checking correlation between different diet metrics
#insectivory by item
insectivory_Item <- dietSummaryByPrey("Arthropoda", 
                                      preyLevel = "Phylum", 
                                      dietType = "Items",
                                      speciesMean = TRUE) |>
  filter(Common_Name %in% mbbs$common_name) |>
  arrange(Common_Name)

overlap <- bind_rows(insectivory_Item, insectivory_Wt) |>
  group_by(Common_Name) |>
  summarize(n = n()) |>
  filter(n == 2)

plot(x = insectivory_Item[insectivory_Item$Common_Name %in% overlap$Common_Name, 4],
     y = insectivory_Wt[insectivory_Wt$Common_Name %in% overlap$Common_Name,4],
     xlab = "Fraction Diet (Item)",
     ylab = "Fraction Diet (Wt_or_Vol)",
     xlim = c(0, 1.5),
     ylim = c(0, 1.5))

#m, there doesn't seem to be a high correlation between them graphically
cor(insectivory_Item[insectivory_Item$Common_Name %in% overlap$Common_Name, 4], insectivory_Wt[insectivory_Wt$Common_Name %in% overlap$Common_Name,4])
#correlation is only .50
#one is not a good proxy for the other

#wellll which mbbs species don't we have by weight?
missing_mbbs_sp <- anti_join(mbbs, insectivory_Wt, by = c("common_name" = "Common_Name"))

#okay, now if we got the diet 5 cat categories, how closely does that align with the data we have? To help fill in gaps in the missing species
elton_traits <- read.delim("Z:/Databases/Elton Traits/BirdFuncDat.txt") %>%
  filter(English %in% mbbs$common_name) %>%
  select(English, Diet.Inv) %>%
  left_join(insectivory_Wt, by = c("English" = "Common_Name"))
#that got 56 of them
cor(elton_traits$Diet.Inv, elton_traits$Fraction_Diet, use = "complete.obs")
#meh it's not a perfect correlation but at .58 it's better than just using the Item amount. Probably my solution to this is going to rely on averaging by family with maybe an adjustment for it's related elton trait amount

nrow(elton_traits %>% filter(English %in% missing_mbbs_sp$common_name))
#okay but all 11 missing species are there!
