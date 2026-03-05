####################
#
# Using the avian diet database
# Get the percent of diet that is
# insects for every species on the MBBS
#
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
  dplyr::select(PRIMARY_COM_NAME, SCI_NAME, ORDER, FAMILY, SPECIES_GROUP)

#get mbbs species list
mbbs <- read.csv("data/analysis.df.csv") |>
  group_by(common_name) |>
  summarize() |>
  left_join(taxonomy, by = c("common_name" = "PRIMARY_COM_NAME")) |>
  mutate(Family = str_remove(FAMILY, "\\([^)]*\\)")) |>
  mutate(Family = str_remove_all(Family, " ")) |>
  dplyr::select(-FAMILY)

#let's get the averages for each family from the wt 
family_Wt <- dietSummaryByPrey("Arthropoda", 
                               preyLevel = "Phylum", 
                               dietType = "Wt_or_Vol",
                               speciesMean = TRUE) |>
  group_by(Family) %>%
  summarize(Family_Fraction_Diet = mean(Fraction_Diet))

#isectivory by weight
insectivory_Wt <- dietSummaryByPrey("Arthropoda", 
                                    preyLevel = "Phylum", 
                                    dietType = "Wt_or_Vol",
                                    speciesMean = TRUE) |>
  filter(Common_Name %in% mbbs$common_name) %>%
  arrange(Common_Name) |>
  left_join(family_Wt, by = c("Family"))

        cor(insectivory_Wt$Fraction_Diet, insectivory_Wt$Family_Fraction_Diet)
        #corr is .75, perfect. I think that's a good enough correlation to feel comfortable working with.
        m <- lm(Fraction_Diet ~ Family_Fraction_Diet, data = insectivory_Wt)
        summary(m) #r2 = .56
        plot(x = insectivory_Wt$Family_Fraction_Diet,
             y = insectivory_Wt$Fraction_Diet,
             pch = 16) +
          abline(m)

diet <- mbbs |>
  left_join(family_Wt, by = c("Family")) |>
  left_join(insectivory_Wt, by = c("common_name" = "Common_Name", "Family", "Family_Fraction_Diet")) |>
  mutate(Fraction_Diet = case_when(
    is.na(Fraction_Diet) ~ Family_Fraction_Diet,
    !is.na(Fraction_Diet) ~ Fraction_Diet
  ),
  Diet_Type = case_when(
    is.na(Diet_Type) ~ "Family_Wt_or_Vol",
    !is.na(Diet_Type) ~ Diet_Type
  ),
  Prey_Name = "Arthropoda",
  Prey_Level = "Phylum") |>
  dplyr::select(common_name, Family, Family_Fraction_Diet, Diet_Type, Fraction_Diet, Prey_Name, Prey_Level)

write.csv(diet, "data/species-traits/fraction_diet_arthropods.csv", row.names = FALSE)





####################
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
#[1] "Chimney Swift"          
#[2] "Field Sparrow"          
#[3] "Fish Crow"              
#[4] "Hooded Warbler"         
#[5] "House Finch"            
#[6] "Indigo Bunting"         
#[7] "Orchard Oriole"         
#[8] "White-breasted Nuthatch"
#[9] "Yellow-throated Warbler"


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
