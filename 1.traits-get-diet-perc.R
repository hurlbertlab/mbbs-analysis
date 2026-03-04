####################
#
# Using the avian diet database
# Get the percent of diet that is
# insects for every species on the MBBS
#
#
#####################

# install.packages("devtools")
#devtools::install_github("ahhurlbert/aviandietdb")

library(aviandietdb)
data(dietdb)

#get mbbs species list
mbbs <- read.csv("data/bbs-regional/species-list.csv")

insectivory_Item <- dietSummaryByPrey("Arthropoda", 
                                 preyLevel = "Phylum", 
                                 dietType = "Items",
                                 speciesMean = TRUE) |>
  filter(Common_Name %in% mbbs$common_name) |>
  arrange(Common_Name)

insectivory_Wt <- dietSummaryByPrey("Arthropoda", 
                                    preyLevel = "Phylum", 
                                    dietType = "Wt_or_Vol",
                                    speciesMean = TRUE) |>
  filter(Common_Name %in% mbbs$common_name) %>%
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
#oh! oh wait these aren't sorted!
cor(insectivory_Item[insectivory_Item$Common_Name %in% overlap$Common_Name, 4], insectivory_Wt[insectivory_Wt$Common_Name %in% overlap$Common_Name,4])
#correlation is only .48
#one is not a good proxy for the other

#wellll which mbbs species don't we have by weight?
missing_mbbs_sp <- anti_join(mbbs, insectivory_Wt, by = c("common_name" = "Common_Name"))
#[1] "Canada Goose"           
#[2] "Chimney Swift"          
#[3] "Field Sparrow"          
#[4] "Fish Crow"              
#[5] "Hooded Warbler"         
#[6] "House Finch"            
#[7] "Indigo Bunting"         
#[8] "Orchard Oriole"         
#[9] "Red-shouldered Hawk"    
#[10] "White-breasted Nuthatch"
#[11] "Yellow-throated Warbler"

#so really what we care about that's missing is:
#Chimney Swift, Field Sparrow, Fish Crow (probably same as American Crow.. 0), Hooded Warbler, House Finch, Indigo Bunting, Orchard Oriole, White-breasted Nuthatch, Yellow-throated Warbler

#okay, now if we got the diet 5 cat categories, how closely does that align with the data we have? To help fill in gaps in the missing species
elton_traits <- read.delim("Z:/Databases/Elton Traits/BirdFuncDat.txt") %>%
  filter(English %in% mbbs$common_name) %>%
  select(English, Diet.Inv) %>%
  left_join(insectivory_Wt, by = c("English" = "Common_Name"))
#that got 56 of them
  cor(elton_traits$Diet.Inv, elton_traits$Fraction_Diet, use = "complete.obs")
  #meh it's not a perfect correlation but at .60 it's better than just using the Item amount. Probably my solution to this is going to rely on averaging by family with maybe an adjustment for it's related elton trait amount

nrow(elton_traits %>% filter(English %in% missing_mbbs_sp$common_name))
#okay but all 11 missing species are there!

