########################
#
# Get the forest specialization and grassland specialization
# Habitat association metrics from ebird status and trends
# as well as early successional forest vs mature forest if that
# distinction exists
#
########################

library(ebirdst)
library(dplyr)
library(terra)

species_list <- read.csv("data/species-traits/species_list.csv")
#okay so ebirdst_habitat() has been deprecated, reached out for help and will return to this file when I've got that further information/download in hand
pis <- read.csv("data/species-traits/ebird-habitat-association/ebirdst_predictors.csv") 
#filter only to predictors that are forest and grassland
pis <- pis[59:84,] %>%
  filter(!label %in% c("Evergreen Broadleaf Forests (% cover)", "Evergreen Broadleaf Forests (edge density)", "Deciduous Needleleaf Forests (% cover)", "Deciduous Needleleaf Forests (edge density)", "Mixed Broadleaf Evergreen/Deciduous Forests (% cover)", "Mixed Broadleaf Evergreen/Deciduous Forests (edge density)"))
#filter only to those that are relevant, eg: evergreen needleleaf (pine), mixed forests, and deciduous broadleaf, and then all the shrubland variable
#going to ignore the open forests (tree cover 30-60%) and spare forests

setpath = "Z:/Goulden/mbbs-analysis/ebird-habitat-predictor-importance/"

#Because that information was computationally expensive to generate and underused (I believe you’re the 2nd or 3rd person to contact us about these since we dropped them two versions ago), we no longer compute or provide this information in full. These were constructed from two model outputs, predictor importances and partial dependencies. The partial dependencies were expensive and have been dropped. However,  the R package still contains spatialized, weekly information on predictor importance. See the load_pi() function documentation.

?load_pi()

i = 1

for(i in 1:length(species_list$ebird_code)) {
ebirdst_download_status(species = species_list$ebird_code[i], 
                        path = setpath,
                        download_abundance = FALSE,
                        download_occurrence = FALSE,
                        download_count = FALSE,
                        download_ranges = FALSE,
                        download_regional = FALSE,
                        download_pis = TRUE,
                        download_ppms = FALSE)
}
#all downloaded successfully

c <- list_available_pis(species = "Common Grackle", path = setpath)
#so, the predictor column is going to need some interpretation
grac <- left_join(pis, c, by = "predictor")
#ok, now what needs interpretation is this "rank" value. It would be nice to use that instead of having to dig into the files themselves. PIs are ranked relative to the full suite of environmental predictors.

ex <- load_pi(species = "Common Grackle",
        predictor = pis$predictor[13], #evergreen needleleaf cover
        response = "occurrence",
        path = setpath)
terra::plot(ex$`01-26`)
global(ex$`04-26`, "mean", na.rm = TRUE)
#so here you can plot like, the weekly distribution and then, question mark, how important the predictor is that week?
#so we want to use the breeding season only, let's set that as May-July, the weeks that the MBBS surveys are running. 
#so, in this case we'd what...add up the predictor importance of the various forest predictors? average the forest predictors?
#I guess add them...
#but also, important to remember, not every species is going to have every predictor. Probably at least one forest though yeah.
#be good to know the difference btwn open forest/sparse forest and the other forests

#https://stackoverflow.com/questions/76290971/subset-spatraster-layers-return-layers-that-match-subset-names-ignore-those-th
#subset can be used to select variable names, probably that will be part of the solution here
