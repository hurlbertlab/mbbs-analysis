########################
#
# Get the forest specialization and grassland specialization
# Habitat association metrics from ebird status and trends
# as well as early successional forest vs mature forest if that
# distinction exists
#
########################

library(ebirdst)

species_list <- read.csv("data/species-traits/species_list.csv")
#okay so ebirdst_habitat() has been deprecated, reached out for help and will return to this file when I've got that further information/download in hand

#Because that information was computationally expensive to generate and underused (I believe you’re the 2nd or 3rd person to contact us about these since we dropped them two versions ago), we no longer compute or provide this information in full. These were constructed from two model outputs, predictor importances and partial dependencies. The partial dependencies were expensive and have been dropped. However,  the R package still contains spatialized, weekly information on predictor importance. See the load_pi() function documentation.

?load_pi()

list_available_pis("Louisiana Waterthrush")

i = 1

for(i in 42:length(species_list$common_name)) {
ebirdst_download_status(species = species_list$common_name[i], 
                        path = "Z:/Goulden/mbbs-analysis/ebird-habitat-predictor-importance/",
                        download_abundance = FALSE,
                        download_occurrence = FALSE,
                        download_count = FALSE,
                        download_ranges = FALSE,
                        download_regional = FALSE,
                        download_pis = TRUE,
                        download_ppms = FALSE)
}
#currently on grasshopper sparrow w issues connecting

load_pi(species = "Common Grackle",
        )

c <- list_available_pis(species = "Common Grackle", path = "Z:/Goulden/mbbs-analysis/ebird-habitat-predictor-importance/")
#so, the predictor column is going to need some interpretation