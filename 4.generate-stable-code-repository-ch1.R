#########################
#
# Make the stable repository of everything used in ch1
# ncmbbs-trends-by-traits
#
########################

library(stringr)

#list of files that should be copied
files.list <- c(
  "0a.update-mbbs.r",
  "0b.generate-dataset.r",
  "1.observers-calc-quality.r",
  "1.traits-calc-climate-position.r",
  "1.traits-calc-hyperlocal-habitat-selection.r",
  "1.traits-get-diet-perc.r",
  "1.traits-get-ebird-habitat-association.r",
  "2.analysis-functions.r",
  "2.ch1_active_development_model.stan",
  "2.ch1_m1_dietonly.stan",
  "2.ch1_m2_interaction.stan",
  "2.ch1_observer_only_model.stan",
  "2.ch1-model-trends-traits.r",
  "2.ch1-run-bayes-model-observer-only.r",
  "2.ch1-z-pick-sensitivity-analysis-species.r",
  "3.ch1-estimate-VIF.r",
  "3.ch1-phylogenetic-signal-test.r",
  "3.ch1-prior-predictive-checks.r",
  "3.ch1-supplemental-tables.r",
  "3.plot-ch1.r",
  "3.plot-functions.r",
  "4.ch1-format-latex-formulas.rmd",
  "4.generate-stable-code-repository-ch1.r"
)

folders <- c(
  "data/", #list.files(path = "data/", full.names = TRUE, pattern = ".csv")
  "data/bbs-regional/",
  "data/ch1-supplemental-tables",
  "data/mbbs",
  "data/species-traits",
  "data/species-traits/ebird-habitat-association/",
  "data/species-traits/gdicecco-avian-range-shifts/",
  "figures/",
  "figures/ch1/",
  "model/",
  "model/2026.06.27_ch1_m1_2024rt/",
  "model/2026.06.26_ch1_rmNOBO_2024rt/",
  "model/2026.03.12_ch1_withoutregional_final/",
  "model/2026.04.09_ch1_rmNOBO_woRegional_final/"
)

#copy files to new repository location
#okay, so this does need to be a little more complicated to recreate folders, b/c file.copy only copies files and doesn't preserve folders. But I want to preserve folders.
file.copy(files.list,
          to = "c:/git/ncmbbs-trends-by-traits/",
          overwrite = TRUE,
          copy.date = TRUE)

for(i in 1:length(folders)) {
  
  new_folder <- paste0("C:/git/ncmbbs-trends-by-traits/", folders[i])
  
  #make the folders if they don't already exist
  if (!dir.exists(new_folder)) {dir.create(new_folder)}
  
  #copy files from one directory to the new one. For the data/ folder only copy .csv. Don't copy other figures from figures/
  if(folders[i] == "data/") {
    file.copy(list.files(path = folders[i], full.names = TRUE, pattern = ".csv"),
              to = paste0("C:/git/ncmbbs-trends-by-traits/", folders[i]),
              overwrite = TRUE,
              copy.date = TRUE)
    
  } else if(folders[i] == "figures/") {
    
    #do nothing, all we needed to do was make the file.
    
  } else {
    file.copy(list.files(path = folders[i], full.names = TRUE),
              to = paste0("C:/git/ncmbbs-trends-by-traits/", folders[i]),
              overwrite = TRUE,
              copy.date = TRUE)
  }
  
}

#create and make some testing file to ensure everything is needed in that repository to run the code

#run the testing code

#outside of R, git commit and git push all changes to the repository

