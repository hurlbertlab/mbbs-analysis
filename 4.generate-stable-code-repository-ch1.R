#########################
#
# Make the stable repository of everything used in ch1
# ncmbbs-trends-by-traits
#
########################

#list of files that should be copied
files.list <- c(
  list.files(path = "data/bbs-regional/", full.names = TRUE),
  list.files(path = "data/ch1-supplemental-tables/", full.names = TRUE),
  list.files(path = "data/mbbs/", full.names = TRUE),
  list.files(path = "data/species-traits/", recursive = TRUE, full.names = TRUE),
  list.files(path = "data/", full.names = TRUE, pattern = ".csv"),
  list.files(path = "figures/ch1/", full.names = TRUE),
  list.files(path = "model/", recursive = TRUE, full.names = TRUE),
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



#copy files to new repository location
#okay, so this does need to be a little more complicated to recreate folders, b/c file.copy only copies files and doesn't preserve folders. But I want to preserve folders.
file.copy(files.list,
          recursive = TRUE,
          to = "c:/git/ncmbbs-trends-by-traits/",
          overwrite = TRUE,
          copy.date = TRUE)

#this will just have to be like, make.folder() then list.files everything in that folder then file.copy to that same folder. Can definetely make a function to do that that takes just the folder name. Then for the things NOT in a folder I can just, as above, add to the files.list directly.
#can get a list using a stringr r for after a / [A-Z]+.csv

#create and make some testing file to ensure everything is needed in that repository to run the code

#run the testing code

#outside of R, git commit and git push all changes to the repository

