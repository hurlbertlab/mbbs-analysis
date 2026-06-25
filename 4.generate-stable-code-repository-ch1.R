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
  list.files(path = "figures/ch1/", full.names = TRUE)
)



#copy files to new repository location

#create and make some testing file to ensure everything is needed in that repository to run the code

#run the testing code

#outside of R, git commit and git push all changes to the repository

