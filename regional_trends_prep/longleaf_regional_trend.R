#-------------------------------------
# Get the regional Piedmont trends for all species
# Built to run on longleaf
#-------------------------------------

install.packages("bbsBayes2",
                 repos = c(bbsbayes = 'https://bbsbayes.r-universe.dev',
                           CRAN = 'https://cloud.r-project.org'))

library(bbsBayes2)
library(dplyr)
library(stringr)

species_list <- read.csv()