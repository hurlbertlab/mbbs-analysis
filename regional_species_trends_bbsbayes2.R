#---------------------------------------
# Get the regional Piedmont trends for all species
# Uses bbsbayes2
#---------------------------------------

##!!!!!!!!!!! Update with what's on longleaf, done a little differently now from
#how it's done here.

install.packages("bbsBayes2",
                repos = c(bbsbayes = 'https://bbsbayes.r-universe.dev',
                          CRAN = 'https://cloud.r-project.org'))

library(bbsBayes2)
library(mbbs)
library(dplyr)
library(stringr)
#get the functions we need for this script that are stored elsewhere
source("species-trend-estimate-functions.R")

#read in data, using most updated versions of the mbbs. 
mbbs_orange <- mbbs_orange %>% standardize_year(starting_year = 2000)
mbbs_durham <- mbbs_durham %>% standardize_year(starting_year = 2000)
mbbs_chatham <- mbbs_chatham %>% standardize_year(starting_year = 2000)
mbbs <- bind_rows(mbbs_orange, mbbs_chatham, mbbs_durham) %>% #bind all three counties together
  mutate(route_ID = route_num + case_when(
    mbbs_county == "orange" ~ 100L,
    mbbs_county == "durham" ~ 200L,
    mbbs_county == "chatham" ~ 300L)) %>%
  filter(count > 0) %>%
  ungroup() %>%
  #remove the 1999 data, since it's only 1/3 of the routes that were created by then. 
  filter(year > 1999) %>%
  #clean up for ease of use, lots of columns we don't need rn
  dplyr::select(-sub_id, -tax_order, -count_raw, -state, -loc, -locid, -lat, -lon, -protocol, -distance_traveled, -area_covered, -all_obs, -breed_code, -checklist_comments, -source) %>%
  #create a route-standard from 1-34
  group_by(route_ID) %>%
  mutate(route_standard = dplyr::cur_group_id()) %>%
  ungroup()
#help keep the environment clean
rm(mbbs_chatham); rm(mbbs_durham); rm(mbbs_orange) 

#filter to the species with enough sightings to be used in our analysis
mbbs <- filter_to_min_sightings(mbbs, 9, 5)

#get mbbs species list
species_list <- unique(mbbs$common_name)
#as a df for examination
species_list_df <- 
  mbbs %>%
  group_by(common_name) %>%
  summarize()

#fetch BBS data
fetch_bbs_data()
#  **********************
#*    Data Citation   *
#  **********************
#  Ziolkowski, D.J., Lutmerding, M., English, W.B., Aponte, V.I., and Hudson, M-A.R., 2023, North American Breeding Bird Survey Dataset 1966 - 2022: #U.S. Geological Survey data release, https://doi.org/10.5066/P9GS9K64.

#run bbsbayes for the Bird Conservation Region -- Piedmont region for the species in the list,
#trends from 2000->2024.
#Piedmont is bcr #29
data_bbs <- bbsBayes2::stratify(by = "bcr", species = species_list[1])

for(i in 2:length(species_list)) {
  
  s <- bbsBayes2::stratify(by = "bcr", species = species_list[i])
  print(paste0(i,"/",length(species_list)))
  
  data_bbs <- rbind(data_bbs, s)
}
#okay, so that doesn't work, I think the whole modeling workflow with setting 
#the model and everything needs to be run for each species.
#because of the way this data is all formatted with the lists and everything

testing <- bbsBayes2::stratify(by = "bcr", species = "Northern Cardinal")
#filter to just piedmont bcr
# Filter the routes_strata where bcr == 29 and assign it back to the original list
testing$routes_strata <- testing$routes_strata[testing$routes_strata$bcr == 29:30,]
testing$meta_strata <- testing$meta_strata[testing$meta_strata$strata_name == "BCR29" | testing$meta_strata$strata_name == "BCR30",]
testing$birds_strata <- testing$birds_strata[testing$birds_strata$bcr == 29:30,]

p <- prepare_data(testing, min_year = 2000)

md <- prepare_model(p, model = "first_diff")

m <- run_model(md, iter_sampling = 100, iter_warmup = 500, chains = 1)

# Convergence diagnostics for all parameters
converge <- get_convergence(m)
#write csv

# Summary statistics and convergence diagnostics for all parameters
summary_stats <- get_summary(m)
#write csv

i <- generate_indices(model_output = m)
#get trends
t_10 <- generate_trends(m, min_year = 2000, max_year = 2024)
#save csv
t[["trends"]]
