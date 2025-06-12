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
library(stringr)

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

#okay, yay, now loop through the species and calculate the means for the available pis we're interested in during the breeding season
  #make blank datasets to add to
  all_pis <- data.frame(NULL)
  all_pis_weekly <- data.frame(NULL)
for(i in 1:nrow(species_list)) {
  
  species_pis <- list_available_pis(species = species_list$ebird_code[i], path = setpath) %>%
    left_join(pis, by = "predictor") %>%
    filter(!is.na(class)) %>%
    mutate(total_mean = as.numeric(999)) %>%
  #filter out problematic ones that aren't working.
    filter(
      !(species_code == "acafly" & predictor == "mcd12q1_lccs1_c11_pland" & response == "occurrence"),
      !(species_code == "barswa" & predictor == "mcd12q1_lccs1_c11_pland" & response == "count"),
      !(species_code == "easmea" & predictor == "mcd12q1_lccs1_c11_pland" & response == "occurrence"),
      !(species_code == "eursta" & predictor == "mcd12q1_lccs1_c11_pland" & response == "count"),
      !(species_code == "grcfly" & predictor == "mcd12q1_lccs1_c11_pland" & response == "occurrence"),
      !(species_code == "hoowar" & predictor == "mcd12q1_lccs1_c11_pland" & response == "occurrence"),
      !(species_code == "horlar" & predictor == "mcd12q1_lccs1_c11_pland" & response == "count"),
      !(species_code == "houspa" & predictor == "mcd12q1_lccs1_c15_pland" & response == "count"),
      !(species_code == "kenwar" & predictor == "mcd12q1_lccs1_c11_pland" & response == "occurrence"),
      !(species_code == "logshr" & predictor == "mcd12q1_lccs1_c11_pland" & response == "count"),
      !(species_code == "louwat" & predictor == "mcd12q1_lccs1_c11_pland" & response == "occurrence"),
      !(species_code == "norbob" & predictor == "mcd12q1_lccs1_c11_pland" & response == "occurrence"),
      !(species_code == "norcar" & predictor == "mcd12q1_lccs1_c11_pland" & response == "occurrence"),
      !(species_code == "normoc" & predictor == "mcd12q1_lccs1_c11_pland" & response == "count"),
      !(species_code == "sumtan" & predictor == "mcd12q1_lccs1_c11_pland" & response == "occurrence"),
      !(species_code == "yelwar" & predictor == "mcd12q1_lccs1_c15_pland" & response == "count"),
      !(species_code == "yetvir" & predictor == "mcd12q1_lccs1_c11_pland" & response == "occurrence")
      )
  
  print(species_list$ebird_code[i]) 
  
  #so, both count and occurrence have dif ranks. I think what we should do here is for each of the predictors, calculate the one mean, and then add that back in to this dataset. THEN, in a separate dataset, store the week-by-week data. Can save both bc not sure which one we'll need
  
  for(a in 1:nrow(species_pis)) {
    
    print(paste0(species_pis$predictor[a],"+", species_pis$response[a]))
    
    one_pi <- load_pi(
      species = species_pis$species_code[a],
      #predictor = species_pis$predictor[a],
      predictor = ntl_mean,
      response = species_pis$response[a],
      path = setpath) 
    if(length(names(one_pi)) > 1) {
      #species is not year-round
      one_pi <- one_pi %>%
        #subset to breeding season May-July
        terra::subset(subset = str_detect(names(.), "0[567]-"))
    } 
    #continue on
    one_pi <- one_pi %>%
      #get mean for each week
      terra::global("mean", na.rm = TRUE) %>%
      tibble::rownames_to_column() %>%
      mutate(total_mean = as.numeric(mean(mean)),
             species_code = species_pis$species_code[a],
             predictor = species_pis$predictor[a],
             response = species_pis$response[a])
    
    #save for outside loop
    all_pis_weekly <- bind_rows(all_pis_weekly, one_pi)
    
    #summarize to just one breeding season mean
    summarized_one_pi <- one_pi %>%
      distinct(total_mean, .keep_all = TRUE) %>%
      dplyr::select(-rowname, -mean)
    #add back to species dataset
    new_row <- species_pis %>% 
      filter(species_code == species_pis$species_code[a],
             predictor == species_pis$predictor[a],
             response == species_pis$response[a]) %>%
      mutate(total_mean = summarized_one_pi$total_mean)
    species_pis <- species_pis %>%
      rows_update(new_row, by = c("species_code", "predictor", "response"))
    
  }
  
  #after looping through all the pis, add to the overall dataset for saving outside the loop
  all_pis <- bind_rows(all_pis, species_pis)
}
  
  #save datasets
  write.csv(all_pis, "data/species-traits/ebird-habitat-association/pis_summarized.csv", row.names = FALSE)
  write.csv(all_pis_weekly, "data/species-traits/ebird-habitat-association/pis_weekly.csv", row.names = FALSE)


  #great! Now what I want to do is finish creating the metrics.
associations <- read.csv("data/species-traits/ebird-habitat-association/pis_summarized.csv") %>%
  #filter out count bc we only care about occurrence
  filter(!response == "count") %>%
  #and we also only want labels that are about %cover, we ignore edge density in this analysis
  #yeah and it looks like already with table(all_pis$label) that's already true even w/o filtering
  filter(str_detect(.$label, "% cover") == TRUE) %>%
  #cut out open and sparse forest, we're not concerned about those
  filter(!str_detect(.$label, "Open Forests|Sparse Forests")) %>%
  mutate(landtype = case_when(
    str_detect(.$label, "Forests") ~ "forest",
    TRUE ~ "grassland")) %>%
  #group by and summarize
  group_by(species_code, landtype, response) %>%
  summarize(lty_association = sum(total_mean),
            n_variables = n()) %>%
  ungroup() %>%
  tidyr::pivot_wider(names_from = landtype, values_from = c("lty_association"), id_cols = c(species_code), names_prefix = "ebirdst_association_")
#yeah so, that the response is "count" and that there are different number of variables that are important to each lty that were/were not included in the count for the species gets dropped here. For posterity, running just part of the code above does generate that. But ultimately, we don't need it for the analysis and I'm not too worried about figuring out how to keep it. These effects are additive - if three forest types are important vs if only one forest type makes the top 30 variables like, they'll come out differently and that's what matters.

assertthat::assert_that(!any(is.na(associations[,2:3]))) #awesome, asserts true, there's no NA grassland or forest (eg. category did not make top 30 variable importance for species if NA)

#let's write this to csv, then join it with our overall species list and save that again.
  write.csv(associations, "data/species-traits/ebird-habitat-association/forest-grass-habitat-associations.csv", row.names = FALSE)
  #species_list <- read.csv("data/species-traits/species_list.csv") %>%
  #  left_join(associations, by = c("ebird_code" = "species_code"))
  #write.csv(species_list, "data/species-traits/species_list.csv", row.names = FALSE)
  
  
#plot
  plot(associations$ebirdst_association_forest, associations$ebirdst_association_grassland, xlim = c(0, .32)) + 
    text(associations$ebirdst_association_forest+.01, associations$ebirdst_association_grassland-.002, labels = associations$species_code, cex = 1)

  
  
  
#########################  
# space for figuring things out, how pis plot, etc.  
  
  
c <- list_available_pis(species = "Common Grackle", path = setpath)
#so, the predictor column is going to need some interpretation
grac <- left_join(pis, c, by = "predictor")
#ok, now what needs interpretation is this "rank" value. It would be nice to use that instead of having to dig into the files themselves. PIs are ranked relative to the full suite of environmental predictors.

ex <- load_pi(species = "Common Grackle",
        predictor = pis$predictor[13], #evergreen needleleaf cover
        response = "occurrence",
        path = setpath)

subset_ex <- terra::subset(ex, subset = str_detect(names(ex),"0[567]-"))
names(subset_ex)

global(subset_ex, "mean", na.rm = TRUE) #awesome, prints the whole list
means <- global(subset_ex, "mean", na.rm = TRUE) %>%
  tibble::rownames_to_column()

#okay, so now, we need to for each species get the list of pis that are included, whether each affects occurance or abundance, then for each of those pis get the means for the related breeding season

#so here you can plot like, the weekly distribution and then, question mark, how important the predictor is that week?
#so we want to use the breeding season only, let's set that as May-July, the weeks that the MBBS surveys are running. 
#so, in this case we'd what...add up the predictor importance of the various forest predictors? average the forest predictors?
#I guess add them...
#but also, important to remember, not every species is going to have every predictor. Probably at least one forest though yeah.
#be good to know the difference btwn open forest/sparse forest and the other forests

#https://stackoverflow.com/questions/76290971/subset-spatraster-layers-return-layers-that-match-subset-names-ignore-those-th
#subset can be used to select variable names, probably that will be part of the solution here
