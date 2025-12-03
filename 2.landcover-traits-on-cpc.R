##########################
#
# Testing the effects of species traits
# on how strongly a species' count changes
# in response to change in perc. developed
# AND percent forest. Test both here.
#
#
## traits:
# temperature niche position (where NC is in the species temperature range)
# ebird-derived forest selectivity
# ebird-derived grasslands selectivity
# Neate-Clegg et al. Urban Association Index
#
#########################

library(dplyr)
library(rstan)
library(stringr)
unloadNamespace("rethinking") #just in case, can interfere


#load in dataframes
###################################
#Get the b_dev data from the last run of the first step of the landcover modeling
load_from <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.09.18_cpc_rm0sprt_alllandcovers/"
species_list <- read.csv(paste0(load_from, "species_list.csv")) 

#climate 
climate <- read.csv("data/species-traits/climate_position.csv") %>%
  dplyr::select(common_name, z_tempwq)

#UAI
uai <- read.csv("data/species-traits/UAI-NateCleg-etall.csv") %>%
  dplyr::filter(City == "Charlotte_US") %>%
  #fix House Wren -> Northern House Wren which has had a taxonomy change since this data was published
  dplyr::mutate(Species = 
                  case_when(
                    Species == "House Wren" ~ "Northern House Wren",
                    TRUE ~ Species
                  )) %>%
  dplyr::select(-X, -SE)

#ebird-habitat data
#habitat_select <- read.csv("data/species-traits/ebird-habitat-association/forest-grass-habitat-associations.csv")
habitat_select <- read.csv("data/species-traits/species_list.csv") %>%
  dplyr::select(common_name, ebird_code, ebirdst_association_forest, ebirdst_association_grassland)

#add all to species_list
traits <- species_list %>%
  left_join(climate, by = "common_name") %>%
  left_join(habitat_select, by = "common_name") %>%
  left_join(uai, by = c("common_name" = "Species")) %>%
  #scale all the variables so we can compare across them
  mutate(
    scale_z_tempwq = ((z_tempwq - mean(z_tempwq))/sd(z_tempwq)),
    scale_UAI = ((UAI - mean(UAI))/sd(UAI)),
    scale_eaforest = ((ebirdst_association_forest - mean(ebirdst_association_forest))/sd(ebirdst_association_forest)),
    scale_eagrassland = ((ebirdst_association_grassland - mean(ebirdst_association_grassland))/sd(ebirdst_association_grassland))
  ) 
#save copy
write.csv(traits, "data/species-traits/2.landcover_cpc_analysis_full_traits.csv", row.names = FALSE)

    #function for handling the bdevs
    format_fit_summaries <- function(bdf, 
                                     traitsdf = traits, 
                                     select_rownames = "b_landcover_change") {
      bdf |>
        dplyr::filter((str_detect(rownames, select_rownames))) |>
        dplyr::select(rownames, mean, sd, conf_2.5, conf_97.5, slope, common_name) |>
        left_join(traitsdf, by = "common_name")
    }
    
    format_fit_posteriors <- function(bdf,
                                      traitsdf = traits) {
      bdf |>
        left_join(traitsdf, by = "common_name")
    }

#load in the slopes from our change per change model
bdev <- read.csv(paste0(load_from, "dev+barren_fit_summaries.csv")) %>%
  format_fit_summaries()
bforest_pos <- read.csv(paste0(load_from, "forest_positive_fit_summaries.csv")) %>%
  format_fit_summaries()
bforest_neg <- read.csv(paste0(load_from, "forest_negative_fit_summaries.csv")) %>%
  format_fit_summaries()
bgrassland_pos <- read.csv(paste0(load_from, "grassland_positive_fit_summaries.csv")) %>%
  format_fit_summaries()
bgrassland_neg <- read.csv(paste0(load_from, "grassland_negative_fit_summaries.csv")) %>%
  format_fit_summaries()

#okay, rather than posterior samples and pulling from there, instead we pass just the mean and standard variation. much neater.

fit_sums <- bind_rows(bdev, bforest_pos, bforest_neg, bgrassland_pos, bgrassland_neg) %>%
  mutate(landcover = str_extract(slope, "(?<=landcover, ).*"))

rm(bdev, bforest_pos, bforest_neg, bgrassland_neg, bgrassland_pos)


#   #make a test dataset where the connection is like 1:1 to ensure our code is running as we expect. clear loss in response to increasing development and high uai (negative cor), clear loss in response to forest decreasing (positive cor), clear gain in response to forest increasing (positive cor)
# #mixing this up w prev model. Here the test is just making a really clear cor btwn bdev and scale_UAI. If I know there's an effect there does that come out in my model.
#   btest <- bdev %>%
#     dplyr::arrange(mean) %>%
#     mutate(scale_UAI = sort(rnorm(nrow(bdev), 0, 1), decreasing = FALSE))
  
  #main potential problem I see is that:
  cor(traits$scale_eagrassland, traits$scale_eaforest) #is just about .7 - where the inclusion of both in the model may cause problems where it looks like one is not significant
  cor(traits$scale_eagrassland, traits$scale_UAI) #not cor, 0.06
  cor(traits$scale_z_tempwq, traits$scale_UAI) #fine cor, 0.3
  cor(traits$scale_z_tempwq, traits$scale_eaforest) #fine cor, -0.02

#where to save
save_to <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.12.03_tcpc_newmethod2/"
#if the output folder doesn't exist, create it
if (!dir.exists(save_to)) {dir.create(save_to)}
#load the stan file, compile stan file, save stan file
stan_model_file <- "2.landcover_traits_on_cpc.stan"
stan_model <- stan_model(file = stan_model_file)
beepr::beep()
file.copy(stan_model_file, save_to, overwrite = TRUE)

landtypes <- unique(fit_sums$landcover)
############for testing!
landtypes <- landtypes[1]

for(b in 1:length(landtypes)) {

#create blank dfs for the data to go into
fit_summaries <- as.data.frame(NULL)
posterior_results <-  as.data.frame(NULL)
#set our dataframe
standf <- fit_sums %>%
  filter(landcover == landtypes[b])
#print the landtype we're currently working with
print(landtypes[b])
  
datstan <- list(
  N = nrow(standf),
  z_score_tempwq = standf$scale_z_tempwq,
  forest_association = standf$scale_eaforest,
  grassland_association = standf$scale_eagrassland,
  uai = standf$scale_UAI,
  cpc_mean = standf$mean,
  cpc_sd = standf$sd
)

fit <- sampling(stan_model,
                data = datstan,
                chains = 4,
                cores = 4, 
                iter = 2000,
                warmup = 1000)

fit_temp <- as.data.frame(summary(fit)$summary) %>%
  mutate(rownames = rownames(.)) %>%
  relocate(rownames, .before = mean) %>%
  #rename numeric rows
  rename_with(~ paste0("conf_", .), .cols = matches("^[0-9]")) %>%
  #remove %s in column names
  rename_with(~ str_remove(., "%"), .cols = everything())
#bind rows
fit_summaries <- bind_rows(fit_summaries, fit_temp)

temp_posterior <- as.data.frame(fit) %>%
  select(starts_with("b_")) %>%
  mutate(row_id = row_number()) 
#bind rows
posterior_results <- bind_rows(posterior_results, temp_posterior)

timestamp()
print(paste0("landcover ", b," completed"))

#write only at end bc really long
write.csv(posterior_results, paste0(save_to, landtypes[b], "posterior_samples.csv"), row.names = FALSE)
write.csv(fit_summaries, paste0(save_to, landtypes[b], "fit_summaries.csv"), row.names = FALSE)
print(paste0("landcover ", b," results saved"))
}
