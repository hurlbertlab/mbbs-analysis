#########
#
# Run model estimating effect of UAI on how strongly species respond to development
# These are really closely correlated so, just trying to assess how strongly they agree
# from this model, rather than trying to explain a relationship.
# Broadly, does this metric they caluclated for many species all over the globe,
# agree with this metric we've taken from how populations are changing?
#
##########

library(dplyr)
library(rstan)
library(stringr)
unloadNamespace("rethinking") #just in case, can interfere

#load in dataframes
###################################
#Get the b_dev data from the last run of the first step of the landcover modeling
load_from <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.03.27_loopdevelopment/"
species_list <- read.csv(paste0(load_from, "species_list.csv"))

#uai from Neate-Clegg et al.
uai <- read.csv("data/species-traits/UAI-NateCleg-etall.csv") %>%
  mutate(Species = case_when(Species == "House Wren" ~ "Northern House Wren",
                             TRUE ~ Species)) %>%
  filter(City == "Charlotte_US", 
         Species %in% species_list$common_name)

df <- read.csv(paste0(load_from, "fit_summaries.csv")) %>%
  #filter to just species we're keeping, and we only want our slopes
  filter(common_name %in% species_list$common_name,
         is.na(slope) == FALSE) %>%
  dplyr::select(-NA., -q_rt_standard) %>%
  relocate(slope:common_name, .before = rownames) %>%
  dplyr::select(-rownames) %>%
  #left-join traits
  left_join(uai, by = c("common_name" = "Species")) %>%
  #create groups
  group_by(common_name) %>%
  mutate(sp_standard = cur_group_id()) %>%
  ungroup() %>%
  group_by(SPECIES_GROUP) %>%
  mutate(group_standard = cur_group_id())%>%
  #within each species group we ought to also z-score the mass. 
  mutate(spg_mean_mass = mean(Mass),
         spg_sd_mass = sd(Mass),
         z_mass_spg = ((Mass - spg_mean_mass)/spg_sd_mass),
         z_mass_spg = ifelse(is.na(z_mass_spg), 0, z_mass_spg)) %>%
  ungroup() %>%
  #we don't need the year effects, only the dev effects
  filter(slope == "dev")

save_to <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.04.15_uai_on_bdev/"
#if the output folder doesn't exist, create it
if (!dir.exists(save_to)) {dir.create(save_to)}
#load the stan file
stan_model_file <- "2.landcover_uai_on_bdev.stan"
#compile the stan model first thing.
stan_model <- stan_model(file = stan_model_file)
beepr::beep()
#save the model text
file.copy(stan_model_file, save_to, overwrite = TRUE)
#also save the base df for info about the groupings
write.csv(df, paste0(save_to, "species_variables.csv"), row.names = FALSE)
#create blank dfs for the data to go into
fit_summaries <- as.data.frame(NULL)
posterior_results <-  as.data.frame(NULL)

  standf <- df
  
  datstan <- list(
    N = nrow(standf),
    uai = standf$UAI,
    effect_of_development = standf$mean
  )
  
  fit <- sampling(stan_model,
                  data = datstan,
                  chains = 4,
                  cores = 4, 
                  iter = 4000,
                  warmup = 2000)
  
  fit_temp <- as.data.frame(summary(fit)$summary) %>%
    mutate(rownames = rownames(.)) %>%
    relocate(rownames, .before = mean)
  #bind rows
  fit_summaries <- bind_rows(fit_summaries, fit_temp)
  #save
  write.csv(fit_summaries, paste0(save_to,"fit_summaries.csv"), row.names = FALSE)
  #extract posterior samples and save those also
  temp_posterior <- as.data.frame(fit) %>%
    select(starts_with("b_")) %>%
    mutate(row_id = row_number())
  #bind rows
  posterior_results <- bind_rows(posterior_results, temp_posterior)
  #save, we'll save at the end actually :)
  write.csv(posterior_results, paste0(save_to,"/posterior_samples.csv"), row.names = FALSE)
