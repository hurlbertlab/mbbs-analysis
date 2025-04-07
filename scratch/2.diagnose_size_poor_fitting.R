####
#
# My stan model is having issues with fitting size within groups
# the Rhats come out far over 1 and the neffs are really small.
# So, seperating this aspect of the model out from the others,
# I am going to attempt to fix this issue. Likely with setting
# proper priors?
#
#
# At end of script there is also some examples to help with
# interpretation :)
#####


library(dplyr)
library(rstan)
library(stringr)
unloadNamespace("rethinking")

#load in dataframes
###################################
load_from <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.03.27_loopdevelopment/"
species_list <- read.csv(paste0(load_from, "species_list.csv"))

#get species groups
taxonomy <- read.csv("data/species-traits/eBird_taxonomy_v2024.csv") %>%
  select(PRIMARY_COM_NAME, SPECIES_GROUP, SCI_NAME)

#size comes from AVONNET
size <- read.csv("data/species-traits/avonet_filtered.csv") %>%
  dplyr::select(Mass, common_name, SPECIES_GROUP)


df <- read.csv(paste0(load_from, "fit_summaries.csv")) %>%
  #filter to just species we're keeping, and we only want our slopes
  filter(common_name %in% species_list$common_name,
         is.na(slope) == FALSE) %>%
  dplyr::select(-NA., -q_rt_standard) %>%
  relocate(slope:common_name, .before = rownames) %>%
  dplyr::select(-rownames) %>%
  #left-join traits
  left_join(taxonomy, by = c("common_name" = "PRIMARY_COM_NAME")) %>%
  left_join(size, by = c("common_name", "SPECIES_GROUP")) %>%
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
  #we only need the development effects for this model
  filter(slope == "dev")

#make some fake species with very strong size effects that we can use in testing as well.
fake <- df[1:6,] %>%
  mutate(mean = c(-1, -.83, -.40, 0, .3, .67),
         z_mass_spg = c(-1, -.83, -.4, 0, .3, .67),
         group_standard = 25)

df[1:6,] <- fake 
standf <- df
      datstan <- list(
        N = nrow(standf),
        Nspg = length(unique(standf$group_standard)),
        species_group = standf$group_standard,
        #UAI? - nope, measured too close to the same way.
        effect_of_development = standf$mean,
        mass = standf$z_mass_spg
      )


posterior_samples <- read.csv(paste0(load_from, "posterior_samples.csv")) %>%
  filter(common_name %in% species_list$common_name) %>%
  select(-b_year) %>%
  left_join(df, by = c("common_name"))


save_to <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.04.07_scratch_diagnose_size_poor_fitting/"
#if the output folder doesn't exist, create it
if (!dir.exists(save_to)) {dir.create(save_to)}


#load the stan file
stan_model_file <- "scratch/2.diagnose_size_poor_fitting.stan"
#compile the stan model first thing.
stan_model <- stan_model(file = stan_model_file)
beepr::beep()
#save the model text
file.copy(stan_model_file, save_to, overwrite = TRUE)

#create blank dfs for the data to go into
fit_summaries <- as.data.frame(NA)
posterior_results <-  as.data.frame(NA)

for(i in 1:4) {
  standf <- posterior_samples %>%
    filter(row_id == i)
  
  datstan <- list(
    N = nrow(standf),
    Nspg = length(unique(standf$group_standard)),
    species_group = standf$group_standard,
    #UAI? - nope, measured too close to the same way.
    effect_of_development = standf$b_dev,
    mass = standf$z_mass_spg
  )
  
  fit <- sampling(stan_model,
                  data = datstan,
                  chains = 4,
                  cores = 4, 
                  iter = 4000,
                  warmup = 2000,
                  control = list(adapt_delta = 0.99, max_treedepth = 12))
  
  fit_temp <- as.data.frame(summary(fit)$summary) %>%
    mutate(rownames = rownames(.),
           bootstrap_run = i) %>%
    relocate(rownames, .before = mean)
  #bind rows
  fit_summaries <- bind_rows(fit_summaries, fit_temp)
  #save
  write.csv(fit_summaries, paste0(save_to,"fit_summaries.csv"), row.names = FALSE)
  #extract posterior samples and save those also
  temp_posterior <- as.data.frame(fit) %>%
    select(starts_with("b_")) %>%
    mutate(row_id = row_number(),
           bootstrap_run = i) %>%
    #but we don't need to save 4000 from each sample, we're also bootstrapping.
    #save the first 1000
    filter(row_id < 1001)
  #bind rows
  posterior_results <- bind_rows(posterior_results, temp_posterior)
  #save
  write.csv(posterior_results, paste0(save_to,"/posterior_samples.csv"), row.names = FALSE)
  
  timestamp()
  print(paste0("bootstrap ", i," completed"))
  
}


#help with interpretation:
#Example Interpretation in a Model

#Suppose this is part of a species-size effects model (Nspg = number of species):
  
#  b_size[3] = -0.2 (90% CI [-0.5, 0.1]) → The 3rd species’s effect is slightly negative but uncertain.

#b_bar_size = 0.4 (90% CI [0.2, 0.6]) → On average, size has a positive effect.

#b_sig_size = 0.5 (90% CI [0.3, 0.8]) → Species vary moderately around the mean.















