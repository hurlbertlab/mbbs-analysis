########################
#
# Take the results of our modeling trends
# at the quarter-route level
# and run an analysis on the effects of species traits
# on how strongly the species responses to development
#
# traits:
# year trend (don't expect to be significant)
# hyperlocal habitat selection eg. ndvi
# climate position (edge of range or center?)
# size [within species group]
#
########################

library(dplyr)
library(rstan)
library(stringr)
unloadNamespace("rethinking") #just in case, can interfere

#load in dataframes
###################################
#Get the b_dev data from the last run of the first step of the landcover modeling
load_from <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.03.27_loopdevelopment/"
species_list <- read.csv(paste0(load_from, "species_list.csv"))

  #get species groups
  taxonomy <- read.csv("data/species-traits/eBird_taxonomy_v2024.csv") %>%
    select(PRIMARY_COM_NAME, SPECIES_GROUP, SCI_NAME)
  
  #size comes from AVONNET
  size <- read.csv("data/species-traits/avonet_filtered.csv") %>%
    dplyr::select(Mass, common_name, SPECIES_GROUP) %>%
    mutate(log_mass = log(Mass))
  
  #climate position
  climate_position <- read.csv("data/species-traits/climate_position.csv") %>%
    select(common_name, z_tempwq, z_precipwq, climate_position)
  #ndvi selection
  habitat <- read.csv("data/species-traits/ndvi_habitat_selection.csv")
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
    left_join(taxonomy, by = c("common_name" = "PRIMARY_COM_NAME")) %>%
    left_join(size, by = c("common_name", "SPECIES_GROUP")) %>%
    left_join(climate_position, by = "common_name") %>%
    left_join(habitat, by = "common_name") %>%
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
  
  posterior_samples <- read.csv(paste0(load_from, "posterior_samples.csv")) %>%
    filter(common_name %in% species_list$common_name) %>%
    select(-b_year) %>%
    left_join(df, by = c("common_name"))
  #k yeah, that looks better. Accounting for the variance now when I use these.
  #BUT STILLLLLLLL you need to bring them together and account for that they're samples from the same thing. Otherwise the model is going to come out significant bc it can predict things really well... but it can do that bc there's a ton of samples and things are closely correlated w parts of those samples.
  
  #each row_id in posterior samples represents a bootstrapped value, so, to get a dataset to run each time I can select i = 
  
####################################################
  
 #Set up for stan
  #we will be BAGGING the data, where we bootstrap the data, fit the model, and then average over our predictions at the end.
  #set where to save things
  save_to <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.04.11_traits_on_bdev_add_logsize/"
  #if the output folder doesn't exist, create it
  if (!dir.exists(save_to)) {dir.create(save_to)}
  #load the stan file
  stan_model_file <- "2.landcover_traits_on_bdev.stan"
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
#for(i in 1:max(posterior_samples$row_id)) {
  for(i in 52:400) {
    standf <- posterior_samples %>%
      filter(row_id == i)
    
    datstan <- list(
      N = nrow(standf),
      climate_position = standf$climate_position,
      habitat = standf$habitat_selection,
      mass_group = standf$z_mass_spg,
      Nspg = length(unique(standf$group_standard)),
      species_group = standf$group_standard,
      log_mass = standf$log_mass,
      #UAI? - nope, measured too close to the same way.
      effect_of_development = standf$b_dev
    )
    
    fit <- sampling(stan_model,
                    data = datstan,
                    chains = 4,
                    cores = 4, 
                    iter = 4000,
                    warmup = 2000)
    
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
    #save, we'll save at the end actually :)
    #write.csv(posterior_results, paste0(save_to,"/posterior_samples.csv"), row.names = FALSE)
    
    timestamp()
    print(paste0("bootstrap ", i," completed"))
    
  }
  #save at end bc it takes a bit, huge dataset
  write.csv(posterior_results, paste0(save_to,"/posterior_samples1-400.csv"), row.names = FALSE)
  beepr::beep()
  write.csv(fit_summaries, paste0(save_to,"fit_summaries1-400.csv"), row.names = FALSE)
  
  #i got to 2460 overnight - so this is a quite slow way to work with the data. 
  fit_summaries <- read.csv(paste0(save_to,"fit_summaries.csv"))
  View(fit_summaries)
  
  temp <- fit_summaries %>% filter(rownames %in% c("b_climate", "b_habitat"))
   View(temp)
  b_clim <- temp %>% filter(rownames == "b_climate")
  b_hab <- temp %>% filter(rownames == "b_habitat")
  hist(b_clim$`2.5%`) #almost always negative
  hist(b_clim$`97.5%`) #always positive
  hist(b_clim$mean)
  
  hist(b_hab$`2.5%`) #always negative
  hist(b_hab$`97.5%`) #always positive..
  hist(b_hab$mean)
  
  #in terms of summarizing, these should still be like the confidence intervals! With both the mean, the 87% and the 95%. Even though they graze 0 in the 2.5...it's the whole distribution that matters. Are they all important or only some of them?
  
  #scatterplots of data
  plot(df$mean, df$habitat_selection) #but it looks like habitat does have a relationship?
  plot(df$mean, df$climate_position) #so climate position is quite scattered
  df <- df %>%
    filter(!common_name == "Wild Turkey")
  plot(df$mean, df$Mass)
  
  ggplot(df, aes(x=mean, y=Mass, col = SPECIES_GROUP)) + 
    geom_point()
  
  #let's also run a quick model with uai
  
  datstanuai <- list(
    N = nrow(df),
    uai = df$UAI,
    #year?
    #size?
    #UAI? - nope, measured too close to the same way.
    effect_of_development = df$mean
  )
  
  stan_model_file <- "2.landcover_uai_on_bdev.stan"
  #compile the stan model first thing.
  stan_model <- stan_model(file = stan_model_file)
  
  fit <- sampling(stan_model,
                  data = datstanuai,
                  chains = 4,
                  cores = 4, 
                  iter = 2000,
                  warmup = 1000)
  
  fit_temp <- as.data.frame(summary(fit)$summary) %>%
    mutate(rownames = rownames(.)) %>%
    relocate(rownames, .before = mean)
  
  post_uai <- as.data.frame(fit) %>%
    select(b_uai)
  plot(df$mean, df$UAI)
  library(bayesplot)
  mcmc_areas(post_uai,
                 prob_outer = 0.95)
  
  
  
  
  
  
  
  
  
  
  
# int<lower=0> N; //number of observations
# vector[N] climate_position; //x variable, eg. climate position
# vector[N] dev_mean; //y variable, known mean for each species
# vector[N] dev_sd; //for use in re-sampling y, known SD for each species
# int<lower=0, upper=1> resample;

datstan <- list(
  N = nrow(standf),
  climate_position = standf$climate_position,
  dev_mean = standf$mean, #effect of development for each observation
  dev_sd = standf$sd, #standard deviation of development for each observations, 
  resample = 1
)

stan_model_file <- "2.landcover_traits_on_bdev.stan"
#compile the stan model
stan_model <- stan_model(file = stan_model_file)

fit <- sampling(stan_model,
                data = datstan,
                chains = 4,
                cores = 4, 
                iter = 2000,
                warmup = 1000)

fit_temp <- as.data.frame(summary(fit)$summary) %>%
  mutate(rownames = rownames(.)) %>%
  relocate(rownames, .before = mean)
  
datstan <- list(
  N = nrow(standf), #number of observations
  Nsp = length(unique(standf$sp_standard)), #number of species (same as n observations)
  dev_mean = standf$mean, #effect of development for each observation
  dev_sd = standf$sd, #standard deviation of development for each observations
  climate_position = standf$climate_position,
  habitat = standf$habitat_selection,
  mass = standf$Mass
)

  
