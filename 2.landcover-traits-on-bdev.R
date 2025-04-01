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
unloadNamespace("rethinking")

#load in dataframes
###################################
  #get species groups
  taxonomy <- read.csv("data/species-traits/eBird_taxonomy_v2024.csv") %>%
    select(PRIMARY_COM_NAME, SPECIES_GROUP, SCI_NAME)
  
  #size comes from AVONNET
  size <- read.csv("data/species-traits/avonet_filtered.csv") %>%
    dplyr::select(Mass, common_name, SPECIES_GROUP)
  
  #climate position
  climate_position <- read.csv("data/species-traits/climate_position.csv") %>%
    select(common_name, z_tempwq, z_precipwq, climate_position)
  #ndvi selection
  habitat <- read.csv("data/species-traits/ndvi_habitat_selection.csv")


load_from <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.03.27_loopdevelopment/"
  species_list <- read.csv(paste0(load_from, "species_list.csv"))
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
    #create groups
    group_by(common_name) %>%
    mutate(sp_standard = cur_group_id()) %>%
    ungroup() %>%
    group_by(SPECIES_GROUP) %>%
    mutate(group_standard = cur_group_id())%>%
    ungroup()
  
  #separate out year effects
  year_effect <- df %>%
    filter(slope == "year")
  
  #so now we don't need year in there..
  df <- df %>%
    filter(slope == "dev")
  
  posterior_samples <- read.csv(paste0(load_from, "posterior_samples.csv")) %>%
    filter(common_name %in% species_list$common_name) %>%
    select(-b_year) %>%
    left_join(df, by = c("common_name"))
  #k yeah, that looks better. Accounting for the variance now when I use these.
  #BUT STILLLLLLLL you need to bring them together and account for that they're samples from the same thing. Otherwise the model is going to come out significant bc it can predict things really well... but it can do that bc there's a ton of samples and things are closely correlated w parts of those samples.
  
####################################################
  
  #simple glm
  m <- glm(mean ~ habitat_selection + climate_position + Mass + se_mean, data = df, family = gaussian)
  summary(m)
 #Set up for stan
  #or..... load in the posterior samples. Those are my bootstrapped amounts. Connect everything by common_name?...
  #hmmmmmmmmmmmmmmmmmmmmmmmmmmmm, it would again be like.....
  #bootstrap the means
  #and fit a different intercept for each species? To be like, I know these species are different..........
  #but ah, I don't want a different intercept for each species! or, do I? That way I get the 'mean' for each.........
  #but REALLY it's that the se/sd is error in there somewhere......
  #like I want to model the whole distribution with the rest....
datstan <- list(
  N = nrow(df), #number of observations
  Nsp = length(unique(df$sp_standard)), #number of species (same as n observations)
  dev_mean = df$mean, #effect of development for each observation
  dev_sd = df$sd, #standard deviation of development for each observations
  climate_position = df$climate_position,
  
)

  
