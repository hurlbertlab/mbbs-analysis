########################################
#
# Script where the stan lists for different models are stored
# stored as functions.
#
######################################

#' List for a model that predicts the influence of 
#' three species traits:
#'  climate_position
#'  habitat_niche
#'  regional_trend
#' on species trends (beta or 'b')
#' mbbs trends hierarchically modeled for each species
#'    eg. beta is made up of an alpha predicted for every route for that species.
#' @returns datstan
datstan_traits_influence_mbbs <- function(mbbs_dataset) {
  
  datstan <- list(
    N = nrow(mbbs_dataset), #number of observations
    Nsp = length(unique(mbbs_dataset$common_name_standard)), #n species
    Nsprt = length(unique(mbbs_dataset$sprt_standard)), #n species route combos
    Nyr = length(unique(mbbs_dataset$year_standard)), #n years
    sp = mbbs_dataset$common_name_standard, #species indices
    sprt = mbbs_dataset$sprt_standard, #species route indices
    sp_sprt = sp_sprt_base$common_name_standard, #species for each species route
    year = mbbs_dataset$year_standard, #year indices
    observer_quality = mbbs_dataset$observer_quality, #measure of observer quality, NOT CENTERED and maybe should be? Right now there are still negative and positive observer qualities, but these are ''centered'' within routes. Actually I think this is fine non-centered, because the interpretation is that the observer observes 'quality' species of birds more or less than any other observer who's run the route. Only way it could not be fine is bc it's based on each individual route, but the observer is actually judged cross-routes.
    Nobs = length(unique(mbbs_dataset$observer_ID)), #n observers
    obs = mbbs_dataset$observer_ID, #observer index
    #trait_diet = mbbs_dataset$shannonE_diet, #! NOT CENTERED YET
    #trait_climate = mbbs_dataset$climate_vol_2.1, #! NOT CENTERED YET
    #trait_habitat = mbbs_dataset$habitat_ssi, #! NOT CENTERED YET
    t_climate_pos = traits$climate_position, #! NOT CENTERED YET
    t_habitat_selection = traits$habitat_selection, #! NOT CENTERED YET
    t_regional = traits$usgs_trend_estimate,
    sp_t = traits$common_name_standard, #species for each trait
    C = mbbs_dataset$count #count data
  )
  
}

#' List for a model that predicts the influence of 
#' two species traits:
#'  climate_position
#'  habitat_niche
#' on the difference between a species' regional and local "beta"/"b" trend
#' @returns datstan
datstan_traits_influence_dif_btwn_mbbs_regional <- function(mbbs_dataset, traits, sp_sprt_base, obs_q_base, bootstrap_regional) {
  
  datstan <- list(
    N = nrow(mbbs_dataset), #number of observations
    Nsp = length(unique(mbbs_dataset$common_name_standard)), #n species
    Nrt = length(unique(mbbs_dataset$route_standard)), #n routes
    Nsprt = length(unique(mbbs_dataset$sprt_standard)), #n species route combos
    Nyr = length(unique(mbbs_dataset$year_standard)), #n years
    sp = mbbs_dataset$common_name_standard, #species indices for each observation
    rt = mbbs_dataset$route_standard, #route indices for each observation
    #  sp_a = unique(mbbs_dataset$common_name_standard), #the unique species ids
    #sprt = mbbs_dataset$sprt_standard, #species route indices
    sp_sprt = sp_sprt_base$common_name_standard, #species for each species route
    year = mbbs_dataset$year_standard, #year indices
    observer_quality = obs_q_base$observer_quality, #measure of observer quality, NOT CENTERED and maybe should be? Right now there are still negative and positive observer qualities, but these are ''centered'' within routes. Actually I think this is fine non-centered, because the interpretation is that the observer observes 'quality' species of birds more or less than any other observer who's run the route. Only way it could not be fine is bc it's based on each individual route, but the observer is actually judged cross-routes.
    Nobs = length(unique(mbbs_dataset$observer_ID)), #n observers
    obs = mbbs_dataset$observer_ID, #observer index
    #trait_diet = mbbs_dataset$shannonE_diet, #! NOT CENTERED YET
    #trait_climate = mbbs_dataset$climate_vol_2.1, #! NOT CENTERED YET
    #trait_habitat = mbbs_dataset$habitat_ssi, #! NOT CENTERED YET
    t_climate_pos = traits$climate_position, #! NOT CENTERED YET
    t_habitat_selection = traits$habitat_selection, #! NOT CENTERED YET
    t_regional = traits$usgs_trend_estimate,
    sp_t = traits$common_name_standard, #species for each trait
    C = mbbs_dataset$count, #count data
    #unique for calculating the difference between regional and local trends
    Nboot = nrow(bootstrap_regional), #number of bootstrap obs overall
    Nboot_sample = num_bootstrap, #number of regional and b samples per species
    boot_regional = bootstrap_regional$bootstrapped_regional, #regional trend for each bootstrapped observation
    boot_sp = bootstrap_regional$common_name_standard #species index for each bootstrapped observation
  )
  
}
