#file to have all the trend estimate modeling on one file, ideally just adding everything to one nice datasheet as I go

#get the functions we need for this script that are stored elsewhere
source("species-trend-estimate-functions.R")

#libraries
library(beepr) #beeps
library(dplyr) #data manip
library(tidyr) #data manip
library(lme4) #modeling
library(MASS) #modeling
#library(geepack) #GEE modeling
#library(mbbs) #data comes from here
#library(broom) #extracts coefficient values out of models, works with geepack (not using geepack rn)
library(rstan) #stan
library(StanHeaders) #stan helper
#library(rethinking) #alt. way to model with bayes, from Statistical Rethinking course. Can't use for final analysis bc the ulam helper function has some presets that don't fit my needs (eg. 87% standard deviation calculations)

#prevent scientific notation to make a trend table easier to read
options(scipen=999)

#------------------------------------------------------------------------------
# Bayes model, based initially on Link and Sauer 2001
  # https://academic.oup.com/auk/article/128/1/87/5149447
# Modeling help from review of DiCecco Pop Trends code
  # https://github.com/gdicecco/poptrends_envchange/tree/master
#------------------------------------------------------------------------------
#--------------------------------------------------------------------------
# Stan
#-------------------------------------------------------------------------

#read in analysis file 
mbbs <- read.csv("data/analysis.df.csv", header = TRUE) %>%
    dplyr::select(-weather, -vehicles, -notes, -hab_hm, -hab_p, -hab_o, -hab_b, -hab_other, -S, -N, - month, -day, -species_comments)

#read in trait files
##########################################
  gdic_diet <- read.csv("data/species-traits/gdicecco-avian-range-shifts/diet_niche_breadth_mbbs.csv") %>%
    dplyr::select(english_common_name, shannonE_diet)
  
  gdic_climate <- read.csv("data/species-traits/gdicecco-avian-range-shifts/climate_niche_breadth_mbbs.csv") %>%
    dplyr::select(english_common_name, climate_vol_2.1)
  
  gdic_habitat <- read.csv("data/species-traits/gdicecco-avian-range-shifts/habitat_niche_ssi_true_zeroes.csv") %>%
    dplyr::select(english_common_name, ssi) %>%
    #for now, we're kinda doing a mock ssi bc we don't have all the species. Stan does not support NAs in data, so let's change all the ssi to 1. It's not interpretable with only half the data and half mock data anyway
    mutate(habitat_ssi = ssi) %>% #change name for interpret-ability 
    dplyr::select(-ssi)
  
  regional <- read.csv("data/bbs-regional/species-list-usgs-regional-trend.csv") %>%
    dplyr::select(-done, -running, -notes) 
  
    #NOTE: might need to *.001 regional to make it more interpretable? mayybeeee.. just bc output is eg. 0.06 for a 6% change and for regional that same change would be 6.00
  climate_position <- read.csv("data/species-traits/climate_position.csv") %>%
    dplyr::select(common_name, climate_position)
  
  habitat_selection <- read.csv("data/species-traits/ndvi_habitat_selection.csv")
#############################################

#left join trait files
mbbs_traits <- mbbs %>%
  #shannonE_diet
  left_join(gdic_diet, by = c("common_name" = "english_common_name" )) %>%
  #climate_vol_2.1
  left_join(gdic_climate, by = c("common_name" = "english_common_name")) %>%
  #habitat_ssi
  left_join(gdic_habitat, by = c("common_name" = "english_common_name")) %>%
  #climate_position
  left_join(climate_position, by = "common_name") %>%
  #local habitat selection (mean ndvi of stops observed on)
  left_join(habitat_selection, by = "common_name") %>%
  left_join(regional, by = "common_name") %>%
  #Recreate IDs for common name
  group_by(common_name) %>%
  mutate(common_name_standard = cur_group_id()) %>%
  ungroup() 
  #sweep up envrionment
  rm(gdic_diet, gdic_climate, gdic_habitat, climate_position, habitat_selection, regional)

#filtered mbbs, is just acadian flycatcher and woodthrush. for testing purposes

filtered_mbbs <- mbbs_traits %>% filter(common_name == "Wood Thrush" | common_name == "Acadian Flycatcher") %>%
  #Recreate IDs for common name
  group_by(common_name) %>%
  mutate(common_name_standard = cur_group_id()) %>%
  ungroup() %>%
  #Recreate IDs for primary observer
  group_by(primary_observer) %>%
  mutate(observer_ID = cur_group_id()) %>%
  ungroup() 

#change to filtered_mbbs for testing, mbbs_traits for the real thing
mbbs_dataset <- filtered_mbbs
#where to save stan code and fit
save_to <- "model/2025.01.31_Grace_style_model_definition/"
#if the output folder doesn't exist, create it
if (!dir.exists(save_to)) {dir.create(save_to)}

#prepare the data list for Stan
datstan <- list(
  N = nrow(mbbs_dataset), #number of observations
  S = length(unique(mbbs_dataset$common_name_standard)), #n species
  R = length(unique(mbbs_dataset$route_standard)), #n routes
  Y = length(unique(mbbs_dataset$year_standard)), #n years
  species = mbbs_dataset$common_name_standard, #species indices
  route = mbbs_dataset$route_standard, #route indicies
  year = mbbs_dataset$year_standard, #year indices
  observer_quality = mbbs_dataset$observer_quality, #measure of observer quality, NOT CENTERED and maybe should be? Right now there are still negative and positive observer qualities, but these are ''centered'' within routes. Actually I think this is fine non-centered, because the interpretation is that the observer observes 'quality' species of birds more or less than any other observer who's run the route. Only way it could not be fine is bc it's based on each individual route, but the observer is actually judged cross-routes.
  #observer_ID = mbbs_dataset$observer_ID, #observer index
  #O = length(unique(mbbs_dataset$observer_ID)), #n observers
  #trait_diet = mbbs_dataset$shannonE_diet, #! NOT CENTERED YET
  #trait_climate = mbbs_dataset$climate_vol_2.1, #! NOT CENTERED YET
  #trait_habitat = mbbs_dataset$habitat_ssi, #! NOT CENTERED YET
  trait_climateposition = mbbs_dataset$climate_position, #! NOT CENTERED YE
  trait_habitatselection = mbbs_dataset$habitat_selection, #! NOT CENTERED YE
  trait_regional = mbbs_dataset$usgs_trend_estimate,
  C = mbbs_dataset$count #count data
)

#specify the stan model code
stan_model_code <- "
data {
  int<lower=0> N; // number of rows
  int<lower=1> S; // number of species
  int<lower=1> R; // number of routes
  int<lower=1> Y; // number of years
//  int<lower=1> O; // number of observers
  array[N] int<lower=1, upper=S> species; // there is a species for every row and it's an integer between 1 and S
  array[N] int<lower=1, upper=R> route;  // there is a route for every row and it's an integer between 1 and R
  array[N] int<lower=1, upper=Y> year;  // there is an integer for every year and it's an integer between 1 and Y
//  array[N] int<lower=1, upper=O> observer_ID;  // there is an observer_ID for every row and it is in integer between 1 and 'O'(not a zero)
 vector[N] observer_quality;  // there is an observer quality for every row and it is a real number, because it is continuous it can be a vector instead of an array
  array[N] int<lower=0> C;  // there is a count (my y variable!) for every row and it is an unbounded integer that is at least 0.
  vector[N] trait_regional; 
  vector[N] trait_climateposition; 
  vector[N] trait_habitatselection; 
}

parameters {
  vector[S] b; //species trend
  matrix[R, S] a; //species trend along a specific route
  vector[S] a_bar; //the intercept eg. initial count at yr 0 along a route, is allowed to vary by species
  real<lower=0> sigma_a; //standard deviation in a
  real gamma_b; //intercept 
  real kappa_regional;
  real kappa_climateposition;
  real kappa_habitatselection;
  real<lower=0> sig_b;
  
}

transformed parameters {
  real mu_b; //may cause an error here bc S.....will we still only get one kappa?...ACTUALLY can't define it as vector[S] mu_b bc kappas are real and not vectors
  
  mu_b = gamma_b + kappa_regional*trait_regional[S] + kappa_climateposition*trait_climateposition[S] + kappa_habitatselection*trait_habitatselection[S];


}



model {
  a_bar ~ normal(1, 0.5);
  sigma_a ~ exponential(1);
//GRACE METHODS.......................
  b ~ normal(mu_b, sig_b); //mu_b defined in transformed parameters
//^^^^^^^.............................
//  b ~ normal(0, 0.2); //without traits
//  b ~ normal(gamma_b + kappa_regional*trait_regional[S] + kappa_climateposition*trait_climateposition[S] + kappa_habitatselection*trait_habitatselection[S], sig_b);
//  gamma_b ~ normal(0, 0.2);
//.............BY COMMENTING OUT KAPPAS, DEFAULT UNIFORM PRIORS ARE USED.................
//  kappa_regional ~ normal(0, .02); 
//  kappa_climateposition ~ normal(0, .2); 
//  kappa_habitatselection ~ normal(0, .2);
//.............^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^..................
  sig_b ~ exponential(1);
  to_vector(a) ~ normal(a_bar[S], sigma_a); //uses a species specific a_bar
  

// Non-vectorized, so slower than it could be. Let's ignore speed and work on content.
   for (n in 1:N) {
     C[n] ~ poisson_log(a[route[n], species[n]] + b[species[n]] * year[n] + observer_quality[n]);
   }
}
"

#compile the stan model
stan_model <- stan_model(model_code = stan_model_code)
beepr::beep()

#save stan model text if it compiled without errors
cmdstanr::write_stan_file(code = stan_model_code, dir = save_to)

#fit the model to the data
#options(mc.cores = parallel::detectCores())
fit <- sampling(stan_model, 
                data = datstan, 
                chains = 4,
                cores = 4, 
                iter = 2000, 
                warmup = 1000, 
                sample_file = save_to
                )
beepr::beep()

#view results
#Accessing the contents of a stanfit object:
#https://cran.r-project.org/web/packages/rstan/vignettes/stanfit-objects.html
#print(fit)
#perfect, that looks great! b[1] and b[2] are as expected, at 0.05 and -0.06

fit_summary <- summary(fit)
#View(fit_summary$summary) #check R hats and neff

rownames <- row.names(fit_summary$summary)
fit_final <- as.data.frame(fit_summary$summary)
fit_final$rownames <- rownames  
fit_final <- fit_final %>%
  relocate(rownames, .before = mean)

View(fit_final) ##DO NOT HAVE THIS ON LONGLEAF

write.csv(fit_summary$summary, paste0(save_to,"fit_summary.csv"), row.names = TRUE)

#extract posterior samples
post <- extract(fit)















#---------------------------------bsft---------------------------------------
  output$trend_year <- output$trend_year*100
  output$std.error_year <- output$std.error_year*100
  
  minout <- min(output$trend_year)
  maxout <- max(output$trend_year)
  output$trend_std <-  round(50*((output$trend_year)-minout)/(maxout - minout),0) +1
  colorramp = colorRampPalette(c("red", "white", "blue"))
  output <- output %>% arrange(trend_year)
  
  hist(output$trend_year, breaks = 15)
  
  
  mid <- barplot(output$trend_year)
  barplot(height = output$trend_year,
         # names.arg = output$common_name,
         #percent_change - 2 * standard_deviation, percent_change + 2 * standard_deviation),  # Adjust ylim to include error bars,
          #main = "Species Yearly Trends",
         xlab = "Bird Species",
         ylab = "% Yearly Change",
         ylim = c(-7,7),
         col = colorramp(51)[output$trend_std],
         border = "black",
         cex.lab=1.5,
         cex.axis=1.5,
         yaxs = "i")
  #arrows(x0 = mid, y0 = output$trend_year + output$std.error_year,
  #       x1 = mid, y1 = output$trend_year - output$std.error_year,
  #       angle = 90, code =3, length = 0.01)
  
  #read in Grace's diet and migration metrics
  diet <- read.csv("data/gdicecco-avian-range-shifts/diet_niche_breadth.csv", header = TRUE)
  migdist <- read.csv("data/gdicecco-avian-range-shifts/migratory_distance.csv", header = TRUE)
  
  output <- output %>% 
    left_join(diet, by = c("common_name" = "english_common_name")) %>%
    left_join(migdist, by = c("common_name" = "english_common_name"))
  
  fit <- lm(trend_year ~ shannonE_diet, data = output)
  summary(fit)
  fit <- lm(trend_year ~ mig_dist_m, data = output)
  summary(fit)
  #nope, either are significant.
  
  #okay. um. some start to visuals for this presentation..
  average.count <- 
    mbbs_nozero %>%
    filter(year < 2003) %>%
    group_by(common_name) %>%
    summarize(average.count = mean(count))
  
  output <- left_join(output, average.count, by = "common_name")
  
  fit <- lm(trend_year ~ average.count, data = output, family = gaussian)
  summary(fit)
  plot(output$average.count, output$trend_year,
       xlab = "Mean Count 1999-2002",
       ylab = "% Change/Year", 
       pch = 16,
       cex = 1.4,
       cex.axis = 1.5,
       cex.lab = 1.5
  )
  abline(fit, lwd = 2)
  abline(h = 0, lty = 2, col = "red")
  text(20, 7, "P = 0.046, R2 = 0.069", cex = 1.5)
  
  #meantime. let's do migratory distance and diet etc. predictions. left_join traits
  traits <- read.csv("data/NC_species_traits.csv", header = TRUE)
  output <- output %>%
    left_join(traits, by = c("common_name" = "english_common_name"))  
  
  output$Winter_Biome <- as.factor(output$Winter_Biome)
  output$Diet_5Cat <- as.factor(output$Diet_5Cat)
  output$Migrate <- as.factor(output$Migrate)
  output 
  levels(output$Winter_Biome) 
  library(ggpubr)
  library(rstatix)
  ggboxplot(output, x = "Winter_Biome", y = "trend_year")
  ggboxplot(output, x = "Diet_5Cat", y = "trend_year",
            font.label = list(size = 25, color = "black"))
  ggboxplot(output, x = "Migrate", y = "trend_year")
  
  output %>% levene_test(trend_year ~ Winter_Biome) #passes with p > 0.05  #fit an ANOVA
  winter.aov <- output %>% anova_test(trend_year ~ Winter_Biome, detailed = T)
  winter.aov
  #no significance  
  
  library(rstatix)
  
  #fit an ANOVA for diet
  diet.aov <- output %>% anova_test(trend_year ~ Diet_5Cat, detailed = T)
  diet.aov
  pwc <- output %>% tukey_hsd(trend_year ~ Diet_5Cat)
  pwc <- pwc %>% add_xy_position(x = "Diet_5Cat")
  ggboxplot(output, x = "Diet_5Cat", y = "trend_year", add = "jitter") +
    stat_pvalue_manual(pwc, hide.ns = TRUE) +
    labs(
      subtitle = get_test_label(diet.aov, detailed = TRUE),
      caption = get_pwc_label(pwc)
    )
  #no significance  
  
  #git an ANOVA for migration
  migration.aov <- output %>% anova_test(trend_year ~ Migrate, detailed = T)
  migration.aov  #this is how you do it in base R
  summary(aov(trend_year ~ Winter_Biome, data = output))
  pwc <- output %>% tukey_hsd(trend_year ~ Migrate)
  pwc <- pwc %>% add_xy_position(x = "Migrate")
  ggboxplot(output, x = "Migrate", y = "trend_year") +
    stat_pvalue_manual(pwc, hide.ns = TRUE) +
    labs(
      subtitle = get_test_label(migration.aov, detailed = TRUE),
      caption = get_pwc_label(pwc)
    )
  
  #hey, Ivara, this is the wrong way to analyze this data. Rather than fitting a linear line to a category vs trend, this is a t-test style analysis that requires box plots of differences. let's do that and THEN make calls, ok? uai is a continous variable and can be fit with a line, these categorical variables are not and you should treat them like the cat data.
  fit <- lm(trend_year ~ Breeding_Biome, data = output)
  summary(fit)
  #____________________________________________________________________________
  
  
  output_base <- run_gee(formula = f_base,
                    mbbs, species_list) %>%
    #remove intercept information
    filter(!value %in% "(Intercept)") %>%
    #add trend into (exponentiate the esimate)
    mutate(trend = exp(estimate)-1) %>%
    #pivot_wider
    pivot_wider(names_from = value, values_from = c(estimate, std.error, statistic, p.value, trend)) %>%
    dplyr::select(-name)%>%
    left_join(uai, by = c("common_name" = "Species"))
  
  #estimate and trend are 1:1 matched. !!! Something is probably wrong with trend, I don't think it should all be in the .30s when the estimates are -06:0.08
  plot(output$trend_year, output$estimate_year)
  
  fit <- lm(trend_year ~ UAI, data = output)
  plot(y=output$trend_year, x=output$UAI)
  plot(trend_year ~ UAI, data = output, 
       pch = 16,
       col = colorramp(51)[output$trend_std])
  abline(fit)
  
  plot(count ~ year, data = pw)
  fit <- lm(count ~ year, data = pw)
  abline(fit)
  
  plot(x=output_cor$trend_year,y= output_cor$estimate_percent_developed)
    
#------------------------------------------ 