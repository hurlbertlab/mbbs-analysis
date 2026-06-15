##################
#Hi Ivara,

#Nice chatting today. One last thing that came to mind that I forgot to mention. If you’re looking at trends as a function of species’ traits you’ll want to make sure there isn’t any phylogenetic ’signal’ in the residuals of that part of the model. This is because these trends are not independent in the strict sense, because some species are more closely related than others (and thus may share similar characteristics). So phylo signal I mean, for the below ’level’ of the model

#b_k ~ normal(gamma + kappa1 * trait1_j + …, sig)

#first calculate eps_k = b_k - gamma + kappa1 * trait_k + ...

#eps_k is the deviation from the ‘line’ explaining trend based on trait. Then you assess whether this has some relationship to the pairwise phylogenetic distance between all species. Essentially you get the phylogenetic tree, calculate the distance between all the species (think the length of the line connect two species on a phylogeny) and then use something like Pagel’s lambda to assess the degree that phylogeny is related to the residuals (given by eps). Happy to show you an example of that at some point. If you do have phylo signal, you’ll need to incorporate a phylogenetic component into your model (not too difficult). If you don’t, you’re good to go! Again, something likely to come up in review.
###################

library(dplyr)
library(rtrees)
library(ape)
library(phytools)
library(stringr)
library(RColorBrewer)

#get the species residuals from the final model.
#mean for each species from the mean line
fit_summary <- read.csv("model/2026.04.09_ch1_rmNOBO_final/fit_summary.csv") |>
  mutate(select_for = str_detect(rownames, "^b|kappa|gamma_b")) |>
  filter(select_for == TRUE) |>
  mutate(common_name_standard = as.integer(str_extract(rownames, "[0-9]([0-9])?"))) |>
  left_join(read.csv("model/2026.04.09_ch1_rmNOBO_final/beta_to_common_name.csv"), by = c( "common_name_standard")) |>
  #add species traits
  left_join(read.csv("model/2026.04.09_ch1_rmNOBO_final/species_traits.csv"), by = c("common_name_standard"))

kappa_regional = fit_summary$mean[fit_summary$rownames == "kappa_regional"]
kappa_temp_pos = fit_summary$mean[fit_summary$rownames == "kappa_temp_pos"]
kappa_habitat_selection = fit_summary$mean[fit_summary$rownames == "kappa_habitat_selection"]
kappa_diet = fit_summary$mean[fit_summary$rownames == "kappa_diet"]
gamma_b = fit_summary$mean[fit_summary$rownames == "gamma_b"]

eps_k <- fit_summary |>
  #calc eps_k, the point for each species minus the residual line
  mutate(eps_k = mean - 
           (gamma_b + 
              kappa_diet * scale_insect_perc +
              kappa_habitat_selection * scale_habitat_ssi +
              kappa_temp_pos * scale_ztempwq +
              kappa_regional * scale_usgs_trend)) |>
  dplyr::select(common_name, eps_k) |>
  filter(!is.na(common_name))

cor(eps_k$eps_k, fit_summary$scale_ztempwq[!is.na(fit_summary$scale_ztempwq)])
cor(eps_k$eps_k, fit_summary$scale_habitat_ssi[!is.na(fit_summary$scale_ztempwq)])
cor(eps_k$eps_k, fit_summary$scale_insect_perc[!is.na(fit_summary$scale_ztempwq)])
cor(eps_k$eps_k, fit_summary$scale_usgs_trend[!is.na(fit_summary$scale_ztempwq)])
#kay cool, all those correlations are very low.

#generate a phylogenetic tree for these species from the final model
species_list <- read.csv("model/2026.04.09_ch1_rmNOBO_final/beta_to_common_name.csv") |>
  #add scientific name, I assume we'll need that
  left_join((
    read.csv("data/mbbs/mbbs_route_counts.csv") |>
      distinct(common_name, sci_name)),
    by = c("common_name")) |>
  dplyr::select(-common_name_standard) |>
  mutate(genus = str_remove(str_extract(sci_name, "[A-Z][a-z]+ "), " "),
         species = str_replace(sci_name, " ", "_")) |>
  left_join(read.csv("data/species-traits/eBird_taxonomy_v2024.csv"), by = c("sci_name" = "SCI_NAME")) |>
  dplyr::select(common_name, genus, species, FAMILY) |>
  rename(family = FAMILY) |>
  mutate(family = str_remove(str_extract(family, "[A-Z][a-z]+ "), " ")) |>
  #left-join the trait data, here the residuals from the model.
  left_join(eps_k, by = "common_name") |>
  # add color based on eps_k 
  arrange(eps_k) |>
  mutate(color = colorRampPalette(c("blue", "red"))(59))

#For bird, the mega-trees are loaded via megatrees::get_tree_bird_n100()
megatrees::get_tree_bird_n100()

#get phylogenetic trees
tree = get_tree(sp_list = species_list,
                     taxon = "bird",
                     scenario = "at_basal_node",
                     show_grafted = FALSE)
#5 species added at genus level (*) 
#4 species added at family level (**) 

plot(tree[[1]], tip.color =  species_list$color) #ah! okay this gives 100 different trees off the bat.

#okay, we'll loop this through to calculate these metrics across 100 trees, then average.
source("2.analysis-functions.R")
tree_stats <- make_trend_table(
  cols_list = c("iteration", "K", "K.p", "lambda", "lambda.p"), 
  rows_list = c(1:100))

pdf()

for(t in 1:length(tree)) {
  
  #pull the labels in the right order.
  labels = tree[[t]]$tip.label
  
  # Now get the species dataframe in the same phylogenetic order as the tip labels
  species_list$species = factor(species_list$species, levels = labels)
  species_list = species_list[order(species_list$species),]
  species_list$species = as.character(species_list$species)
  
  # Calculate Blomberg's K and Pagel's lambda
  blomK = phylosig(tree[[t]], species_list$eps_k, method = 'K', nsim = 999, test = TRUE)
  lambda = phylosig(tree[[t]], species_list$eps_k, method = 'lambda', nsim = 999, test = TRUE) 
  
  tree_stats$iteration[t] <- t
  tree_stats$K[t] <- blomK$K
  tree_stats$K.p[t] <- blomK$P
  tree_stats$lambda[t] <- lambda$lambda
  tree_stats$lambda.p[t] = lambda$P
  
  plot(tree[[t]], tip.color = species_list$color)
}

dev.off()

tree_stats <- tree_stats |>
  mutate(mean_K = mean(K),
         mean_K.p = mean(K.p),
         mean_lambda = mean(lambda),
         mean_lambda.p = mean(lambda.p))

#save results
write.csv(tree_stats, "model/2026.04.09_ch1_rmNOBO_final/phylogenetic_signal_test.csv", row.names = FALSE)

# Calculate Blomberg's K and Pagel's lambda
blomK = phylosig(tree[[1]], species_list$eps_k, method = 'K', nsim = 999, test = TRUE)      
blomK
#A K less than one implies that relatives resemble each other less than expected under Brownian motion evolution along the candidate tree. This could be caused by either departure from Brownian motion evolution, such as adaptive evolution that is uncorrelated with the phylogeny (i.e., homoplasy), or ‘‘measurement error’’ in the broad sense (see Discussion). A K greater than one implies that close relatives are more similar than expected under Brownian motion evolution. - from Blomberg et al. 2003
# K = 0.28, p = 0.025 okay! So we have a K less than one, and so the relatives are not related in their eps_K. yay! now, next step is to run iterations
lambda = phylosig(tree[[1]], species_list$eps_k, method = 'lambda', nsim = 999, test = TRUE) 
lambda
# lambda = 0.16976, p = 1

temp_table <- temp_table |>
  mutate(K = blomK$K,
         K.p = blomK$P,
         lambda = lambda$lambda,
         lambda.p = lambda$P)


X<-fastBM(tree[[1]],nsim=1000)
K<-apply(X,2,phylosig,tree=tree[[1]])
quantile(K,c(0.05,0.95))
