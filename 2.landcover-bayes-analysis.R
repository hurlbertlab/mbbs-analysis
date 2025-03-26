#############################
#
# Take the stop-level data we have on birds
# and run an analysis of the effects of landcover
# on species population trends
# at the quarter-route level. 
#
#############################

library(dplyr)
library(rstan)
library(stringr)

source("2.species-trend-estimate-functions.R")

#read in data we need
dev <- read.csv("data/nlcd-landcover/nlcd_annual_running_max_developed.csv") %>%
  #get percent developed by route-quarter
  mutate(quarter_route = as.integer(case_when(stop_num > 15 ~ 4,
                                              stop_num > 10 ~ 3,
                                              stop_num > 5 ~ 2,
                                              stop_num > 0 ~ 1))) %>%
  group_by(route, quarter_route, year) %>%
  #summarize bc we only need to keep 1 entry per route quarter
  summarize(rmax_dev_quarter = mean(running_max_perc_developed)) %>%
  ungroup() %>%
  #now, we're going to take an extra step and center this data. What this is going to do is help keep everything interpretable in the model. So, rather than our intercepts being at 0% urbanization, our intercepts will be at mean_urbanization
  mutate(mean_dev = mean(rmax_dev_quarter),
         sd_dev = sd(rmax_dev_quarter),
         centered_rmax_dev = rmax_dev_quarter - mean_dev,
         z_score_rmax_dev = ((rmax_dev_quarter - mean_dev)/ sd_dev))
#so mean urbanization is about a quarter urbanized.
#if we use the centered rmax dev, the interpretation of the intercept is the intercept at the mean urban % (a quarter urbanized)
#if we use the z score rmax dev, which has been standardized, 
#the intercept represents the expected bird counts at the average urbanization (a quarter urbanized)
#a 1 unit change in the z score is 1 SD above the mean. the SD is 21.1% change. So 

max_nlcd_year <- max(dev$year)

stopdata <- read.csv("data/mbbs/mbbs_stops_counts.csv") %>%
  #make unique quarter route notifier
  mutate(quarter = case_when(stop_num > 15 ~ 4,
                                   stop_num > 10 ~ 3,
                                   stop_num > 5 ~ 2,
                                   stop_num > 0 ~ 1),
         quarter_route = paste0(route,"-",quarter)) %>%
  group_by(quarter_route) %>%
  mutate(q_rt_standard = cur_group_id()) %>%
  ungroup() %>%
  #need to sum the counts to the quarter-route, right now by each individual stop, which is a different analysis unit from the quarter-route
  group_by(year, quarter_route, common_name, sci_name, q_rt_standard, route, quarter) %>%
  summarize(q_rt_count = sum(count)) %>%
  ungroup() %>%
  #keep only the data that's up to the year we have nlcd data for
  filter(year <= max_nlcd_year) %>%
  #standardize year
  standardize_year(starting_year = 2012) %>% #current stopdata max year is 2023 (huh! pull the 2024 data, might as well use it! actually this might be happening b/c we don't have 2024 landcover data..... anyway! we'll standardize on year 2012 (abt halfway btwn 2000 and 2023))
#maybe year should be GENUINELY standardized. that would make the intercept lean towards the years where we have the most data.
  ####let's genuinely standardize year. like, the same way we z_score development.
  mutate(z_score_year = (year-mean(year))/sd(year))
#mean year is 2014.36
#sd year is 7.87

## step to pull out only the species that have enough non-zero data included.

forest <- read.csv("data/nlcd-landcover/nlcd_annual_sum_forest.csv") %>%
  #summarize bc we only need 1 entry per route quarter
  group_by(route, quarter_route, year, perc_forest_quarter) %>%
  summarize() %>%
  ungroup

#Let's leftjoin, the dataset isn't so huge at 160k entries before filtering
  stopdata <- stopdata %>%
    left_join(dev, by = c("route", "quarter" = "quarter_route", "year")) %>%
    left_join(forest, by = c("route", "quarter" = "quarter_route", "year"))

#where to save stan code and fit
save_to <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.03.25_designing_fullystandardizeyear"
#if the output folder doesn't exist, create it
if (!dir.exists(save_to)) {dir.create(save_to)}

# really basic glm, how would this go?
testdata <- stopdata %>% 
  filter(common_name == "Wood Thrush")

test <- glm(count ~ year + rmax_dev_quarter, family = quasipoisson, data = testdata)
summary(test)

# let's start simple. all we want is to predict the intercepts of the different quarter routes
datstan <- list(
  N = nrow(testdata), #number of observations
  Nqrt = length(unique(testdata$q_rt_standard)), #number of unique quarter routes
  qrt = testdata$q_rt_standard, #qrt index for each observation
  perc_dev = testdata$z_score_rmax_dev, #percent developed for each observation
  year = testdata$year_standard, #year for each observation
  C = testdata$q_rt_count #count data for each observation
  )

#stan model specified in landcover_model.stan
stan_model_file <- "2.landcover_model.stan"

#compile the stan model
stan_model <- stan_model(file = stan_model_file)
beepr::beep()
#save the model text
file.copy(stan_model_file, save_to, overwrite = TRUE)

#fit the model to the data
fit <- sampling(stan_model,
                data = datstan,
                chains = 4,
                cores = 4, 
                iter = 2000,
                warmup = 1000)
beepr::beep()

fit_summary <- summary(fit)
fit_final <- as.data.frame(fit_summary$summary)
fit_final$rownames <- rownames(fit_final)
fit_final <- fit_final %>%
  relocate(rownames, .before = mean) %>%
  mutate(exp_mean = exp(mean),
         exp_minconf = exp(`2.5%`),
         exp_maxconf = exp(`97.5%`),
         q_rt_standard = as.numeric(ifelse(
           str_detect(rownames, "a\\[\\d+\\]"),
           str_extract(rownames, "\\d+"),
           NA))
         ) %>%
  filter(str_detect(rownames, "a_z") == FALSE)
#save fitfinal
#Save the fit
saveRDS(fit, paste0(save_to, "/stanfit.rds"))
#Save the summary
write.csv(fit_final, paste0(save_to,"/fit_summary.csv"), row.names = FALSE)


#plot the means without any information added
c <- testdata %>%
  group_by(q_rt_standard) %>%
  summarize(mean_count = mean(q_rt_count))
plot(c$q_rt_standard, c$mean_count,
     ylim = c(0,12),
     pch = 19) #this is our raw average count.
  #add the posterior means
#after fitting the model we want to plot the posterior means on top, should all shrink towards the overall mean because of the effect of pooling. Add posterior intervals as well.
fit_plot <- fit_final %>% 
  filter(is.na(q_rt_standard) == FALSE)
  points(fit_plot$q_rt_standard, fit_plot$exp_mean, col = "purple", pch = 19)
  segments(fit_plot$q_rt_standard,
           fit_plot$exp_minconf,
           fit_plot$q_rt_standard,
           fit_plot$exp_maxconf,
           col = "purple")
  