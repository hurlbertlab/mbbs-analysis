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

#read in data we need
stopdata <- read.csv("data/mbbs/mbbs_stops_counts.csv") %>%
  #make unique quarter route notifier
  mutate(quarter_route = case_when(stop_num > 15 ~ 4,
                                   stop_num > 10 ~ 3,
                                   stop_num > 5 ~ 2,
                                   stop_num > 0 ~ 1),
         quarter_route = paste0(route,"-",quarter_route)) %>%
  group_by(quarter_route) %>%
  mutate(q_rt_standard = cur_group_id()) %>%
  ungroup()
dev <- read.csv("data/nlcd-landcover/nlcd_annual_running_max_developed.csv") %>%
  #get percent developed by route-quarter
  mutate(quarter_route = case_when(stop_num > 15 ~ 4,
                                   stop_num > 10 ~ 3,
                                   stop_num > 5 ~ 2,
                                   stop_num > 0 ~ 1)) %>%
  group_by(route, quarter_route, year) %>%
  #summarize bc we only need to keep 1 entry per route quarter
  summarize(rmax_dev_quarter = mean(running_max_perc_developed)) %>%
  ungroup()

forest <- read.csv("data/nlcd-landcover/nlcd_annual_sum_forest.csv") %>%
  #summarize bc we only need 1 entry per route quarter
  group_by(route, quarter_route, year, perc_forest_quarter) %>%
  summarize() %>%
  ungroup

#Things don't have to be left-joined, bc we will datstan everything together.

#where to save stan code and fit
save_to <- "Z:/Goulden/mbbs-analysis/model_landcover/2025.03.24_designing"
#if the output folder doesn't exist, create it
if (!dir.exists(save_to)) {dir.create(save_to)}

# really basic glm, how would this go?
testdata <- stopdata %>% 
  filter(common_name == "Wood Thrush")

test <- glm(count ~ year + rmax_dev_quarter, family = quasipoisson, data = testdata)

# let's start simple. all we want is to predict the intercepts of the different quarter routes
datstan <- list(
  N = nrow(testdata), #number of observations
  Nqrt = length(unique(testdata$q_rt_standard)), #number of unique quarter routes
  qrt = testdata$q_rt_standard, #qrt index for each observation
  C = testdata$count #count data for each observation
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
         )

#plot the means without any information added
c <- testdata %>%
  group_by(q_rt_standard) %>%
  summarize(mean_count = mean(count))
plot(c$q_rt_standard, c$mean_count,
     ylim = c(0,3.5),
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