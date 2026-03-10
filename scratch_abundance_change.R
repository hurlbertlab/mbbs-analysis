library(dplyr)
library(beepr)
library(rstan)
library(bayesplot)
library(stringr)
source("2.analysis-functions.R")

#How has abundance changed on routes over time?
surveys_all <- read.csv("data/mbbs/surveys.csv") %>%
  get_observer_quality() %>%
  mutate(date = as.Date(date)) %>%
  mutate(jday = as.numeric(format(date, "%j")),
         #account for leap years in what the first start day could be
         start_day = as.numeric(format(as.Date(paste0(year,"-05-15")), "%j")),
         standard_jday = jday - start_day,
         year_standard = year - 1999) %>%
  group_by(route) %>%
  mutate(route_standard = cur_group_id())
  
surveys <- surveys_all |>
  group_by(year, year_standard) %>%
  summarize(nroutes = n(),
            total_abundance = sum(total_abundance),
            total_obs_quality = sum(observer_quality),
            average_jdate = mean(standard_jday)) |>
  mutate(average_abundance = total_abundance/nroutes,
         average_obs_quality = total_obs_quality/nroutes)


model <- lm(average_abundance ~ year_standard, data = surveys)
summary(model)
r2 <- summary(model)$r.squared |>
  round(5) #round to five digits (arbitrarily for text showing purposes)
#get confidence intervals
intercept <- model$coefficients[1] |>
  round(3)
beta_year <- model$coefficients[2] |>
  round(5)
confidence_intervals <- predict(model,
                                interval = "confidence", 
                                level = 0.95)

plot(x = surveys$year_standard,
     y = surveys$average_abundance,
     main = "NCMBBS Average Abundance by Year",
     xlab = "Year (1999 is Year 0)",
     ylab = "Average Abundance per Route",
     pch = 1) 
abline(model, col = "blue")
#add the background data from each route
#points(x = surveys_all$year_standard,
#       y = surveys_all$total_abundance,
#       cex = .5,
#       pch = 16)
#add regression line
#abline(lm(surveys$average_abundance ~ surveys$year_standard), col = "red")
#add r2
text(x = 20, y = 295, paste("r2 =", r2))
text(x = 17, y = 288, paste("y = ", intercept, "+", beta_year,"*Year"))
#add confidence intervals as shaded areas
polygon(
  x = c(surveys$year_standard, rev(surveys$year_standard)),
  y = c(confidence_intervals[, "lwr"],
    rev(confidence_intervals[, "upr"])),
  col = rgb(0, 0, 1, 0.2),
  border = TRUE
)



model2 <- glm(average_abundance ~ year_standard + average_obs_quality,
             data = surveys)
summary(model2)
# Calculate R-squared
deviance <- summary(model2)$deviance
null_deviance <- summary(model2)$null.deviance
rsquared <- 1 - (deviance / null_deviance)

# Print the R-squared value
print(rsquared) # model r2 = 0.51, model2 r2 = 0.59 so adding observer information captures about 8% of the variation
# I wonder if date doesn't capture the rest

model3 <- glm(average_abundance ~ year_standard + average_obs_quality + average_jdate, 
              data = surveys)
summary(model3) #nah, jdate's not significant.

summary(model2)$aic
summary(model3)$aic
#and the models are within 2 AIC so def not worth it to include an average jdate. Probably has a role to play in predicting the count on each route, but the average jdate doesn't predict the average count.
#so in a bayes model, it would be a predictor at the route level.

#lets model this in a Bayes format ^u^ ignore average abundance across routes and instead model abundance across routes with one effect of year, obs, and jday, but routes can have different intercepts
datstan <- 
  list(
    N = nrow(surveys_all),
    Nrt = length(unique(surveys_all$route)),
    abundance = surveys_all$total_abundance,
    julian_day = surveys_all$standard_jday,
    observer_quality = surveys_all$observer_quality,
    year = surveys_all$year_standard,
    rt = surveys_all$route_standard
  )

#specify stan model code
stan_model_file <- "scratch_abundance_change.stan"
#compile
stan_model <- stan_model(file = stan_model_file)
beepr::beep()

#fit the model to the data
fit <- sampling(stan_model, 
                data = datstan,
                chains = 4,
                cores = 4,
                iter = 5000,
                warmup = 1000
                )
beepr::beep() #perf, takes about a minute fitting the different route-year combos

save_stan_traceplot_pdf(fit,
                    file = "scratch_abundance_change_model_pairs.pdf",
                    pars = NULL,
                    n_per_page = 4)

fit_summary <- summary(fit)
rownames <- row.names(fit_summary$summary)
fit_final <- as.data.frame(fit_summary$summary)
fit_final$rownames <- rownames
fit_final <- fit_final |>
  relocate(rownames, .before = mean) |>
  #rename numeric columns
  rename_with(~ paste0("conf_", .), .cols = matches("^[0-9]")) |>
  #remove %s in column names
  rename_with(~ str_remove(., "%"), .cols = everything()) |>
  mutate(actual_intercept = 
           ifelse(str_detect(rownames, "a\\[|a_bar"), exp(mean), NA),
         exponentiate = exp(mean)
         )
#b_year_bar
#-0.01174385801
#1% decrease per year
#ah hell, maybe it does make sense to fit a slope for each route for each year and then take the average.
#average route (a_bar) = starts with 280 birds * the average year effect across routes..
280*0.9883248^1 - 280
#decrease of 3.2 birds per route per year.
#so this estimates the average route has lost...
280*0.9883248^27 - 280
#76 birds in 27th year

surveys$average_abundance[surveys$year_standard == 0] - surveys$average_abundance[surveys$year_standard == 26]
#m but really the average route has lost 43 birds.
#well, no, that's not the same.
average_loss <- surveys_all %>%
  group_by(route) %>%
  mutate(minyear = min(year),
         maxyear = max(year)) %>%
  filter(year == minyear | year == maxyear) %>%
  summarize(loss = total_abundance[year == minyear] - total_abundance[year == maxyear])
#hm! from this what I'm actually getting is that. Probably I SHOULD fit a different year intercept for each route. Some of these are not declining. But does that change things? Does that matter? I want to know what happens on the AvERAGE ROUTE
#and this here just has like. Some routes have more birds at the end. But in the area overall what's happening?
mean(average_loss$loss)
#Taking the average only at the end the average route has lost only 29 birds over time. But there's some big variance there.
#I suppose with some of those rt-years there's really bad observers and then really good observers at the end WHICH IS WHY the bayes model accounts for that difference in observer quality.
#Alright but also, each route can change differently over the years AND much like we have a_bar AVERAGE ROUTE INTERCEPT we can have y_bar AVERAGE YEAR EFFECT ACROSS ROUTES
#^u^ so if I come spend more time on this again... I can fit a different year effect across routes.
#would be nice to figure out how to graph these results.......... like with the lm model the same way. With the data and then these predicted a_bar + Byear effect.
#lmao can't I just do that?
# x values (years)
x <- seq(min(datstan$year), max(datstan$year), length.out = 100)
a_bar <- fit_final[fit_final$rownames == "a_bar",2]
beta_year <- fit_final[fit_final$rownames == "b_year_bar",2]
# predicted counts
y_hat <- exp(a_bar + beta_year * x)

# add fitted line
lines(x, y_hat, col = "red", lwd = 2)


write.csv(fit_final, "scratch_abundance_change_fitsummary.csv", row.names = FALSE)
