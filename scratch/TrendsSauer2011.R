#load mbbs library
library(dplyr)
library(stringr)
library(MCMCpack)
library(ggplot2)
library(interactions)
library(car)
library(lmtest)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
library(GGally)
library(gvlma)
library(rstatix)
library(tidyverse)
library(performance)
library(report)
library(ggstance)
library(jtools)
library(caret)
library(kableExtra)
library(gtools)
library(MASS)
library(pROC)
library(countreg)
library(gridExtra)
library(countreg)
library(bbsBayes)

load("C:/git/mbbs_orange.rda")
load("C:/git/mbbs_durham.rda")
load("C:/git/mbbs_chatham.rda")
load("C:/git/mbbs_survey_events.rda")

#All predictors are random in this model? year, stratum, and observer_effects - here counted as year, route_num, observer_ID? Some of the stratum stuff doesn't matter, because that's comparing like, all the routes at the state and then national level. Here our best stratum would be route_num vs whole survey.

mbbs_all <- bind_rows(mbbs_orange, mbbs_chatham, mbbs_durham) %>%
  left_join(mbbs_survey_events, by = c("mbbs_county", "year", "route_num")) %>%
  mutate(route_ID = route_num + case_when(
    mbbs_county == "orange" ~ 100L,
    mbbs_county == "durham" ~ 200L,
    mbbs_county == "chatham" ~ 300L,))


#let's just try REVI with an overdispersed poisson from Week 7 stats
test <- mbbs_all %>% filter(common_name == "Red-eyed Vireo")
revi.nbm <- glm.nb(count ~ year + (1|route_ID) + (1|observer_ID), data = test)
summary(revi.nbm)

MCMChpoisson(
  fixed = count ~ year + observer_ID + route_num,
  random = ~year + observer_ID + route_num,
  group = "mbbs_county",
    data = test,
  r=q
)
