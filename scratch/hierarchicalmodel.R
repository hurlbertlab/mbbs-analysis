library(dplyr)
library(lme4)
library(beepr)

#read in data
mbbs <- read.csv("data/analysis.df.csv", header = T)
survey.events <- read.csv("data/survey.events.csv", header = T)

#need to leftjoin survey.events to mbbs to get the primary observers column (which should also be a numerical rather than name! don't forget! this is gonna happen!)

#get species list to filter through
species.list <- unique(mbbs$common_name)

#we want the model to be like.... (filtered for each species) count ~ year + observer + (1|route_num) + (1|mbbs_county) +measureofaccountingfordispersion??
#how do I account for averages in this? And the effort of how many times the model was run in each year? What's the purpose of survey_event and taking ave in species-trend-estimates.R if we don't account for effort here? Ask Allen, or work through this with another student

#there are likely. like, checks to run through first. To make sure the data doesn't violate the assumptions of the model! We will check this slightly later, let's just check about running a lmer first

#trim species list
species.sample <- c(species.list[6], species.list[14], species.list[66])

sample <- mbbs %>% filter(common_name == species.sample[3])
sample.hierarchical <- lmer(count ~ year + (1|route) + (1|mbbs_county), data = mbbs)
summary(sample.hierarchical)
#okay! so, I removed the observer bc even when playing around it should really be combined with the primary observers, leftjoin that on the next time you come back. Investigate what boundary(singular) (console suggests help('isSingular')) rlly means. I think in this case, it's just that we've got multiple levels but county is not actually having an effect here. As you can see we used route rather than route_num b/c now route is distinct. This probably is not accounting well for sampling effect tho.
#LOL!! We have to take the average of the counts (and within that account for sampling effort) b/c the data from 2019+ is by stop, and the data before that is by route. Now, you can probably still aggregate counts only at the route level, and not the stop level, but you DO have to aggregate for the data to be comprable.

model <- lmer(math_score ~ gender + (1 | classroom_id) + (1 | school_id), data = data)

for(i in 1:species.sample) {
  
}
