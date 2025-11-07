#heatmap of changes
#take long term mean, calculate anamaly for each year
#good/bad years

library(dplyr)
library(stringr)

#dendrogram, puts species with a high correlation close together

#what's the temporal anamoly in each yr for each species, focus on difference between species. Don't worry about spatial at all
#group by background eg. forest amount for a non-arbitrary grouping amount, like county is totally arbitrary

#calculate whole route, don't do quarter route
#nah, calculate across the whole mbbs good/bad years anamaly from mean, species that aren't really synchronous won't have as extreme changes from their mean each year
#group sp. by similarity in interannual responses

#https://bioinformatics.ccr.cancer.gov/docs/data-visualization-with-r/Lesson5_intro_to_ggplot/
#https://www.datanovia.com/en/blog/how-to-create-a-beautiful-interactive-heatmap-in-r/ 

mbbs <- read.csv("data/analysis.df.csv", header = TRUE) %>%
  #create a datapoint for each sp per year
  group_by(common_name, year) %>%
  summarize(t_count = sum(count)) %>%
  ungroup() %>%
  group_by(common_name) %>%
  #better mean calculation for testing anomaly might be with a running mean?
  mutate(long_term_mean = mean(t_count),
         anomaly = t_count - long_term_mean,
         rollmean = zoo::rollmean(t_count, k=4, fill = NA, align = 'right'), #want a dif fill value
         rollmean_anomaly = t_count - rollmean) %>%
  ungroup()
mutate(avg_sales3 = rollmean(sales, k=3,  align='center'))


stopdata <- read.csv("data/mbbs/mbbs_stops_counts.csv") %>%
  ##########
# testing
#filter(common_name %in% c("Acadian Flycatcher", "Wood Thrush", "Northern Bobwhite", "Indigo Bunting", "Northern Cardinal")) %>%
############
#make unique quarter route identifier
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
  #let's pull out the species that are unscientific, waterbirds, etc.
  filter(!common_name %in% excluded_species) %>%
  filter_to_min_qrts(min_quarter_routes = 20) %>%
  #now we only have species of interest, create a species_id 
  group_by(common_name) %>%
  mutate(sp_id = cur_group_id()) %>%
  ungroup() %>%
  group_by(common_name, quarter_route) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    long_mean = mean(q_rt_count)) 
