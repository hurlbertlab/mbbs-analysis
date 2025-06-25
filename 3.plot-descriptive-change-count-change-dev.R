##############################
#
# Make a descriptive plot
# of how route change over time in each quarter route
# relates to change in count for each species
# and print it pdf
#
###########################
library(dplyr)
library(stringr)
library(ggplot2)
library(ggpubr)
library(gridExtra)
source("2.species-trend-estimate-functions.R")


#load in dataset and create change per change
  #development
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
  max_nlcd_year <- max(dev$year)

  #forest
    forest <- read.csv("data/nlcd-landcover/nlcd_annual_sum_forest.csv") %>%
      #summarize bc we only need 1 entry per route quarter
      group_by(route, quarter_route, year, perc_forest_quarter) %>%
      summarize() %>%
      ungroup
    
  
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
      standardize_year(starting_year = 2012) %>% 
      mutate(z_score_year = (year-mean(year))/sd(year)) %>%
      #mean year is 2014.36
      #sd year is 7.87
      #let's pull out the species that are unscientific, waterbirds, etc.
      filter(!common_name %in% excluded_species) %>%
      #let's also remove species that don't meet our minimum bound observations 
      #set right now at 20 quarter routes
      #this excludes species that are not seen enough to make any sort of confident estimate on their trends, although one benefit of the bayes model is that the number of datapoints you need is 0, the slopes we fit are also going to SPAN 0 and be insigificant. 
      #this represents species that just do not commonly breed in the area and that we ought not make assumptions about anyway bc this isn't their usual breeding location.
      filter_to_min_qrts(min_quarter_routes = 20) %>%
      #let's left_join in the landcover data
      left_join(dev, by = c("route", "quarter" = "quarter_route", "year")) %>%
      left_join(forest, by = c("route", "quarter" = "quarter_route", "year")) %>%
      #time for the new stuff. For a change for change analysis, rather than each data point being the count and the urbanization%, each datapoint needs to be a lag count and lab urbanization percent. let's also have a years_btwn variable thats how long the latest lag is. let's sort the data first.
      group_by(common_name, quarter_route) %>%
      arrange(year, .by_group = TRUE) %>%
      mutate(
        change_count = q_rt_count - lag(q_rt_count),
        change_dev = rmax_dev_quarter - lag(rmax_dev_quarter),
        years_btwn = year - lag(year)) %>%
      ungroup() %>%
      #remove the NA years (first record of each quarter route) 
      filter(is.na(change_count) == FALSE) %>%
      #hmmmm and remake year_standard while we're here? now we've changed it. rn year standard is based on the mean year.
      standardize_year(starting_year = 2012) %>%
      #and now like, arrange nicely for plotting
      arrange(common_name, q_rt_standard, year) %>%
      mutate(county = str_sub(.$route, start = 1, end = 4))

species_list <- unique(stopdata$common_name)

pdf(file = "figures/descriptive-change-count-change-dev-8sp.pdf",
    width = 14,
    onefile = TRUE)
par(mfrow = c(2,4))
plot_lst <- vector("list", length = length(species_list))
for(i in 1:length(species_list)) {
  
  x <- stopdata %>%
    filter(common_name == species_list[i])
  
  # cthm <-  x %>% 
  #   filter(county == "cthm") %>%
  #   group_by(q_rt_standard) %>%
  #   ggplot(aes(x=change_dev, y=change_count, color = q_rt_standard)) +
  #   ggtitle(label = paste0(x$common_name[1],", Chatham")) +
  #   labs(x = "Change in % Urbanization",
  #        y = "Change in Count") +
  #   geom_point(position = "jitter") +
  #   #geom_segment(aes(
  #   # xend=c(tail(change_count, n=-1), NA), 
  #   #  yend=c(tail(change_dev, n=-1), NA)
  #   #)
  #   #arrow=arrow(length=unit(0.3,"cm"))
  # # )  +
  #   theme(legend.position="none")
  # 
  # drhm <-  x %>% 
  #   filter(county == "drhm") %>%
  #   group_by(q_rt_standard) %>%
  #   ggplot(aes(x=change_dev, y=change_count, color = q_rt_standard)) +
  #   ggtitle(label = paste0(x$common_name[1],", Durham")) +
  #   labs(x = "Change in % Urbanization",
  #        y = "Change in Count") +
  #   geom_point(position = "jitter") +
  #   #geom_segment(aes(
  #   #  xend=c(tail(change_count, n=-1), NA), 
  #   #  yend=c(tail(change_dev, n=-1), NA),
  #   #  
  #   #)
  #   #arrow=arrow(length=unit(0.3,"cm"))
  #   #) +
  #   theme(legend.position="none")
  # orng <-  x %>% 
  #   filter(county == "orng") %>%
  #   group_by(q_rt_standard) %>%
  #   ggplot(aes(x=change_dev, y=change_count, color = q_rt_standard)) +
  #   ggtitle(label = paste0(x$common_name[1],", Orange")) +
  #   labs(x = "Change in % Urbanization",
  #        y = "Change in Count") +
  #   geom_point(position = "jitter") +
  #   #geom_segment(aes(
  #   #  xend=c(tail(change_count, n=-1), NA), 
  #   #  yend=c(tail(change_dev, n=-1), NA)
  #   #)
  #   #arrow=arrow(length=unit(0.3,"cm"))
  #   #) +
  #   theme(legend.position="none")
  all <- x %>%
    group_by(q_rt_standard) %>%
    ggplot(aes(x=change_dev, y=change_count, color = county)) +
    ggtitle(label = paste0(x$common_name[1])) +
    labs(x = "Change in % Urbanization",
         y = "Change in Count") +
    geom_point(position = "jitter") 
    #geom_segment(aes(
    #  xend=c(tail(change_count, n=-1), NA), 
    #  yend=c(tail(change_dev, n=-1), NA)
    #), 
    #arrow=arrow(length=unit(0.3,"cm"))
    #)
  
  #to prevent problems with corrupting the pdf file wrap the ggarrange/ggplot with the print() function
  #print(ggarrange(orng, drhm, cthm, all,
  #          ncol = 4, nrow = 1))
  plot_lst[[i]] <- all
  #print(all)
  
  #print just all, set plot into square, print multiple species per 
  #page

}

allgraphs <- marrangeGrob(plot_lst, nrow = 2, ncol = 4)
allgraphs
dev.off()








x_first <- x %>%
  filter(q_rt_standard == 1)

plot(x = x_first$change_count,
     y = x_first$change_dev,
     ylab = "Change in % Urbanization",
     xlab = "Change in Count")

plot(x = x$change_dev,
     y = x$change_count,
     ylab = "Change in % Urbanization",
     xlab = "Change in Count",
     col = x$q_rt_standard)

x_first %>% 
  ggplot(aes(x=change_dev, y=change_count, label=year)) +
  geom_point() +
  geom_segment(aes(
    xend=c(tail(change_count, n=-1), NA), 
    yend=c(tail(change_dev, n=-1), NA)
  )#,
  #arrow=arrow(length=unit(0.3,"cm"))
  ) 


