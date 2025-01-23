#plot the focus species of the bsft across all counties.
library(ggplot2)
library(ggpubr)

plot_one_species_one_county <- function(mbbs_county, species, county_name) {
  
  
  #get the mean number seen across all routes, this code accounts for that there are multiple stops per route 2020 onwards with the [summarize(sum = sum(count)) %>% group_by(year) %>% summarize(mean = mean(sum))] rather than just using [%>% summarize(mean = mean(count)] 
  
  #We do need to also add a check for the number of routes run each year, bc if a route wasn't run that needs to be incorporated 
  ave <- mbbs_county %>% filter(common_name == species) %>% group_by(year, route_num) %>% summarize(sum = sum(count)) %>% group_by(year) %>% summarize(mean = mean(sum)) #average is mean by year
  
  route_totals <- mbbs_county %>% 
    filter(common_name == species) %>%
    group_by(year, route_num) %>% 
    summarize(sum = sum(count)) %>%#count in each route each year
    filter(year >= 2002)
    
  #plot
  #plot each route, plot average across route (line + points), add trend line (lm), add species name and county to top of graph
  #mbbs_county %>% filter(common_name == species) %>%  ggplot(aes(x = year, y = count)) +
  # xlim(1999,2022) +
  # geom_line(aes(group = route_num), color = "grey") +
  #  geom_line(data = ave, aes(x = year, y = mean)) +
  # geom_point(data = ave, aes(x = year, y = mean)) +
  #  stat_smooth(method = "lm", se = FALSE) + 
  # labs(title=paste(species), subtitle = county_name) +
  #theme_bw()
  
  route_totals %>% ggplot(aes(x=year, y=sum)) +
    xlim(2002,max(mbbs_county$year)) +
    geom_line(aes(group = route_num), color = "grey") +
    geom_line(data = ave, aes(x = year, y = mean)) +
    geom_point(data = ave, aes(x = year, y = mean)) +
    stat_smooth(method = "lm", se = FALSE) + 
    ylab("Count")+
    xlab("Year")+
    ylim(0,40)+  ##40 for Wood Thrush, 80 for CAWR, 20 for WEVI
    labs(title=paste(species), subtitle = county_name) +
    theme_bw()
  
}

plot_one_species_county_breakdown <- function (species) {
  
  p1 <- plot_one_species_one_county(mbbs[mbbs$mbbs_county == "orange",], species, "Orange")
  p2 <- plot_one_species_one_county(mbbs[mbbs$mbbs_county == "durham",], species, "Durham")
  p3 <- plot_one_species_one_county(mbbs[mbbs$mbbs_county == "chatham",], species, "Chatham")
  p4 <- plot_one_species_one_county(mbbs, species, "All Counties")
  
  ggarrange(p1,p2,p3,p4, nrow = 1)
  
}

plot_one_species_county_breakdown("Wood Thrush")
