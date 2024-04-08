#extract and save landfire data

library(tidyr)
library(dplyr)
library(sf) #this is the spatial package
library(terra)
library(stringr)
library(beepr)

#read in the longleaf files into a stack
path <- "spatial/shapefiles/landfire/"
rastlist <- list.files(path = path, pattern = '.tif$', all.files=TRUE, full.name=TRUE)
stack <- rast(rastlist)

#load the spatial dataset 
mbbs_buffers <- read.csv("spatial/Routestops.csv", header = TRUE) %>%
  dplyr::select(- c(notes)) %>% #remove notes column
  mutate(ID = row_number()) %>%
  relocate(ID, .before = County_Route_Stop)

#make 400m buffers
mbbs_buffers <- st_as_sf(mbbs_buffers, coords = c('lon', 'lat'), crs = 4269)
#transform to a meters based crs
mbbs_buffers <- st_transform(mbbs_buffers, crs = 5070) #meters, NC 
#buffer each point by 400m, now geometry is polygons
mbbs_buffers <- st_buffer(mbbs_buffers, dist = 400) 

#put the buffers in the same projection as the landfire data
mbbs_buffers <- st_transform(mbbs_buffers, crs(stack))

#clip rasters
bufferstack <- mask(stack, mbbs_buffers)
bufferstack <- crop(stack, mbbs_buffers)

#extract data
extracted <-  terra::extract(x = bufferstack, y = mbbs_buffers, df = TRUE)

#add back in the route information
landfire_raw <- left_join(extracted, mbbs_buffers, by = "ID") %>%
  #get the total number of pixels on each route_stop
  group_by(ID) %>%
  mutate(numpix = n()) %>% #year doesn't matter yet, because all we have is IDs and each year is a column
  ungroup() %>%
  #get the total number of pixels on each route
  group_by(County_Route) %>%
  mutate(totpix_route = n()) %>%
  ungroup() %>%
  #add in route stop information
  mutate(routequarter = case_when(stop_num < 6 ~ 1,
                                  stop_num < 11 ~ 2,
                                  stop_num < 16 ~ 3,
                                  stop_num > 15 ~ 4),
         routehalf = case_when(routequarter < 3 ~ 1,
                               routequarter > 2 ~ 2)) %>% 
  #get total number of pixels on each route quarter
  group_by(County_Route, routequarter) %>%
  mutate(totpix_quarter = n()) %>%
  ungroup %>%
  #pivot time! Now each row is a pixel + a year. 
  pivot_longer(cols = landfire_2001evh:landfire_2022evh, names_to = "year", names_prefix = "landfire_", values_to = "raw_landtype") %>% #pivot
  mutate(year = as.numeric(str_extract(year, "[0-9]+"))) %>% #make year a number
  #convert landtypes to 2001 style
  mutate(converted_landtype = case_when(
    year > 2014 &
      raw_landtype > 200 & raw_landtype < 206 ~ 104,
    year > 2014 &
      raw_landtype > 205 & raw_landtype < 211 ~ 105,
    year > 2014 &
      raw_landtype > 210 & raw_landtype < 230 ~ 106,
    year > 2014 &
      raw_landtype == 230 ~ 107,
    year > 2014 & 
      raw_landtype > 99 & raw_landtype < 105 ~ 108,
    year > 2014 & 
      raw_landtype > 104 & raw_landtype < 111 ~ 109,
    year > 2014 & 
      raw_landtype > 110 & raw_landtype < 126 ~ 110,
    year > 2014 & 
      raw_landtype > 125 & raw_landtype < 151 ~ 111,
    year > 2014 & 
      raw_landtype > 150 & raw_landtype < 200 ~ 112,
    TRUE ~ raw_landtype
  )) %>%
  arrange(ID, year, converted_landtype) %>% #ease of readability
  #plenty of landtypes that we don't care about. Let's filter them out. For now, all we're interested in is that successional forest height. So we want landtypes 106-109. If interested in just forest, > 100 < 200, if interested in forest + herb in the later years of landfire, only >100
  filter(converted_landtype >= 106 & converted_landtype <= 109) %>%  
  #get route level information and summaries
  group_by(County_Route, year, raw_landtype) %>%
  mutate(frequency_raw_route = n()) %>%
  ungroup() %>%
  group_by(County_Route, year, converted_landtype) %>%
  mutate(frequency_converted_route = n(),
         percent_converted_route = (frequency_converted_route/totpix_route)*100) %>%
  ungroup() %>%
  group_by(County_Route, year) %>% #only by route and year because we've filtered to just the landtypes we're interested in looking at.
  mutate(frequency_succesional_route = n(),
         percent_succesional_route = (frequency_succesional_route/totpix_route)*100) %>%
  ungroup()
#% each land type
#% filtered landtypes
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Below here needs updating.
  
  group_by(County_Route_Stop, year, raw_landtype) %>%
  mutate(frequency_raw = n()) %>%
  ungroup() %>%
  #2014 has shrub information included in the low 100s information, needs to be 108 or greater for those years
  #filter(year != 2014 | (year == 2014 & raw_landtype >= 108)) %>% #comment out, using high shrub info now.
  #add in total pixels for each route
  group_by(County_Route, year) %>%
  mutate(totpix_route = sum(numpix)) %>%
  ungroup() %>%
  #add in total pixels for each route quarter
  group_by(County_Route, year, routequarter) %>%
  mutate(totpix_quarter = sum(numpix)) %>%
  ungroup() %>%
  #add in route_level summaries
  group_by(County_Route, year, converted_landtype) %>%
  mutate(frequency_route = sum(frequency),
         percent_route = (frequency_route/totpix_route)*100) %>%
  ungroup() %>%
  group_by(County_Route, year) %>% 
  mutate(median_route = median(converted_landtype),
         mean_route = mean(converted_landtype),
         q3_route = quantile(converted_landtype, 0.75),
         percent_succesional_route <- (sum(frequency)/totpix_route)*100) %>%
  ungroup() %>%
  #add in quarter route summaries
  group_by(County_Route, year, converted_landtype, routequarter) %>%
  mutate(frequency_quarter = sum(frequency),
         percent_quarter = (frequency_quarter/totpix_quarter)*100) %>%
  ungroup() %>%
  group_by(County_Route, year, routequarter) %>%
  mutate(median_quarter = median(raw_landtype),
         mean_quarter = mean(raw_landtype),
         q3_quarter = quantile(raw_landtype, 0.75),
         percent_succesional_quarter <- (sum(frequency)/totpix_route)*100) %>%
  ungroup()

#next step is to get the 2022-2016 differences. We'll do this first by route
values_year <- function(landfire_raw, select_year) {
  values_year <- landfire_raw %>% 
    group_by(County_Route) %>%
    filter(year == select_year) %>%
    distinct(County_Route, .keep_all = TRUE)
}

values_year_quarter <- function(landfire_raw, select_year) {
  values_year <- landfire_raw %>% 
    group_by(County_Route, routequarter) %>%
    filter(year == select_year) %>%
    distinct(County_Route, .keep_all = TRUE)
}

v2016 <- values_year(landfire_raw, 2016) %>%
  rename(y16mean_route = mean_route) %>%
  rename(y16median_route = median_route) %>%
  select(County_Route, y16mean_route, y16median_route)

v2022 <- values_year(landfire_raw, 2022) %>%
  rename(y22mean_route = mean_route) %>%
  rename(y22median_route = median_route) %>%
  select(County_Route, y22mean_route, y22median_route)

#okay good! comparing between those two, values get higher. 
dif <- left_join(v2016, v2022, by = "County_Route") %>%
  mutate(difmean_22_16 = y22mean_route - y16mean_route,
         difmedian_22_16 = y22median_route - y16median_route)

#by routequarter
v2016 <- values_year_quarter(landfire_raw, 2016) %>%
  rename(y16mean_quarter = mean_quarter) %>%
  rename(y16median_quarter = median_quarter) %>%
  select(County_Route, routequarter, y16mean_quarter, y16median_quarter)

v2022 <- values_year_quarter(landfire_raw, 2022) %>%
  rename(y22mean_quarter = mean_quarter) %>%
  rename(y22median_quarter = median_quarter) %>%
  select(County_Route, routequarter, y22mean_quarter, y22median_quarter)

#okay good! comparing between those two, values get higher. 
dif_quarter <- left_join(v2016, v2022, by = c("County_Route", "routequarter")) %>%
  mutate(difmeanquarter_22_16 = y22mean_quarter - y16mean_quarter,
         difmedianquarter_22_16 = y22median_quarter - y16median_quarter)

hist(dif$difmean_22_16) #great! variation. ONE route only experiencing a decline route-wide.
hist(dif$difmedian_22_16) #median experiences no changes 
hist(dif_quarter$difmeanquarter_22_16) #more variation in the quarter routes. Some decline, most stay around 0, a few increase
hist(dif_quarter$difmedianquarter_22_16) #and like above, much less variation in median. I think mean is the way to go because it's capturing more of the distribution of values. 

landfire_raw <- landfire_raw %>% 
  left_join(dif_quarter, by = c("County_Route", "routequarter")) %>%
  left_join(dif, by = c("County_Route"))
#Q: how much a difference does it cause in analysis for this to be something stable across years, vs something that changes year by year. There's interpolation, and maybe we can back calculate degree change from the differences we see from 22-16, like get the rate of change and project that backwards, but eh. I'm not sure about that, it's making up a lot of data. I guess we have the minimum bound of knowing what category the trees were in in 2001. 


landfire_quarter <- landfire_raw %>%
  group_by(County_Route, raw_landtype, year, routequarter) %>%
  distinct(County_Route, raw_landtype, year, routequarter, .keep_all=TRUE) %>%
  ungroup() %>%
  select(-geometry) %>%
  #add lf prefix to columns
  rename_with(~ paste0("lf_", .), numpix:difmedian_22_16)

landfire_route <- landfire_raw %>%
  group_by(County_Route, raw_landtype, year) %>%
  distinct(County_Route, raw_landtype, year, .keep_all=TRUE) %>%
  ungroup() %>%
  select(-geometry) %>%
  #add lf prefix to columns
  rename_with(~ paste0("lf_", .), numpix:difmedian_22_16) 

#save csv
write.csv(landfire_route, "data/landfire_byroute.csv", row.names = FALSE)
write.csv(landfire_quarter, "data/landfire_byroutequarter.csv", row.names = FALSE)



plot(landfire_route$year, landfire_route$mean_route)
plot(landfire_quarter$year, landfire_quarter$mean_quarter)
hist(landfire_quarter$difmeanquarter_22_16)
  
  #graph of 1999 style median mean mode frequency - histogram, nojust a points plot actually pls
  
  ###clean up script below this point.
  
  
  # #group to route_level real quick
  # lf_group_route <- function(landfire) {
  #   landfire_route <- landfire %>%
  #     group_by(County_Route, converted_landtype, year) %>%
  #     mutate(frequency_route = sum(frequency_c),
  #            totpix_route = sum(numpix),
  #            percent = (frequency_route/totpix_route)*100) %>%
  #     ungroup() %>%
  #     distinct(County_Route, converted_landtype, year, .keep_all = TRUE) 
  
  
  
  
#add back in the route information
landfire_raw <- left_join(extracted, mbbs_buffers, by = "ID") %>%
  #get the total number of pixels on each route_stop
  group_by(ID) %>%
  mutate(numpix = n()) %>%
  ungroup() %>%
  #get the frequency of each raw_landtype
  pivot_longer(cols = landfire_2001evh:landfire_2022evh, names_to = "year", names_prefix = "landfire_", values_to = "raw_landtype") %>% #pivot
  mutate(year = as.numeric(str_extract(year, "[0-9]+"))) %>% #make year a number
  group_by(County_Route_Stop, year, raw_landtype) %>%
  mutate(frequency = n()) %>%
  ungroup() %>%
  #convert the raw land cover types to match the earlier kinds
  mutate(converted_landtype = case_when(
    year > 2014 & 
      raw_landtype > 99 & raw_landtype < 105 ~ 108,
    year > 2014 & 
      raw_landtype > 104 & raw_landtype < 111 ~ 109,
    year > 2014 & 
      raw_landtype > 110 & raw_landtype < 126 ~ 110,
    year > 2014 & 
      raw_landtype > 125 & raw_landtype < 151 ~ 111,
    year > 2014 & 
      raw_landtype > 150 & raw_landtype < 200 ~ 112,
    TRUE ~ raw_landtype
  )) %>%
  #add in route stop information
  mutate(routequarter = case_when(stop_num < 6 ~ 1,
                                  stop_num < 11 ~ 2,
                                  stop_num < 16 ~ 3,
                                  stop_num > 15 ~ 4),
         routehalf = case_when(routequarter < 3 ~ 1,
                               routequarter > 2 ~ 2)) %>% 
  ungroup()

landfire <- landfire_raw %>% distinct()

landfire_medians <- landfire_raw %>% 
  filter(raw_landtype > 100 & raw_landtype < 200) %>%
  group_by(County_Route, year, routequarter) %>%
  mutate(median = median(raw_landtype),
         q3 = quantile(raw_landtype, 0.75)) %>%
  distinct() %>%
  ungroup()


#Plot changes for all the landfire data by routequarter
cr <- unique(landfire_quarter$County_Route)
nlcdcode <- unique(landfire_quarter$converted_landtype)
q <- unique(landfire_quarter$routequarter)
#generate the pdf that we'll print to
pdf(file = "spatial/Route_landfire.pdf",
    width = 15)

for(i in 1:length(cr)) {
  
  crselect <- cr[i]
  par(mfrow = c(1,4))
  
  for(a in 1:length(q)) {
    
    qselect <- q[a]
    
    plot_df <- filter(landfire_quarter, County_Route == crselect, routequarter == qselect)
    nlcdclass <- unique(plot_df$)
    color = c("#476BA0", "#AA0000", "#B2ADA3", "#68AA63", "#1C6330", "#B5C98E",  "#CCBA7C", "#77AD93", "#DBD83D", "#AA7028",  "#BAD8EA")
    
    plot(plot_df[plot_df$converted_landtype == nlcdclass[1],]$year, 
         plot_df[plot_df$ijbg_class == nlcdclass[1],]$percent, 
         type = "b",
         col = color[1],
         xlim = c(2001, 2021),
         ylim = c(0, 100), 
         xlab = "Year",
         ylab = "Percent Landcover",
         main = crselect,
         xaxt = "n", 
         lwd = 2)
    axis(1, at=seq(2001,2021,1))
    legend("topleft", legend = unique(plot_df$ijbg_class), col = color, lty = 1, ncol = 2, lwd = 4)
    for(a in 2:length(nlcdclass)) {
      lines(plot_df[plot_df$ijbg_class == nlcdclass[a],]$year,  plot_df[plot_df$ijbg_class == nlcdclass[a],]$percent, type = "b", lwd = 2, col = color[a])
    }
  }
}

dev.off()



#create converted landfire database, getting the frequencies of the converted 
landfire_c <- landfire %>%
  group_by(ID, year, converted_landtype) %>%
  mutate(frequency_c = sum(frequency)) %>%
  arrange(ID, year, converted_landtype) %>%
  distinct(ID, year, converted_landtype, .keep_all = TRUE) %>%
  select(-raw_landtype, -frequency) %>%
  mutate(percent = frequency_c/numpix) 

#if I'm only interested in forest height categories
landfire_cfh <- landfire_c %>% filter(converted_landtype > 100 & converted_landtype < 200)

#group to route_level real quick
lf_group_route <- function(landfire) {
  landfire_route <- landfire %>%
    group_by(County_Route, converted_landtype, year) %>%
    mutate(frequency_route = sum(frequency_c),
           totpix_route = sum(numpix),
           percent = (frequency_route/totpix_route)*100) %>%
    ungroup() %>%
    distinct(County_Route, converted_landtype, year, .keep_all = TRUE) 
  
  return(landfire)
}
landfire_route <- lf_group_route(landfire_cfh)

#save csv
write.csv(landfire_route, "data/landfire_byroute.csv", row.names = FALSE)

#group to quarter route
landfire_quarter <- landfire_cfh %>%
  group_by(County_Route, routequarter, converted_landtype, year) %>%
  mutate(frequency_qroute = sum(frequency_c), 
          totpix_qroute = sum(numpix), 
          percent = (frequency_qroute/totpix_qroute)*100) %>%
  ungroup %>%
  distinct(County_Route, routequarter, converted_landtype, year, .keep_all = TRUE)

#let's just plot for one route_stop........

turn_to_route_level <- function(landfire, grouping_variables) {
  
}

#add in landfire classification information
  #this gets...complicated. early landfire 108-112 is information about tree heights. later landfire anything 101+ the part after the hundreds place is the m height of the trees. This changes so little, is essentially bins the tree categories into like, 2 segments. I Dont know that this is a valuable way to get tree heights. I think, the work is still work working to before ultimately coming to this conclusion. CONFIRM your thoughts before deciding offhand based on looking at the raw data and not clear summaries about how things change along routes. 
