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
  mutate(numpix = n()) %>%
  ungroup() %>%
  #get the frequency of each raw_landtype by stop
  pivot_longer(cols = landfire_2001evh:landfire_2022evh, names_to = "year", names_prefix = "landfire_", values_to = "raw_landtype") %>% #pivot
  mutate(year = as.numeric(str_extract(year, "[0-9]+"))) %>% #make year a number
  group_by(County_Route_Stop, year, raw_landtype) %>%
  mutate(frequency = n()) %>%
  ungroup() %>%
  #add in route stop information
  mutate(routequarter = case_when(stop_num < 6 ~ 1,
                                  stop_num < 11 ~ 2,
                                  stop_num < 16 ~ 3,
                                  stop_num > 15 ~ 4),
         routehalf = case_when(routequarter < 3 ~ 1,
                               routequarter > 2 ~ 2)) %>% 
  #convert landtypes to 2001 style
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
  arrange(ID, year, converted_landtype) %>%
  #plenty of landtypes that we don't care about. Let's filter them out. For now, we can do more than just trees and include shub and herb vegetation in this. If you want to change this later, filter to landtype > 100 and < 200
  filter(raw_landtype > 100) %>% 
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
         percent_route = (frequency_route/totpix_route)*100,
         median_route = median(raw_landtype),
         mean_route = mean(raw_landtype),
         q3_route = quantile(raw_landtype, 0.75)) %>%
  ungroup() %>%
  #add in quarter route summaries
  group_by(County_Route, year, converted_landtype, routequarter) %>%
  mutate(frequency_quarter = sum(frequency),
         percent_quarter = (frequency_quarter/totpix_quarter)*100,
         median_quarter = median(raw_landtype),
         mean_quarter = mean(raw_landtype),
         q3_quarter = quantile(raw_landtype, 0.75)) %>%
  ungroup() %>%
  #next step is to get the 2022-2014 differences
  
  
  
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







#write csv of extracted data
write.csv(extractedbuffers_terra, "/proj/hurlbertlab/ijbgoulden/extractedbuffers.csv")