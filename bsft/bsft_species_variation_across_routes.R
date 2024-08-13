#for getting a trend estimate by route for each species, and then mapping that variation on the buffers
#first, go ahead and run species-trend-estiamtes-allmodels to get the mbbs data.
library(sf)
library(stringr)
#going to need a table of the route information


#for now, let's not worry about mapping along routes. What we want is just the 1st stop
mbbs_buffers <- read.csv("spatial/RouteStops.csv") %>%
  dplyr::select(-notes) %>%
  dplyr::filter(stop_num == 1) %>%
  #get county
  dplyr::mutate(mbbs_county = case_when(
    str_starts(County_Route, "Chatham") ~ "chatham",
    str_starts(County_Route, "Durham") ~ "durham",
    str_starts(County_Route, "Orange") ~ "orange"
  )) %>%
  #get route_num
  #dplyr::mutate(route_num = as.numeric(substr(routes, nchar(routes)-1, nchar(routes)))) %>%
  #recreate route_ID
  dplyr::mutate(route_ID = route_num + case_when(
    mbbs_county == "orange" ~ 100L,
    mbbs_county == "durham" ~ 200L,
    mbbs_county == "chatham" ~ 300L))
  #okay, done! now we can start adding species information to this.
#actually, we do need to convert to a sf.
sf <- st_as_sf(mbbs_buffers, coords = c('lon', 'lat'), crs = 4269) %>%
  mutate(st_coordinates(geometry))

sf$`st_coordinates(geometry)`[,1]

#now for the counties
nc <- st_read("spatial/shapefiles/NC_County_Polygons/North_Carolina_State_and_County_Boundary_Polygons.shp")

#filter to counties we're interested in
nc <- nc %>% filter(County %in% c("Chatham", "Orange", "Durham")) %>%
  st_transform(crs = 4269) #change crs


#take the mbbs, filter to a route, and calculate the route-level trend for each species. Then, add that to the mbbs_buffers datatable so that we can easily graph based on each species. 
species_list <- unique(mbbs$common_name)
filtered_mbbs <- mbbs %>% filter(common_name == species_list[1] & route_ID == mbbs_buffers$route_ID[1]) %>%
  arrange(route_ID, year, common_name)
model <- NA
i=1
results <- geeglm(count ~ year,
                  family = poisson,
                  id = route_ID,
                  data = filtered_mbbs,
                  corstr = "ar1") %>%
  pivot_tidied(species_list[i]) %>%
  filter(value == "year") %>%
  mutate(route_ID = 999)


for(i in 1:length(species_list)) {
  print(paste("started sp", species_list[i]))  #filter mbbs
  filtered_mbbs <- mbbs %>%
    filter(common_name == species_list[i])  
  
  for(a in 1:length(unique(filtered_mbbs$route_ID))){ #and within each route that species has data for.    #filter mbbs again
      filtered_mbbs_temp <- filtered_mbbs %>%
        filter(route_ID == unique(filtered_mbbs$route_ID)[a])    
      
      #model - this is a gee with just one route in the grouping variable
      #okay. this is obviously causing problems. What if I just didn't use a geeglm as a model, and, because I only have the data for one route and am functionally fitting it with just one grouping variable, this could just be a glm. I think that's the better way to run this.
      model <- glm(count~year, family = poisson, data = filtered_mbbs_temp)    
      model <- pivot_tidied(model, species_list[i]) %>%
        filter(value == "year")    
      model$route_ID <- unique(filtered_mbbs$route_ID)[a]
      results <- rbind(results, model)
      print(paste("routed",a))  
      }
  print("species'd")
}

results <- results %>% 
  filter(route_ID %in% 100:400) %>% #remove test
  #shift estimate to a trend
  mutate(trend = exp(estimate)-1) %>%
  #have an acadian flycatcher that needs removing, maybe just one seen on route
  filter(trend < 20)
results$route_ID <- as.double(results$route_ID)


#save df
write.csv(results, "scratch/bsft_species_variation_across_routes.csv", row.names = FALSE)

#read df is needed
results <- read.csv("scratch/bsft_species_variation_across_routes.csv", header = TRUE)


#reconnect route information
results_buffers <- left_join(results, mbbs_buffers, by = "route_ID") %>%
  #connect polygons
  left_join(sf)


pdf(file = "scratch/bsft_species_variation_acorss_routes.pdf")


#for(i in 1:length(unique(results_buffers$common_name))){
#temp <- results_buffers %>% filter(common_name == unique(results_buffers$common_name)[i])
#REVI
temp <- results_buffers %>% filter(common_name == "Red-eyed Vireo")
minestimate <- min(temp$estimate)
maxestimate <- max(temp$estimate)
#if maxestimate is less than 0, we should add a positive number to the scale
if(maxestimate < 0) {
  maxestimate <- .1 #a ten percent increase per year
}
temp$estimate.std = round(30*(temp$estimate-minestimate)/(maxestimate - minestimate),0) +1

plot(nc$geometry,
     main = unique(temp$common_name),
     col = "NA")
plot(first_layer)
points(x = results_buffers$`st_coordinates(geometry)`[,1],  
       y= results_buffers$`st_coordinates(geometry)`[,2],
       col = colorramp(30)[temp$estimate.std],
       pch = 16,
       cex = 2.5)
legend("topright", legend = seq(round(maxestimate*100,3), round(minestimate*100,2), length.out = 2), fill = c("#0000FF","#FF0000"))#}

dev.off()





#we want to standardize the estimates
minestimate <- min(results_buffers$estimate)
maxestimate <- max(results_buffers$estimate)
results_buffers$estimate.std = round(50*(results$estimate-minestimate)/(maxestimate - minestimate),0) +1

plot(check$geometry,
     col = colorramp(50)[check$estimate.std]) 
plot(nc$geometry)


#do the save to pdf thing to look at species variation
pdf(file = "scratch/bsft_species_variation_acorss_routes.pdf")

for(i in 1:length(unique(results_buffers$common_name.x))) {
  
  check <- results_buffers %>% filter(common_name.x == unique(results_buffers$common_name.x)[i])

  hist(check$trend.x,
       main = unique(check$common_name.x))
  
  #plot(check$geometry,
  #   col = colors[ rank ],
  #   main = unique(check$common_name.x))
  
}

dev.off()











