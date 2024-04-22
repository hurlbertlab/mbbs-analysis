#for getting a trend estimate by route for each species, and then mapping that variation on the buffers
#first, go ahead and run species-trend-estiamtes-allmodels to get the mbbs data.
library(sf)
library(stringr)
#going to need a table of the route information
#let's load in the buffers

mbbs_buffers <- read.csv("spatial/RouteStops.csv") %>%
  dplyr::select(-notes)
#create 400m buffers from route stop points
#convert lat/lon to points geometry
mbbs_buffers <- st_as_sf(mbbs_buffers, coords = c('lon', 'lat'), crs = 4269) 
#transform to a meters based crs
mbbs_buffers <- st_transform(mbbs_buffers, crs = 5070) #meters, NC 
#buffer each point by 400m, now geometry is polygons
mbbs_buffers <- st_buffer(mbbs_buffers, dist = 400) 

#merge to routes
test <- mbbs_buffers %>%
  filter(County_Route == "Chatham-01") %>%
  st_union()
routes <- unique(mbbs_buffers$County_Route)
for(i in 1:length(routes)) {
  test[i] <- mbbs_buffers %>%
    filter(County_Route == routes[i]) %>%
    st_union()
}

#recreate a dataframe, now there's a spatial polygon for each individual route.
mbbs_buffers <- data.frame(routes,test) %>%
  #get county
  mutate(mbbs_county = case_when(
    str_starts(routes, "Chatham") ~ "chatham",
    str_starts(routes, "Durham") ~ "durham",
    str_starts(routes, "Orange") ~ "orange"
  )) %>%
  #get route_num
  mutate(route_num = as.numeric(substr(routes, nchar(routes)-1, nchar(routes)))) %>%
  #recreate route_ID
  mutate(route_ID = route_num + case_when(
    mbbs_county == "orange" ~ 100L,
    mbbs_county == "durham" ~ 200L,
    mbbs_county == "chatham" ~ 300L)) %>%
#okay, done! now we can start adding species information to this.

#take the mbbs, filter to a route, and calculate the route-level trend for each species. Then, add that to the mbbs_buffers datatable so that we can easily graph based on each species. 
species_list <- unique(mbbs$common_name)
filtered_mbbs <- mbbs %>% filter(common_name == species_list[1] & route_ID == mbbs_buffers$route_ID[1]) %>%
  arrange(route_ID, year, common_name)
model <- NA
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
results_buffers <- left_join(results, mbbs_buffers, by = "route_ID")

check <- results_buffers %>% filter(common_name.x == "Canada Goose")
#transform a numerican variable into bins for color
  library(paletteer)
  nColor <- 20
  colors = paletteer_c("viridis::inferno", n=nColor)
  rank <- as.factor(as.numeric(cut(results_buffers$estimate, nColor)))

  
plot(check$geometry,
     col = colors[ rank ],
     main = unique(check$common_name.x))

#okay, or plot using ggplot
ggplot(results_buffers, aes(x = geometry)) +
  geom_polygon(aes(fill = trend.x)) +
  scale_fill_gradient2(midpoint = 0, mid = "#eee8d5", low = "#dc322f", high = "#268bd2")

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











