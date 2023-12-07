#Post-2019 data has counts by stops, rather than counts by whole routes. This script generates a version of the dataset with all data equitably formatted by route. 

#TO DO: turn this into a function. One that both can take just a single mbbs_county (and use route_num) or takes an mbbs_all and uses a route_ID. Or just like, ask which column is the route sorting column

test <- mbbs %>% filter(common_name == "Carolina Wren")

test_change <- test %>% group_by(common_name, year, date, route_ID) %>% summarize(sum = sum(count))

test_change_unique <- test %>% 
  group_by(common_name, year, date, route_ID) %>% 
  mutate(count = sum(count)) %>% 
  distinct(common_name, year, date, route_ID, count, .keep_all = TRUE) #keep_all is used to retain all other columns in the dataset