#############################
#script generates testdata where the mean change is 6% and standard deviation is .005%. 
#just does 4 routes.
#############################


route_num <- c(rep(1, 12), rep(2,12), rep(3,12), rep(4,12))
year <- rep(2000:2011, 4)
common_name <- rep("Northern Goshawk", 48)



generate_percent_change <- 
  function(initial_value, n_to_generate, mean_change, standard_dev) {
  
  list <- initial_value
  std_devplusmin <- 2*rbinom(n=n_to_generate, size=1, prob=0.5)-1
  
  for(x in 1:(n_to_generate-1)) {
    
    initial_value <- 
      initial_value + 
      initial_value * (mean_change + (standard_dev * std_devplusmin[x]))
    
    list <- c(list,initial_value)
    
  }
  
  list #return
}

count <- c(generate_percent_change(5, 12, .06, .005),
           generate_percent_change(5, 12, .06, .005),
           generate_percent_change(5, 12, .06, .005),
           generate_percent_change(5, 12, .06, .005))

testdata <- data.frame(common_name, route_num, year, count)

write.csv(testdata, "C:/git/mbbs-analysis/test/testdata_six_percent_change.csv")  
  
  
