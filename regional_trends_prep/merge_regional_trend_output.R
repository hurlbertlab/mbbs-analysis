#--------------------
# Merge together all the csvs output from the regional trend on longleaf
#--------------------

library(dplyr)
library(stringr)

files <- list.files("regional_trends_prep/output/", pattern = "_trends.csv", full.names = TRUE)

regional_trend = data.frame()

for(i in 1:length(files)) {
  
  temp <- read.csv(files[i], header = TRUE) %>%
    filter(region == "BCR29") %>%
    mutate(
      file = files[i],
      common_name = str_extract(files[i], "(?<=/output/)[a-zA-Z\\s-]*(?=_tre)")) %>%
    select(-X)
  
  regional_trend <- rbind(regional_trend, temp)
  
}

write.csv(regional_trend, "regional_trends_prep/regional_trendWIP.csv", row.names = FALSE)
