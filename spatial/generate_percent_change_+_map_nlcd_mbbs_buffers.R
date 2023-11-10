##This file takes the csv of nlcd values extracted from the mbbs buffers and uses it to get % change of landcover times for routes and stops, as well as map the change over all years for each of the routes

library(dplyr)
library(tidyr)

#read in the raw terra:extracted table
extractedbuffers <- read.csv("spatial/nlcd_extracted_buffers.csv", header = TRUE) %>%
  rename(nlcd2001 = NLCD.Land.Cover.Class.1,
         nlcd2004 = NLCD.Land.Cover.Class.2,
         nlcd2006 = NLCD.Land.Cover.Class.3,
         nlcd2008 = NLCD.Land.Cover.Class.4,
         nlcd2011 = NLCD.Land.Cover.Class.5,
         nlcd2013 = NLCD.Land.Cover.Class.6,
         nlcd2016 = NLCD.Land.Cover.Class.7,
         nlcd2019 = NLCD.Land.Cover.Class.8,
         nlcd2021 = NLCD.Land.Cover.Class.9) %>%
  na.omit() %>%
  select(-X)

landtype_bybuff <- as.data.frame(table(extractedbuffers[,1:2])) %>% rename(freq2001 = Freq, nlcd = nlcd2001) 

#loop to make a table for the other years one by one, and then add them to the landtype_bybuff dataset
for(i in 3:length(extractedbuffers)) {
  cols <- c(1, i) 
  year <- c("2001", "2004", "2006", "2008", "2011", "2013", "2016", "2019", "2021")
  colname <- paste("nlcd", year[i-1], sep = "")
  
  add <- extractedbuffers[,cols]
  add <- as.data.frame(table(add)) 
  colnames(add)[3] <- paste("freq", year[i-1], sep = "")
  landtype_bybuff <- left_join(landtype_bybuff, add, by = c("ID", "nlcd" = colname))
}

#ID comes from the order the stops are listed in RouteStops
#let's merge in the RouteStops and the nlcd classifications
RouteStops <- read.csv("spatial/RouteStops.csv", header = TRUE) %>%   
              mutate(ID = row_number(),
                     ID = as.factor(ID)) %>%
              relocate(ID, .before = County_Route_Stop) %>%
              select(-notes)
nlcd_classif <- read.csv("spatial/nlcd_classifications.csv", header = TRUE) %>% mutate(code = as.factor(code))

#merge these with landtype_bybuff to make it way more interpretable
landtype_bybuff <- left_join(RouteStops, landtype_bybuff, by = "ID")
#add in the nlcd classification information
landtype_bybuff <- left_join(landtype_bybuff, nlcd_classif, by = c("nlcd" = "code")) %>% relocate(class:description, .after = nlcd)


#right! beautiful. Let's turn those frequencies into percents 
#landtype_bybuff %>% group_by(ID) %>% summarize(sum(freq2001), sum(freq2021)) #yup, just double-confirming, same number of pixel from 2001 to 2021, we can use whichever to divide
numpixels <- landtype_bybuff %>%
             group_by(ID) %>%
             summarize(sum(freq2001)) #number of pixels in each buffer
numpixels <- numpixels %>%
             mutate(numpix = numpixels$`sum(freq2001)`) %>% #renaming column
             select(ID, numpix) 
#number of pixels varries from 550 to 565, depends on how the exact edges of the 400m boundaries lined up with the nlcd 30x30m pixels

#now here I can get the percent of each 400m buffer that's a given nlcd class
landtype_bybuff <- landtype_bybuff %>% left_join(numpixels, by = "ID") %>%
                    mutate(perc2001 = freq2001/numpix, 
                           perc2004 = freq2004/numpix,
                           perc2006 = freq2006/numpix,
                           perc2008 = freq2008/numpix,
                           perc2011 = freq2011/numpix,
                           perc2013 = freq2013/numpix,
                           perc2016 = freq2016/numpix,
                           perc2019 = freq2019/numpix,
                           perc2021 = freq2021/numpix)

#now, if we want to look at across routes instead of buffers
landtype_byroute <- landtype_bybuff %>% 
                    group_by(County_Route, nlcd) %>%
                    transmute(freq2001 = sum(freq2001),
                              freq2004 = sum(freq2004),
                              freq2006 = sum(freq2006),
                              freq2008 = sum(freq2008),
                              freq2011 = sum(freq2011),
                              freq2013 = sum(freq2013),
                              freq2016 = sum(freq2016),
                              freq2019 = sum(freq2019),
                              freq2021 = sum(freq2021),
                              totpix = sum(numpix)) %>% 
                    distinct() %>% #this gives 510 rows, this is fine b/c land types 12,51,72,73,and 74 are all Alaska only land cover types and are removed for being 0s
    #table(landtype_byroute$County_Route, landtype_byroute$nlcd)
    #https://www.mrlc.gov/data/legends/national-land-cover-database-class-legend-and-description
                    left_join(nlcd_classif, by = c("nlcd" = "code")) %>% #add back in nlcd information
                    pivot_longer(cols = freq2001:freq2021, names_to = "year", names_prefix = "freq", values_to = "frequency") %>% #turn this into a format we can use when plotting
                    mutate(percent = (frequency/totpix) * 100,
                           year = as.integer(year)) %>%
                    relocate(totpix, .after = frequency)

#check percents add up to 100
t <- landtype_byroute %>% filter(year == "2001", County_Route == "Chatham-01") 
sum(t$percent) #yep, sums to 100



#okay..plotting?
cr <- unique(landtype_byroute$County_Route)
nlcdcode <- unique(landtype_byroute$nlcd)
#generate the pdf that we'll print to
pdf(file = "spatial/Route_nlcdcode.pdf")

for(i in 1:length(cr)) {
  
  crselect <- cr[i]
  plot_df <- filter(landtype_byroute, County_Route == crselect)
  color = unique(plot_df$color)

plot(plot_df[plot_df$nlcd == nlcdcode[1],]$year, 
     plot_df[plot_df$nlcd == nlcdcode[1],]$percent, 
     type = "b",
     col = color,
     xlim = c(2001, 2021),
     ylim = c(0, 100), 
     xlab = "Year",
     ylab = "Percent Landcover",
     main = crselect,
     xaxt = "n", 
     lwd = 2)
axis(1, at=seq(2001,2021,1))
legend("topleft", legend = unique(plot_df$description), col = unique(plot_df$color), lty = 1, ncol = 2, lwd = 4)
for(a in 2:15) {
lines(plot_df[plot_df$nlcd == nlcdcode[a],]$year,  plot_df[plot_df$nlcd == nlcdcode[a],]$percent, type = "b", lwd = 2, col = unique(plot_df[plot_df$nlcd == nlcdcode[a],]$color))
}
}

dev.off()


#ok. we need to collapse the nlcd codes..and do that again.
landtype_byroutenclass <- landtype_byroute %>% group_by(County_Route, year, class) %>%
                          transmute(percent = sum(percent)) %>% distinct()
#need to sum based on class. We can add percents here that's fine
pdf(file = "spatial/Route_nlcdclass.pdf")
for(i in 1:length(cr)) {
  
  crselect <- cr[i]
  plot_df <- filter(landtype_byroutenclass, County_Route == crselect)
  nlcdclass <- unique(plot_df$class)
  color = c("#476BA0", "#AA0000", "#B2ADA3", "#68AA63", "#A58C30", "#E2E2C1", "#DBD83D", "#BAD8EA")
  
  plot(plot_df[plot_df$class == nlcdclass[1],]$year, 
       plot_df[plot_df$class == nlcdclass[1],]$percent, 
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
  legend("topleft", legend = unique(plot_df$class), col = color, lty = 1, ncol = 2, lwd = 4)
  for(a in 2:length(nlcdclass)) {
    lines(plot_df[plot_df$class == nlcdclass[a],]$year,  plot_df[plot_df$class == nlcdclass[a],]$percent, type = "b", lwd = 2, col = color[a])
  }
}
dev.off()