##################################
#
# Graphs and supplementary table creation for
# the MBBS data paper
#
#
####################################
library(tidyr)
devtools::load_all()
mbbs <- create_mbbs_data()
surveys <- mbbs$surveys %>% select(route, year) %>% mutate(route_level = TRUE)
ss <- mbbs$stop_surveys %>% group_by(year, route) %>% summarize(stop_level = TRUE)

allsurvs <- left_join(surveys, ss, by = c("route", "year")) %>%
  arrange(desc(route)) #so they display with chatham 1 up top

matrix <- allsurvs %>% 
  mutate(value = case_when(
    route_level & stop_level ~ 2,
    route_level ~ 1,
    TRUE ~ 3)) %>% 
  select(route, year, value) %>% 
  pivot_wider(names_from = year, values_from = value) %>%
  relocate(`1999`) %>%
  relocate(route) 

matrix[is.na(matrix)] <- 3

matrix_final <- as.matrix(matrix[,2:28])
rownames(matrix_final) <- matrix$route

#pick colors
#colors from: https://colorbrewer2.org/#type=sequential&scheme=YlGnBu&n=6
palette <- colorRampPalette(c("#41b6c4", "#2c7fb8", "#edf8b1")) #GreytoGreen
current.palette = palette(3)


png(filename = "scratch_routeyearmatrix.png", 
    width = 540,
    height = 530,
    units = "px", 
    type = "windows")
heatmap(as.matrix(matrix_final), 
        Colv = NA, 
        Rowv = NA, 
        scale = "none", #map the original values directly
        #col = topo.colors(3)
        col = current.palette,
        #ylab = "Route",
        cexRow = 1.1,
        cexCol = 1.1,
        add.expr = abline(h=.5:34.5, col = "#2c7fb8")
)
title(ylab="Route", line=-33.5, cex.lab=1.2)
#title(main = "Route x Year")

dev.off()

png(filename = "scratch_routeyearlegend.png", 
    width = 530,
    height = 530,
    units = "px", 
    type = "windows")
plot(NULL)
#going to need to fix where this legend plots but yeah :)
legend("topleft",
       title = "Data Availability", 
       legend = c("Stop Level", "Route Only", "No Survey"),
       fill = c("#2c7fb8", "#41b6c4", "#edf8b1"), 
       #col = c("#2c7fb8", "#41b6c4", "#edf8b1"),
       #pch = 15
)
dev.off()
