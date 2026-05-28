##################################
#
# Graphs and supplementary table creation for
# the MBBS data paper
#
#
####################################
library(tidyr)
library(dplyr)
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


##############################
#
# Characterize Observer Turnover
#
#
##############################
library(dplyr)
library(scales)
library(stringr)
surveys <- read.csv("data/mbbs/surveys.csv") |>
  select(route, year, standardized_observers) |>
  #assign a unique number to each unique observer within a route
  #NA observers get value maximum_n_colors + 1
  group_by(route) |>
  mutate(
    # Create a temporary column to force 1999 to the top
    year_order = case_when(
      str_detect(route, "cthm") & year == 2000 ~ 0,
      str_detect(route, "cthm") & year != 2000 ~ 1,
      str_detect(route, "orng") & year == 1999 ~ 0,
      str_detect(route, "orng") & year != 1999 ~ 1,
      str_detect(route, "drhm") ~ ifelse(year == 2002, 0, 1)
  )) |>
  arrange(route, year_order, year, .by_group = TRUE) |>
  # Now handle observer ordering: observers from 1999 first, then consecutive observers
  mutate(
    # Identify which observer is first
    first_observer = first(standardized_observers),
    # Create ordering for observers: 1999 observers first, then others
    observer_order = ifelse(standardized_observers == first_observer, 0, 1)
  ) |>
  arrange(route, year_order, observer_order, standardized_observers, .by_group = TRUE) |>
  #okay! yay! first observer is now always first.
  mutate(observer_id = consecutive_id(standardized_observers),
         route_max_turnover = max(observer_id)) |>
  ungroup() |>
  #re-arrange back to normal
  arrange(route, year) |>
  arrange(desc(route)) |> #so they display with chatham 1 up top
  #hell yeah
  select(-first_observer) |> #too confusing if it stays in
  #hm! Steven suggests arranging by frequency of observer turnover.
  arrange(desc(route_max_turnover), route, year) |>
  mutate(route = paste0(route," (", route_max_turnover, ")")) |>
  mutate(route = case_when(route == "orng-12 (1)" ~ "orng-12 (1 turnover)", 
                           TRUE ~ route))

#how many possible observer changes are there?
n_obs_changes <- surveys |> group_by(route) |> summarize(n = n_distinct(standardized_observers))
#up to 11 unique observers across a route. Okay, instead of assigning each observer their own unique code (gets very visually messy with 113 unique observers, just about impossible to tell some of the observer changes apart), let's assign a new code every time an observer changes.
maximum_n_colors <- max(n_obs_changes$n)
#11 max colors
palette <- palette(hcl.colors(maximum_n_colors, "viridis"))
palette[1] <- "black"
palette[maximum_n_colors+1] <- "#edf8b1"
palette <- c( 
  "#e0f3db",
  "#ccece6",
  "#7bccc4",
  "#66c2a4",
  "#4eb3d3",
  "#ccebc5",
  "#c6dbef",
  "#a8ddb5",
  "#2b8cbe",
  "#0868ac",
  "#084081",
  "white" #NA values
)
rainbow(11)
  

          #palette <- rainbow((length(unique(surveys$standardized_observers))))
          #palette <- palette(hcl.colors(113, "viridis"))
          #palette <- hue_pal()(113)
          #palette <- palette.colors(113)
          #palette[114] <- "#edf8b1"


  
#assign each observer a unique color
#observer_color_map <- data.frame(
#  standardized_observers = unique#(surveys$standardized_observers), 
#  color = rainbow(length(unique(surveys$standardized_observers)))
#  )



#surveys <- surveys |>
# left_join(observer_color_map, by = c("standardized_observers"))

matrix <- surveys %>% 
  mutate(value = observer_id) %>% 
  select(route, year, value) %>% 
  pivot_wider(names_from = year, values_from = value) %>%
  relocate(`1999`) %>%
  relocate(route) 

matrix[is.na(matrix)] <- maximum_n_colors+1 #for missing values

matrix_final <- as.matrix(matrix[,2:28])
rownames(matrix_final) <- matrix$route

png(filename = "scratch_observermatrix.png", 
    width = 540,
    height = 530,
    units = "px", 
    type = "windows")
heatmap(as.matrix(matrix_final), 
        Colv = NA, 
        Rowv = NA, 
        scale = "none", #map the original values directly
        #col = topo.colors(3)
        col = palette,
        #ylab = "Route",
        cexRow = 1.1,
        cexCol = 1.1,
        add.expr = abline(h=.5:34.5, col = "#2c7fb8")
)
title(ylab="Route", line=-33.5, cex.lab=1.2)
dev.off()

png(filename = "scratch_observerlegend.png", 
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

#quantify frequency of multi-observer surveys
quant <- surveys |>
  mutate(mult_obs = str_detect(standardized_observers, ","))
table(quant$mult_obs)
sum(quant$mult_obs == TRUE) / nrow(quant)
