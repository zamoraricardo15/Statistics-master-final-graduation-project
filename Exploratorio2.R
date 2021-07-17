library(maps)
library(ggplot2)
library(tidyverse)
library(dbplyr)
library(devtools)

find_rtools()
# NOT RUN {
install.Rtools() # installs the latest version of RTools (if one is needed)
install.Rtools(TRUE) # if one is needed - asks the user to choose the latest 
# version of RTools to install

install.Rtools(TRUE, FALSE) # asks the user to choose 
# the latest version of RTools to install 
# (regardless if one is needed)
# install.Rtools(F,F)

# }

devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)


ggplot() + geom_polygon(data = urbnmapr::states, mapping = aes(x = long, y = lat, group = group),
               fill = "grey", color = "white") + coord_map(projection = "albers", lat0 = 39, lat1 = 45)


# map_USA <- map_data("world")
# head(map_USA)


## Mapping county data
map_county_fips <- readRDS("gadm36_CRI_2_sp.rds")
head(map_county_fips)


df <- read.csv("01_08_CSV_POSITIVOS.csv", header = TRUE) 
head(df)



#df$NAME_2 <- df[,4]
map_county_fips$canton <- map_county_fips$NAME_2
map_county_fips$canton2 <- df$canton
map_county_fips$percent_diff <- df$X8.1.2021
head(map_county_fips)
#map_county_fips$canton

ggplot() +
  geom_polygon(data = map_county_fips, mapping = aes(x = long, y = lat, group = group)) + 
  coord_quickmap()

map_county_per_diff <- map_county_fips %>%
  inner_join(df, by = "canton")

ggplot(data = map_county_fips, mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = bbox(percent_diff)) + 
  coord_quickmap() + 
  labs(title = "Personas positivos de Covid-19 por cantonen Costa Rica al 8 de enero de 2020")

 ###################### 
  
  map_theme <- theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    panel.background = element_rect(fill = "white")
  )
  
  ggplot(data = map_county_fips, mapping = aes(x = long, y = lat, group = group)) +
   # geom_polygon(aes(fill = percent_diff)) + 
    scale_fill_gradient2(low = "blue", high = "red") +
    coord_quickmap() + 
    labs(title = "Election results by county") +
    map_theme
  
  
  #################
  map_state <- map_data("state")
  ggplot(data = map_county_per_diff, mapping = aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = percent_diff)) + 
    geom_polygon(data = map_state, fill = NA, color = "black") +
    scale_fill_gradient2(low = "blue", high = "red") +
    coord_quickmap() + 
    labs(title = "Election results by county") + 
    map_theme
