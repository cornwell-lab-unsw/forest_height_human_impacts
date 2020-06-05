# figure_intact_forests_aus.R 

library(maptools)
library(raster)
library(rgdal) 
library(ncdf4)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggmap)
library(rgeos)
library(readxl)
library(ggpubr)
library(ggExtra)

require(maps)
library(rnaturalearth)
library(rnaturalearthdata)

# Source files from /code 
source("code/data_processing.R")
source("code/init_functions.R")

# get data 
data <- init_dataframe() %>% 
  join_globcover_data() %>% 
  join_ifl_2013_data() %>% 
  drop_na(area_type) %>% 
  filter(x < 165, 
         x > 105,
         y > -45, 
         y < -10, 
         globcover_numeric != 210)

area <- rgdal::readOGR("data/IFL_2013/ifl_2013.shp")
area.rendered <- fortify(area)

australia <- filter(area.rendered,
                    long > 105,
                    long < 160,
                    lat > -45, 
                    lat < -10)

  

# set up world map 
theme_set(theme_bw())
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  xlim(105, 165) +
  ylim(-45, -10) +
  geom_polygon(data = australia,
               aes(x = long, y = lat, group=group),
               color= 'dark green', size=.2) + 
  geom_point(data = data, aes(x=x, y=y, colour=Intact_Forest_2013), size = 0.01, shape = 20) +
  scale_colour_manual(name="Forest Status", values=setNames(c('yellow','red'), c('Intact Forest','Non-Intact Forest')))
  

ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  geom_point(data = data, aes(x=x, y=y, colour=Intact_Forest_2013), size = 0.01) +
  scale_colour_manual(name="Forest Status", values=setNames(c('dark green','red'), c('Intact Forest','Non-Intact Forest'))) +
  xlim(105, 165) + ylim(-45, -10)
