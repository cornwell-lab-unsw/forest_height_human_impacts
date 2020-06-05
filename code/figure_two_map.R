# figure_two_map.R
# map replication 

library(raster)
library(rgdal)
library(ggplot2)
library(dplyr)
library(sf)
library(tidyverse)
require(maps)
library(rnaturalearth)
library(rnaturalearthdata)

source("code/init_functions.R")
source("code/data_processing.R")

coordinates <- get_precip_height_df() %>% 
  filter(canopy_height > 25) %>% 
  select(x, y)

precip.df <- get_area_precip_df() %>% 
  filter(area_type != 210)

theme_set(theme_bw())

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  geom_point(data=precip.df[precip.df$mean_annual_precipitation >= 1000 &
                              precip.df$mean_annual_precipitation < 1500 ,], 
             aes(x = x, y = y), 
             size=0.01, 
             shape=15, 
             color="yellow") +
  geom_point(data=precip.df[precip.df$mean_annual_precipitation >= 1500 ,], 
             aes(x = x, y = y), 
             size=0.01, 
             shape=15, 
             color="orange") +
  geom_point(data=coordinates, aes(x = x, y = y), size=0.005, shape=15) +
  ylim(-60, 90) +
  ggtitle("Global distribution of giant forests")


                 