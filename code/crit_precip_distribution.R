# crit_precip_distribution.R

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

data <- get_precip_height_df() %>% 
  filter(mean_annual_precipitation >= 1500,
         mean_annual_precipitation <= 2000)

theme_set(theme_bw())

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  geom_point(data=data[data$canopy_height >= 0 &
                              data$canopy_height < 23 ,], 
             aes(x = x, y = y), 
             size=0.01, 
             shape=15, 
             color="yellow") +
  geom_point(data=data[data$canopy_height >= 23 &
                         data$canopy_height < 31 ,], 
             aes(x = x, y = y), 
             size=0.01, 
             shape=15, 
             color="orange") +
  geom_point(data=data[data$canopy_height >= 31 ,], 
             aes(x = x, y = y), 
             size=0.01, 
             shape=15, 
             color="red") +
  ylim(-60, 90) +
  ggtitle("Global forest height distribution at MAP:1500-2000mm")

ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  geom_point(data=data, 
             aes(x = x, y = y, colour=cut(data$canopy_height, c(0,23,31,50))), 
             size=0.01, 
             shape=15) +
  scale_colour_manual(name="qsec",
                      values=c("(0,23]"="yellow",
                               "(23,31]"="orange",
                               "(31,Inf]"),
                      labels = c("0-23m", "23-31m", ">31m")) +
  ylim(-60, 90) +
  ggtitle("Global forest height distribution at MAP:1500-2000mm")

