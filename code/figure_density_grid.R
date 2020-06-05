# IFL_script.R 
# intact forest layer test 

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

globcover.legend <- read_xls("../data/Globcover/Globcover_V2.2_Global/Globcover_Legend.xls")

data <- init_dataframe() %>% 
  join_height_data() %>% 
  join_precip_data() %>% 
  join_globcover_data() %>% 
  join_ifl_2013_data() %>% 
  filter( canopy_height <= 50, 
         mean_annual_precipitation <= 3500)

data <- left_join(data,
                  select(globcover.legend, Value, Label), 
                  by = c("area_type" = "Value"), 
                  all.x = TRUE) 

data <- data[, !names(data) %in% c("area_type")]

colour.pallete <- mutate(globcover.legend,
                         HEX = rgb(Red, Green, Blue, max=255)) %>% 
  select(Label, HEX)

test <- palette(brewer.pal(n = 11, name = "Spectral"))
palette <- colorRamps::matlab.like(10)

temp <- init_dataframe() %>% 
  join_ifl_2013_data()

ctobe.data <- filter(data, Label == "Closed to open (>15%) broadleaved evergreen or semi-deciduous forest (>5m)")

ggplot(data) +
  ggtitle("Tree Height/Climate Scatter Density of Intact Forests")+
  geom_point(aes(x=mean_annual_precipitation, y=canopy_height, colour=Label), size = 0.02, alpha=0.1)  +
  scale_colour_manual(name="globcover_value", 
                      values=setNames(unlist(colour.pallete$HEX), unlist(colour.pallete$Label))) +
  xlim(0, 3500) +
  ylim(0, 50) 

ggplot(data) + 
  ggtitle("Grid") +
  geom_point(aes(x=mean_annual_precipitation, y=canopy_height), size = 0.02) + 
  xlim(0, 3500) +
  ylim(0, 50) +
  facet_grid(cols = vars(Intact_Forest_2013), rows = vars(Label))

ggplot(data) + 
  ggtitle("Grid") +
  geom_bin2d(aes(x=mean_annual_precipitation, y=canopy_height), bins=50) + 
  scale_fill_distiller(trans="log", palette= "Spectral", direction=-1, aesthetics = "fill") +
  xlim(0, 3500) +
  ylim(0, 50) +
  facet_grid(cols = vars(Intact_Forest_2013), rows = vars(Label)) +
  theme(strip.text.y = element_text(angle = 0))



ggplot(ctobe.data) + 
  ggtitle("Grid") +
  geom_bin2d(aes(x=mean_annual_precipitation, y=canopy_height), bins=50) + 
  scale_fill_distiller(trans="log", palette= "Spectral", direction=-1, aesthetics = "fill") +
  xlim(0, 3500) +
  ylim(0, 50) +
  facet_grid(cols = vars(Intact_Forest_2013))  
  #theme(strip.text.y = element_text(angle = 0))

ggplot(ctobe.data) + 
  ggtitle("Grid") +
  geom_bin2d(aes(x=mean_annual_precipitation, y=canopy_height), bins=50) + 
  scale_fill_distiller(palette= "Spectral", direction=-1, aesthetics = "fill") +
  xlim(0, 3500) +
  ylim(0, 50) +
  ggMarginal(p, type = "density")


p <- ggplot(ctobe.data) + 
  ggtitle("Distribution of Closed to open (>15%) broadleaved evergreen or semi-deciduous forest (>5m)") +
  geom_bin2d(aes(x=mean_annual_precipitation, y=canopy_height), bins=50) +
  scale_fill_distiller(palette= "Spectral", direction=-1, aesthetics = "fill")+
  geom_point(aes(x = mean_annual_precipitation, y=canopy_height), size = 0.01, alpha = 0.01)+
  xlim(0, 3500) +
  ylim(0, 50)

ggMarginal(p, type = "density")


