# figure_filter_scenarios.R

# Packages
library(maptools)
library(raster)
library(rgdal) 
library(ncdf4)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)

require(maps)
library(rnaturalearth)
library(rnaturalearthdata)

# Source files from /code 
source("code/data_processing.R")
source("code/init_functions.R")
source("code/custom_colour_palettes.R")
source("code/multiplot.R")

data <- init_dataframe() %>% 
  join_height_data() %>% 
  join_globcover_data() %>% 
  join_precip_data() %>% 
  join_tree_cover_data()

# Globcover filters 
filter_1 <- c(14, 20, 30, 40, 50, 60, 70, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 220)
filter_2 <- c(40, 50, 60, 70, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 220)
filter_3 <- c(40, 50, 60, 70, 90, 100, 110, 120, 130, 140, 150)

data.scenario.1 <- data[data$globcover_numeric %in% filter_1 ,]
data.scenario.2 <- data[data$globcover_numeric %in% filter_2 ,]
data.scenario.3 <- data[data$globcover_numeric %in% filter_3 ,]


legend <- readxl::read_xls("../data/Globcover/Globcover_V2.2_Global/Globcover_Legend.xls")
# legend$Label[data$Value %in% setdiff(include_2, include_3)] %>% paste(collapse='; ')

# initial set up for plots 
theme_set(theme_bw())
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

palette <- colorRamps::matlab.like(10)
p <- custom_colour_scale()

# first set of globcover filters 
p1 <- ggplot(data.scenario.1, aes(x=mean_annual_precipitation, y=canopy_height) ) +
  geom_bin2d(bins=50) +
  scale_fill_distiller(trans="log", palette= "Spectral", direction=-1, aesthetics = "fill")  +
  xlim(0, 3500) +   
  ylim(0, 50) + 
  ggtitle("Figure 1b) filter_1")

p2 <- ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  xlim(-90,-40) +
  ylim(-60,10) +
  geom_point(data=data.scenario.1[data.scenario.1$canopy_height > 25 ,], aes(x = x, y = y), size=0.01, shape=15) +
  ggtitle("Global distribution of giant forests (> 25m) filter_1")

data.scenario.1$mean_annual_precipitation[data.scenario.1$mean_annual_precipitation > 4000] <- 4000
p3 <- ggplot() +
  geom_point(data = data.scenario.1, 
             aes(x = percentage_tree_cover, y = canopy_height, color = mean_annual_precipitation),
             size = 0.01)  +
  scale_colour_gradientn(colours = p) +
  ylim(0, 60) +
  xlab("Tree cover (%)") +
  ylab("Canopy height (m)") +
  ggtitle("Figure 3b) filter_1")

# second set of filters
p4 <- ggplot(data.scenario.2, aes(x=mean_annual_precipitation, y=canopy_height) ) +
  geom_bin2d(bins=50) +
  scale_fill_distiller(trans="log", palette= "Spectral", direction=-1, aesthetics = "fill")  +
  xlim(0, 3500) +   
  ylim(0, 50) + 
  ggtitle("Figure 1b) filter_2")

p5 <- ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  xlim(-90,-40) +
  ylim(-60,10) +
  geom_point(data=data.scenario.2[data.scenario.2$canopy_height > 25 ,], aes(x = x, y = y), size=0.01, shape=15) +
  ggtitle("Global distribution of giant forests (> 25m) filter_2")

data.scenario.2$mean_annual_precipitation[data.scenario.2$mean_annual_precipitation > 4000] <- 4000
p6 <- ggplot() +
  geom_point(data = data.scenario.2, 
             aes(x = percentage_tree_cover, y = canopy_height, color = mean_annual_precipitation),
             size = 0.01)  +
  scale_colour_gradientn(colours = p) +
  ylim(0, 60) +
  xlab("Tree cover (%)") +
  ylab("Canopy height (m)") +
  ggtitle("Figure 3b) filter_2")

# third set of filters
p7 <- ggplot(data.scenario.3, aes(x=mean_annual_precipitation, y=canopy_height) ) +
  geom_bin2d(bins=50) +
  scale_fill_distiller(trans="log", palette= "Spectral", direction=-1, aesthetics = "fill")  +
  xlim(0, 3500) +   
  ylim(0, 50) + 
  ggtitle("Figure 1b) filter_3")

p8 <- ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  xlim(-90,-40) +
  ylim(-60,10) +
  geom_point(data=data.scenario.3[data.scenario.3$canopy_height > 25 ,], aes(x = x, y = y), size=0.01, shape=15) +
  ggtitle("Global distribution of giant forests (> 25m) filter_3")

data.scenario.3$mean_annual_precipitation[data.scenario.3$mean_annual_precipitation > 4000] <- 4000
p9 <- ggplot() +
  geom_point(data = data.scenario.3, 
             aes(x = percentage_tree_cover, y = canopy_height, color = mean_annual_precipitation),
             size = 0.01)  +
  scale_colour_gradientn(colours = p) +
  ylim(0, 60) +
  xlab("Tree cover (%)") +
  ylab("Canopy height (m)") +
  ggtitle("Figure 3b) filter_3")

multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, cols=3)



