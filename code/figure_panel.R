# figure_panels.R

# Packages
library(maptools)
library(raster)
library(rgdal) 
library(ncdf4)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(ggExtra)
library(maptools)
library(magrittr)
library(sf)


# Source files from /code 
source("code/data_processing.R")
source("code/init_functions.R")
source("code/multiplot.R")

data <- init_dataframe() %>% 
  join_height_data() %>% 
  join_precip_data() %>% 
  join_globcover_data() %>% 
  join_ifl_2013_data() %>% 
  filter( canopy_height <= 50, 
          mean_annual_precipitation <= 3500)

semi_deciduous_forest_data <- filter(data, globcover_label == "Closed to open (>15%) broadleaved evergreen or semi-deciduous forest (>5m)")
broadleaved_forest_data <- filter(data, globcover_label == "Closed to open (>15%) mixed broadleaved and needleleaved forest (>5m)")


all_vegetation_filter_2 <- c(40, 50, 60, 70, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 220)
all_vegetation_filter_3 <- c(40, 50, 60, 70, 90, 100, 110, 120, 130, 140, 150)

all_vegetation_2 <- data[data$globcover_numeric %in% all_vegetation_filter_2 ,]
all_vegetation_3 <- data[data$globcover_numeric %in% all_vegetation_filter_3 ,]



p1 <- ggplot(semi_deciduous_forest_data) + 
  ggtitle("Intact vs Non-Intact Distributions of Closed to open (>15%) broadleaved evergreen or semi-deciduous forest (>5m)") +
  geom_bin2d(aes(x=mean_annual_precipitation, y=canopy_height), bins=50) + 
  scale_fill_distiller(palette= "Spectral", direction=-1, aesthetics = "fill") +
  xlim(0, 3500) +
  ylim(0, 50) +
  facet_grid(cols = vars(intact_forest_2013))  

p2_density <- ggplot(semi_deciduous_forest_data) + 
  ggtitle("Distribution of Closed to open (>15%) broadleaved evergreen or semi-deciduous forest (>5m)") +
  geom_bin2d(aes(x=mean_annual_precipitation, y=canopy_height), bins=50) +
  scale_fill_distiller(palette= "Spectral", direction=-1, aesthetics = "fill")+
  geom_point(aes(x = mean_annual_precipitation, y=canopy_height), size = 0.01, alpha = 0) + 
  xlim(0, 3500) +
  ylim(0, 50)

p2 <- ggMarginal(p2.density, type = "density")

p3 <- ggplot(broadleaved_forest_data) + 
  ggtitle("Intact vs Non-Intact Distributions of Closed to open (>15%) mixed broadleaved and needleleaved forest (>5m)" ) +
  geom_bin2d(aes(x=mean_annual_precipitation, y=canopy_height), bins=50) + 
  scale_fill_distiller(palette= "Spectral", direction=-1, aesthetics = "fill") +
  xlim(0, 3500) +
  ylim(0, 50) +
  facet_grid(cols = vars(intact_forest_2013))  

p4_density <- ggplot(broadleaved_forest_data) + 
  ggtitle("Distribution of Closed to open (>15%) mixed broadleaved and needleleaved forest (>5m)" ) +
  geom_bin2d(aes(x=mean_annual_precipitation, y=canopy_height), bins=50) +
  scale_fill_distiller(palette= "Spectral", direction=-1, aesthetics = "fill")+
  geom_point(aes(x = mean_annual_precipitation, y=canopy_height), size = 0.01, alpha = 0) + 
  xlim(0, 3500) +
  ylim(0, 50)

p5 <- ggplot(all_vegetation_2) + 
  ggtitle("Intact vs Non-Intact Distributions of intermediate excluded vegetaion cells" ) +
  geom_bin2d(aes(x=mean_annual_precipitation, y=canopy_height), bins=50) + 
  scale_fill_distiller(trans="log", palette= "Spectral", direction=-1, aesthetics = "fill") +
  xlim(0, 3500) +
  ylim(0, 50) +
  facet_grid(cols = vars(intact_forest_2013)) 

p6_density <- ggplot(all_vegetation_2) + 
  ggtitle("Distributions of intermediate excluded vegetation cells" ) +
  geom_bin2d(aes(x=mean_annual_precipitation, y=canopy_height), bins=50) +
  scale_fill_distiller(trans="log", palette= "Spectral", direction=-1, aesthetics = "fill")+
  geom_point(aes(x = mean_annual_precipitation, y=canopy_height), size = 0.01, alpha = 0) + 
  xlim(0, 3500) +
  ylim(0, 50)

p7 <- ggplot(all_vegetation_3) + 
  ggtitle("Intact vs Non-Intact Distributions of strict excluded vegetation cells" ) +
  geom_bin2d(aes(x=mean_annual_precipitation, y=canopy_height), bins=50) + 
  scale_fill_distiller(trans="log", palette= "Spectral", direction=-1, aesthetics = "fill") +
  xlim(0, 3500) +
  ylim(0, 50) +
  facet_grid(cols = vars(intact_forest_2013)) 
  
p8_density <- ggplot(all_vegetation_3) + 
  ggtitle("Distributions of strict excluded vegetation cells" ) +
  geom_bin2d(aes(x=mean_annual_precipitation, y=canopy_height), bins=50) +
  scale_fill_distiller(trans="log", palette= "Spectral", direction=-1, aesthetics = "fill")+
  geom_point(aes(x = mean_annual_precipitation, y=canopy_height), size = 0.01, alpha = 0) + 
  xlim(0, 3500) +
  ylim(0, 50)


multiplot(p1, p3, p5, p7, p2_density, p4_density, p6_density, p8_density, cols=2)

