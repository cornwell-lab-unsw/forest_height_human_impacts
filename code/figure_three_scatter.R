# figure_three_scatter.R 
# 
# Replication of Figure 3b) scatter plot from 
# A global climate niche for giant trees
# https://onlinelibrary.wiley.com/doi/abs/10.1111/gcb.14167

library(raster)
library(rgdal) 
library(ncdf4)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(colorRamps)

# Source files from /code
source("code/init_functions.R") 
source("code/data_processing.R")
source("code/custom_colour_palettes.R")

# core data, dataframe which maps each grid cell 
# to a tree height value obtained from GLAS, and 
# percentage tree cover (MODIS)
coverheight.df <- get_cover_height_df()

# data for z-value of scatter plot, and 
# area type to filter out water, bare, and 
# artificial lands
areaprecip.df <- get_area_precip_df()

# left join, mapping all keys from 
# core data to respective keys and values from 
# areaprecip.df + filter out instances of water, bare
# and artificial surfaces e
processed.df <- merge(x = coverheight.df, 
                      y = areaprecip.df,
                      by = c("x","y"), 
                      all.x = TRUE) %>% 
  filter(area_type != 190,
         area_type != 200, 
         area_type != 210,
         area_type != 220,
         area_type != 11,
         area_type != 14,
         area_type != 20, 
         area_type != 30,
         area_type != 230) 

# detail-oriented code: (probably flawed) method
# of matching the mean annual precipitation 
# scale from the paper 
processed.df$mean_annual_precipitation[processed.df$mean_annual_precipitation > 4000] <- 4000

# set up spectral like colour palette
# using matlab.like wrapper function
p <- custom_colour_scale()

# scatter plot 
# x: percentage tree cover (%) 
# y: canopy height (m)
# z: mean annual precipitaion (mm)
ggplot() +
  geom_point(data = processed.df, 
             aes(x = percentage_tree_cover, y = canopy_height, color = mean_annual_precipitation),
             size = 0.01)  +
  scale_colour_gradientn(colours = p) +
  ylim(0, 60) +
  xlab("Tree cover (%)") +
  ylab("Canopy height (m)") +
  ggtitle("Figure 3b)")
  

