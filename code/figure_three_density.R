##' figure_three_density.R 
##' 
##' Replication of Figure 3a) prob density plot from 
##' A global climate niche for giant trees
##' https://onlinelibrary.wiley.com/doi/abs/10.1111/gcb.14167

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
         area_type != 230)  %>% 
  drop_na(mean_annual_precipitation, area_type, percentage_tree_cover) %>% 
  group_by(range=cut(percentage_tree_cover, breaks=seq(0,100, by=5)) ) %>%
  sample_n(400)

test <- palette(brewer.pal(n = 11, name = "Spectral"))
palette <- colorRamps::matlab.like(10)

ggplot(processed.df, 
       aes(x = percentage_tree_cover, y = canopy_height) ) +
  geom_bin2d(bins=50) +
  scale_fill_distiller(trans="log", palette= "Spectral", direction=-1, aesthetics = "fill") +
  ylim(0, 60)

ggplot(processed.df, 
       aes(x = percentage_tree_cover, y = canopy_height) ) +
  stat_density_2d(geom = "raster", aes(fill = stat(density)), contour = FALSE) +
  scale_fill_gradientn(colours = palette, trans="log") +
  ylim(0, 60) + 
  ggtitle("Figure 3a)")
