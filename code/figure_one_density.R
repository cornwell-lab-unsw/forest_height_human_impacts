# figure_one_density.R 
# 
# Replication of Figure 1 density plot from 
# A global climate niche for giant trees
# https://onlinelibrary.wiley.com/doi/abs/10.1111/gcb.14167

library(raster)
library(rgdal) 
library(ncdf4)
library(dplyr)
library(ggplot2)
library(tidyverse)

# Source files from /code
source("code/init_functions.R") 
source("code/data_processing.R")
source("code/custom_colour_palettes.R")

processed.dataframe <- get_precip_height_df() %>% 
  filter(canopy_height <= 50,
         mean_annual_precipitation <= 3500) #%>% 
  #group_by(range=cut(mean_annual_precipitation, breaks=seq(0,3500, by=500)) ) %>%
  #sample_n(438)

palette <- custom_log_density_scale()

# Show the area only
ggplot(processed.dataframe, aes(x=mean_annual_precipitation, y=canopy_height) ) +
  stat_density_2d(geom = "raster", aes(fill = stat(density)), contour = FALSE) + 
  scale_fill_gradientn(trans = "log", colours = palette) +
  xlim(0, 3500) +
  ylim(0, 50) + 
  ggtitle("Figure 1b) Loose Exclusion Criteria")

ggplot(processed.dataframe, aes(x=mean_annual_precipitation, y=canopy_height) ) +
  geom_bin2d(bins=50) +
  scale_fill_distiller(trans="log", palette= "Spectral", direction=-1, aesthetics = "fill")  +
  xlim(0, 3500) +   
  ylim(0, 50)  

#ggplot(processed.dataframe, aes(x=mean_annual_precipitation, y=canopy_height) ) +
#  geom_point()  +
#  xlim(0, 3500) +
#  ylim(0, 50)
