# figure_mapping_mid_precip_forests.R

library(raster)
library(rgdal) 
library(ncdf4)
library(dplyr)
library(ggplot2)
library(tidyverse)

# Source files from /code
source("code/init_functions.R") 
source("code/data_processing.R")

df<-get_precip_height_df()

df$in_precip_range<-df$mean_annual_precipitation<2250 & df$mean_annual_precipitation >1750
df$canopy_height_cat<-ifelse(df$canopy_height>35,
                         "big_forest >35m",
                         ifelse(df$canopy_height>20,
                                "in saddle 20<h<35",
                                "small_forest <20m"))

only_mid<-filter(df,in_precip_range)

library(rnaturalearth)
library(sp)

#world countries
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(only_mid,aes(x=x,y=y,col=canopy_height_cat))+geom_point()

ggplot(data = world) +
  geom_sf() +
  geom_point(data = only_mid, aes(x=x,y=y,col=canopy_height_cat),size=0.7)+
  ggtitle("Only forests between 1750 and 2250 mm / year ")


