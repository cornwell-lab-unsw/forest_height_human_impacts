# figure_intersected_distribution.R

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

semi.deciduos.data <- filter(data, globcover_label == "Closed to open (>15%) broadleaved evergreen or semi-deciduous forest (>5m)")

protected.layer <- sf::st_read("data/WDPA_Nov2019-shapefile/WDPA_Nov2019-shapefile-polygons.shp")
# plot(st_geometry(protected.layer))
pts <- select(semi.deciduos.data, x, y) %>% 
  mutate(sf_formatted_pts = paste("POINT(",x, " ",y,")"))

temp <- select(semi.deciduos.data, x, y) 
pts.sfc <- st_as_sfc(unlist(pts$sf_formatted_pts)) %>% 
  st_sf(ID = paste0(1:length(pts$sf_formatted_pts)))
st_crs(pts.sfc) <- st_crs(protected.layer)

processed.df <- over(as(pts.sfc, "Spatial"), as(protected.layer, "Spatial"))

# st_join(pts.sfc, protected.layer, join = st_intersects)

processed.df <- cbind(processed.df, x = temp$x, 
                      y = temp$y)  
temp <- select(processed.df, x, y)

df.copy <- processed.df %>% 
  is.na() %>% 
  as_tibble() %>% 
  mutate(protected = ifelse(!(WDPA_PID), "Protected", "Non-Protected")) %>% 
  cbind(x = temp$x, y = temp$y, enactment_year = processed.df$STATUS_YR) %>% 
  select(x, y, protected, enactment_year) %>% 
  join_height_data() %>% 
  join_ifl_2013_data() %>% 
  join_precip_data() 

r <- filter(df.copy, Intact_Forest_2013 == "Intact Forest", protected == "Non-Protected")
s <- filter(df.copy, Intact_Forest_2013 == "Intact Forest", protected == "Protected")

height.year.df <- drop_na(df.copy, enactment_year) %>% 
  select(x, y, canopy_height, enactment_year, Intact_Forest_2013) %>% 
  filter(# Intact_Forest_2013 == "Non-Intact Forest",
         enactment_year != 0)

p1 <- ggplot(df.copy) + 
  ggtitle("Intact vs Non-Intact Protected vs Non-Protected Distributions of Closed to open (>15%) broadleaved evergreen or semi-deciduous forest (>5m)") +
  geom_bin2d(aes(x=mean_annual_precipitation, y=canopy_height), bins=50) + 
  scale_fill_distiller(palette= "Spectral", direction=-1, aesthetics = "fill") +
  xlim(0, 3500) +
  ylim(0, 50) +
  facet_grid(cols = vars(Intact_Forest_2013), rows = vars(protected))

p2 <- ggplot(height.year.df) + 
  ggtitle("Canopy Height vs Enactment Year Distributions (Protected Forests)") + 
  # geom_bin2d(aes(x = enactment_year, y = canopy_height), bins=50) + 
  geom_point(aes(x = enactment_year, y = canopy_height), size = 0.05) +
  ylim(0,50) + 
  facet_grid(cols = vars(Intact_Forest_2013))
