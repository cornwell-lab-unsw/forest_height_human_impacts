# figure_height_enactment_year.R 

# Load packages
library(maptools)
library(dplyr)
library(raster)
library(rgdal) 
library(ncdf4)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(ggExtra)
library(sf)
library(drake)

# Load functions
source("code/data_processing.R") 
source("code/custom_colour_palettes.R")

protected_layer <- sf::st_read("data/WDPA_Nov2019-shapefile/WDPA_Nov2019-shapefile-polygons.shp")

semi_deciduous_pts <- init_dataframe() %>% 
  join_globcover_data() %>% 
  filter(globcover_label == "Closed to open (>15%) broadleaved evergreen or semi-deciduous forest (>5m)") %>%
  mutate(sf_formatted_pts = paste("POINT(",x, " ",y,")", sep=""))

# preserve a copy of points
temp_coords <- select(semi_deciduous_pts, x, y) 

# convert semi deciduous points to sf POINTS
pts_sfc <- st_as_sfc(unlist(semi_deciduous_pts$sf_formatted_pts)) %>% 
  st_sf(ID = paste0(1:length(semi_deciduous_pts$sf_formatted_pts)))
st_crs(pts_sfc) <- st_crs(protected_layer)

height_enactment_year_data <- over(as(pts_sfc, "Spatial"), as(protected_layer, "Spatial")) %>%
  cbind(x = temp_coords$x, y = temp_coords$y) %>% 
  as_tibble() %>% 
  mutate(protected = ifelse(!is.na(WDPA_PID), "Protected", "Non-Protected")) %>% 
  select(x, y, protected, STATUS_YR) %>% 
  join_height_data() %>% 
  join_precip_data() %>% 
  join_ifl_2013_data() %>% 
  drop_na(STATUS_YR) %>% 
  filter(canopy_height <= 50, 
         mean_annual_precipitation <= 3500, 
         STATUS_YR != 0) %>% 
  rename(STATUS_YR = enactment_year)

p2 <- ggplot() + 
  ggtitle("Canopy Height vs Enactment Year Distributions (Protected Forests)") + 
  geom_point(data = height_enactment_year_data, 
             aes(x = enactment_year, y = canopy_height), size = 0.1) +
  scale_colour_gradientn(colours = p) +
  ylim(0,50) + 
  facet_grid(cols = vars(intact_forest_2013))









