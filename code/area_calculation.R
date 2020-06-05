# polygon_test.R 

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

# Set working directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Source files from /code 
source("data_processing.R")
source("init_functions.R")
source("multiplot.R")

convert_point_to_sf_polygon <- function(x, y) {
  poly <- paste("POLYGON((",
                x-0.25, " ",
                y-0.25, ",",
                x-0.25, " ",
                y+0.25, ",",
                x+0.25, " ",
                y-0.25, ",",
                x+0.25, " ",
                y+0.25, ",",
                x-0.25, " ",
                y-0.25, 
                "))",
                sep="")
}

protected.layer <- sf::st_read("../data/WDPA_Nov2019-shapefile/WDPA_Nov2019-shapefile-polygons.shp")
intact.landscapes.layer <- sf::st_read("../data/IFL_2013/ifl_2013.shp")

points.as.polygon <- select(df.copy, x, y) %>% 
  mutate(formatted_sf_polygon = convert_point_to_sf_polygon(x, y)) %>%
  select(formatted_sf_polygon) 

polygon <- unlist(points.as.polygon$formatted_sf_polygon) %>% 
  st_as_sfc(crs = st_crs(intact.landscapes.layer)) %>% 
  st_sf(polygon = 1:length(points.as.polygon$formatted_sf_polygon), geoms = ., stringsAsFactors = FALSE)

plot(polygon$geoms, add=TRUE)

pi <- st_intersection(intact.landscapes.layer, polygon)
