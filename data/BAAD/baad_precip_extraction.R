# baad_precip_extraction.R 

# Packages 
library(raster)
library(maptools)
library(rgdal)
library(ncdf4)
library(sp)
library(tidyverse)

# Set working directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Source files from /code 
source("data_processing.R")
source("init_functions.R")
source("multiplot.R")

format_months <- function(val) {
  if(val < 10) {
    val <- paste("0", val, sep="")
  } else if(val >= 10) {
    val <- as.character(val)
  } 
  val
}

extract_map <- function(precip.data, longitude, latitude) {
  point <- SpatialPoints(data.frame(longitude = longitude, latitude = latitude), 
                         proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  val <- raster::extract(precip.data, point)
}

precip.data <- 1:12 %>% 
  lapply(function(x){format_months(x)}) %>% 
  lapply(function(x) {raster(paste("data/wc2.0/wc2.0_30s_prec_", x, ".tif", sep=""))}) %>% 
  stack() %>% 
  sum()

precip.data.alternate <- TempMapRaster()

data <- baad.data::baad_data()$data %>%  
  as_tibble() %>% filter(!is.na(age), !is.na(longitude), !is.na(latitude)) %>% 
  mutate(mean_annual_precipitation = extract_map(precip.data, longitude, latitude))

# write.csv(data, "data/BAAD/baad_with_map.csv")





