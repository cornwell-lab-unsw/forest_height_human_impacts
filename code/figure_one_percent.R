# figure_one_percent.R 
# 
# Replication of Figure 1 graph 
# (percentage of giant forest cells >25m) from 
# A global climate niche for giant trees
# https://onlinelibrary.wiley.com/doi/abs/10.1111/gcb.14167

library(raster)
library(rgdal) 
library(ncdf4)
library(tidyverse)
library(dplyr)
library(zoo)
library(ggplot2)

# Source files from /code
source("code/init_functions.R") 
source("code/data_processing.R")

# perform a left join from our globcover
# precipitation dataframe onto our tree modes using 
# coordinates as our key 
processed.dataframe <- get_precip_height_df() %>% 
  filter(mean_annual_precipitation >= 0,
         mean_annual_precipitation <= 3700)  %>% 
  group_by(range=cut(mean_annual_precipitation, breaks=seq(0,3700, by=20)) ) %>% 
  summarise(percentage=100*sum(canopy_height > 25)/n(), 
            total = n()) %>% 
  cbind(seq(0,3680, by=20)) %>% 
  dplyr::rename(mean_annual_precipitation = "seq(0, 3680, by = 20)")

temp <- zoo::rollmean(processed.dataframe$percentage,
                      10)

processed.dataframe <- cbind(processed.dataframe[1:176,], temp) 

ggplot(processed.dataframe, aes(x = mean_annual_precipitation, y = temp)) +
  geom_line(colour="blue") 
  
