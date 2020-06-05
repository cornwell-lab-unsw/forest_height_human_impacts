# figure_one_histograms.R 
# 
# Replication of Figure 1a) histograms from 
# A global climate niche for giant trees
# https://onlinelibrary.wiley.com/doi/abs/10.1111/gcb.14167

library(raster)
library(rgdal) 
library(ncdf4)
library(tidyverse)
library(dplyr)
library(ggplot2)

# Source files from /code
source("code/init_functions.R") 
source("code/data_processing.R")

figure_one_histogram_data <- init_dataframe() %>% 
  join_height_data() %>% 
  join_precip_data() %>%
  join_globcover_data() %>% 
  join_tree_cover_data() %>% 
  intermediate_exclusion_filter()

binned_data = 1:6 %>% 
  lapply(function(x) {figure_one_histogram_data[figure_one_histogram_data$mean_annual_precipitation <= x*500 & 
                                                  figure_one_histogram_data$mean_annual_precipitation > (x-1)*500 ,]}) 

histogram_plots <- 
  lapply(1:6, function(x) {
    gghistogram(binned_data[[x]], x = "canopy_height", fill = "dark blue",
                binwidth = 2) + 
      ggtitle(paste0(as.character((x-1)*500), "mm - ", as.character(x*500), "mm")) + 
      xlab("canopy_height (m)") +
      ylab("frequency") + 
      xlim(0,70)
  })

ggarrange(histogram_plots[[1]], 
   histogram_plots[[2]],
   histogram_plots[[3]],
   histogram_plots[[4]],
   histogram_plots[[5]],
   histogram_plots[[6]])




  
