## scheffer_replication.R 
## attempted replication of figures 1-3 from 
## A Global Climate Niche for Giant Trees
 
scheffer_replication_analyses <- function () {
  
  ##### 00 main data set ----
  
  main_scheffer_data <- drake_plan(
    scheffer_data = init_dataframe() %>% 
      join_height_data() %>% 
      join_precip_data() %>%
      join_globcover_data() %>% 
      join_tree_cover_data() 
  )
  
  ##### 01 analyses per figure replication
  
  figure_one_histogram_analyses <- drake_plan(
    figure_one_histogram_data = scheffer_data %>% 
      intermediate_exclusion_filter(), 
    
    binned_data = 1:6 %>% 
      lapply(function(x) {figure_one_histogram_data[figure_one_histogram_data$mean_annual_precipitation <= x*500 & 
                                                      figure_one_histogram_data$mean_annual_precipitation > (x-1)*500 ,]}) 
  ) 
  
  figure_one_density_analyses <- drake_plan(
    figure_one_density_data = scheffer_data %>% 
      intermediate_exclusion_filter() %>% 
      filter(canopy_height <= 50,
             mean_annual_precipitation <= 3500)
  ) 
  
  figure_one_percent_analyses <- drake_plan(
    figure_one_percent_init = scheffer_data %>% 
      intermediate_exclusion_filter() %>% 
      filter(mean_annual_precipitation >= 0,
             mean_annual_precipitation <= 3700)  %>% 
      group_by(range=cut(mean_annual_precipitation, breaks=seq(0,3700, by=20)) ) %>% 
      summarise(percentage=100*sum(canopy_height > 25)/n(), 
                total = n()) %>% 
      cbind(seq(0,3680, by=20)) %>% 
      dplyr::rename(mean_annual_precipitation = "seq(0, 3680, by = 20)"), 
    
    rolling_mean = zoo::rollmean(figure_one_percent_init$percentage,
                          10), 
    
    figure_one_percent_data = cbind(figure_one_percent_init[1:176,], rolling_mean) 
  )
  
  figure_two_map_analyses <- drake_plan(
    figure_two_map_data = scheffer_data %>% 
      intermediate_exclusion_filter() %>% 
      filter(canopy_height > 25) %>% 
      select(x, y, mean_annual_precipitation), 
    
    global_precipitation = init_dataframe() %>% 
      join_precip_data() %>% 
      join_globcover_data() %>% 
      filter(globcover_label != "Water bodies")
  )
  
  figure_three_density_analyses <- drake_plan(
    figure_three_density_data = scheffer_data %>% 
      intermediate_exclusion_filter() 
  )
  
  figure_three_scatter_analyses = drake_plan(
    figure_three_scatter_data = scheffer_data %>% 
      intermediate_exclusion_filter() %>% 
      filter(mean_annual_precipitation <= 4000)
  )
  
  ##### 02 ggPlots ----
  
  figure_one_histograms <- drake_plan(
    histogram_plots = 
      lapply(1:6, function(x) {
        gghistogram(binned_data[[x]], x = "canopy_height", fill = "dark blue",
          binwidth = 2) + 
        ggtitle(paste0(as.character((x-1)*500), "mm - ", as.character(x*500), "mm")) + 
          xlab("canopy_height (m)") +
          ylab("frequency") + 
          xlim(0,70)
      }), 
    
    histogram_grid = ggarrange(histogram_plots[[1]], 
                               histogram_plots[[2]],
                               histogram_plots[[3]],
                               histogram_plots[[4]],
                               histogram_plots[[5]],
                               histogram_plots[[6]])
      
  )
  
  figure_one_density_plot <- drake_plan(
    figure_one_density = 
      ggscatter(figure_one_density_data,
                x = "mean_annual_precipitation", y = "canopy_height", alpha=0) + 
      geom_bin2d( aes(x=mean_annual_precipitation, y=canopy_height), bins=50) +
      ggtitle("(Log-Scaled) Height-Climate Distributions of All Vegetation") + 
      scale_fill_distiller(name = "log(density)", trans = "log", palette= "Spectral", direction=-1, 
                           aesthetics = "fill", breaks = c(1,7,54)) +
      xlab("mean annual precipitation (mm)") +
      ylab("canopy height (m)") +
      theme(
        legend.direction = "vertical", 
        legend.position = "right"
      )
  )
  
  figure_one_percent_plot <- drake_plan(
    figure_one_percent_line = 
      ggline(readd(figure_one_percent_data), x = "mean_annual_precipitation" , y = "rolling_mean", 
          color = "blue", shape = NA, numeric.x.axis = TRUE) + 
          scale_x_continuous(limits=c(0,3500),breaks=c(0,500,1000,1500,2000,2500,3000,3500)) +
          ggtitle("Percentage giant forest grid cells") + 
          ylab("percentage of giant forest grid cells (>25m) %") +
          xlab("mean annual precipitation (mm)")
      
  )
  
  
  ## TODO: map of giant forest frid cells  
  figure_two_map_plot <- drake_plan(
    theme_set = theme_set(theme_bw()),
    
    world = ne_countries(scale = "medium", returnclass = "sf"),
    
    figure_two_map = 
      ggplot(data = world) +
        geom_sf() +
        xlab("Longitude") + ylab("Latitude") +
        geom_point(data=global_precipitation[global_precipitation$mean_annual_precipitation >= 1000 &
                                    global_precipitation$mean_annual_precipitation < 1500 ,], 
                   aes(x = x, y = y), 
                   size=0.01, 
                   shape=15, 
                   color="yellow") +
        geom_point(data=global_precipitation[global_precipitation$mean_annual_precipitation >= 1500 ,], 
                   aes(x = x, y = y), 
                   size=0.01, 
                   shape=15, 
                   color="orange") +
        geom_point(data=figure_two_map_data, aes(x = x, y = y), size=0.005, shape=15) +
        ylim(-60, 90) +
        ggtitle("Global distribution of giant forests")
  )
  
  figure_three_density_plot <- drake_plan(
    figure_three_density = 
      ggscatter(figure_three_density_data,
                x = "percentage_tree_cover", y = "canopy_height", alpha=0) + 
      geom_bin2d( aes(x=percentage_tree_cover, y=canopy_height), bins=50) +
      ggtitle("(Log-Scaled) Canopy Height-Percentage Cree Cover Distributions of All Vegetation") + 
      scale_fill_distiller(name = "log(density)" ,trans = "log", palette= "Spectral", direction=-1, 
                           aesthetics = "fill", breaks=c(0,7,54,403)) +
      ylab("canopy height (m)") +
      xlab("tree cover (%)") +
      theme(
        legend.direction = "vertical", 
        legend.position = "right"
      )
  )
  
  figure_three_scatter_plot <- drake_plan(
    spectral_palette = custom_colour_scale(), 
    
    figure_three_scatter = 
      ggscatter(figure_three_scatter_data, 
                x = "percentage_tree_cover", y = "canopy_height", color = "mean_annual_precipitation",
                size = 0.5) +
      scale_colour_gradientn(name = "MAP (mm)", colours = spectral_palette) + 
      ggtitle("Canopy Height-Percentage Tree Cover Scatter") +
      xlab("percentage tree cover (%)") +
      ylab("canopy height (m)") + 
      theme(
        legend.direction = "vertical", 
        legend.position = "right"
      )
  )
  
  
  ##### 03 output pdf ----
  
  scheffer_replication_output <- drake_plan(
    figure_one_histograms_pdf = 
      ggsave("figure_one_histograms.pdf", plot = histogram_grid,
             device = "pdf", path = "figures/Scheffer Replication",
             width = 10, height = 6), 
    figure_one_density_pdf = 
      ggsave("figure_one_density.pdf", plot = figure_one_density,
             device = "pdf", path = "figures/Scheffer Replication", 
             width = 10, height = 6),
    figure_one_percent_pdf = 
      ggsave("figure_one_percent.pdf", plot = figure_one_percent_line,
             device = "pdf", path = "figures/Scheffer Replication", 
             width = 8, height = 6),
    ## TODO: figure 2 
    figure_two_map_pdf = 
      ggsave("figure_two_map.pdf", plot = figure_two_map,
             device = "pdf", path = "figures/Scheffer Replication",
             width = 13, height = 6.7),
    figure_three_density_pdf = 
      ggsave("figure_three_density.pdf", plot = figure_three_density,
             device = "pdf", path = "figures/Scheffer Replication",
             width = 7, height = 6),
    figure_three_scatter_pdf = 
      ggsave("figure_three_scatter.pdf", plot = figure_three_scatter,
             device = "pdf", path = "figures/Scheffer Replication",
             width = 7, height = 6)
  )
  
  
  
  scheffer_replication_plan <- rbind(main_scheffer_data, 
                                     figure_one_density_analyses, figure_one_histogram_analyses, figure_one_percent_analyses,
                                     figure_two_map_analyses, 
                                     figure_three_density_analyses, figure_three_scatter_analyses, 
                                     figure_one_density_plot, figure_one_histograms, figure_one_percent_plot,
                                     figure_two_map_plot,
                                     figure_three_density_plot, figure_three_scatter_plot,
                                     scheffer_replication_output)
}