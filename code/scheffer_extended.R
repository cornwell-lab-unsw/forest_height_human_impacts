# scheffer_extended.R

scheffer_extended_analyses <- function () {

  ##### 01 Analyses per figure ----
  
  deciduous_forests_analyses_plan <- drake_plan(
    protected_layer = sf::st_read("data/WDPA_Nov2019-shapefile/WDPA_Nov2019-shapefile-polygons.shp"), 
    
    semi_deciduous_pts = init_dataframe() %>% 
      join_globcover_data() %>% 
      filter(globcover_label == "Closed to open (>15%) broadleaved evergreen or semi-deciduous forest (>5m)") %>%
      mutate(sf_formatted_pts = paste("POINT(",x, " ",y,")", sep="")),
    
    # preserve a copy of points
    temp_coords = dplyr::select(semi_deciduous_pts, x, y), 
    
    # convert semi deciduous points to sf POINTS
    pts_sfc = convert_pts_to_sfc(semi_deciduous_pts, protected_layer),
    
    semi_deciduous_forest_WDPA_data = over(as(pts_sfc, "Spatial"), as(protected_layer, "Spatial")) %>%
      cbind(x = temp_coords$x, y = temp_coords$y) %>% 
      as_tibble() %>% 
      mutate(protected = ifelse(!is.na(WDPA_PID), "Protected", "Non-Protected")) %>% 
      dplyr::select(x, y, protected, STATUS_YR) %>% 
      join_height_data() %>% 
      join_precip_data() %>% 
      join_ifl_2013_data() %>% 
      filter(mean_annual_precipitation <= 3500, 
             canopy_height <= 50)
    
  )
  
  height_enactment_year_analyses_plan <- drake_plan(
    # extension of above analyses 
    height_enactment_year_data = filter(semi_deciduous_forest_WDPA_data, 
                                        STATUS_YR != 0) %>% 
      drop_na(STATUS_YR) %>% 
      rename(enactment_year = STATUS_YR) 
  )
  
  forest_height_climate_distributions_plan <- drake_plan(
    
    all_data = init_dataframe() %>% 
      join_height_data() %>% 
      join_precip_data() %>% 
      join_globcover_data() %>% 
      join_ifl_2013_data() %>% 
      filter( canopy_height <= 50, 
              mean_annual_precipitation <= 3500),
    
    semi_deciduous_forest_data = filter(all_data, globcover_label == "Closed to open (>15%) broadleaved evergreen or semi-deciduous forest (>5m)"),
    broadleaved_forest_data = filter(all_data, globcover_label == "Closed to open (>15%) mixed broadleaved and needleleaved forest (>5m)"),
    intermediate_excluded_vegetation_data = intermediate_exclusion_filter(all_data),
    strict_excluded_vegetation_data = strict_exclusion_filter(all_data)
    
  )
  
  ##### 02 ggPlots ----
  
  deciduous_forests_plot <- drake_plan(
    intact_protected_forest_distribution_plot = 
      ggscatter(semi_deciduous_forest_WDPA_data,
                x = "mean_annual_precipitation", y = "canopy_height", alpha=0) + 
      geom_bin2d(aes(x=mean_annual_precipitation, y=canopy_height), bins=50) +
      ggtitle("Intact vs Non-Intact Protected vs Non-Protected Distributions of Closed to open (>15%) broadleaved evergreen or semi-deciduous forest (>5m)") + 
      scale_fill_distiller(palette= "Spectral", direction=-1, aesthetics = "fill") +
      xlab("mean annual precipitation (mm)") +
      ylab("canopy height (m)") +
      facet_grid(cols = vars(intact_forest_2013), rows = vars(protected)) + 
      theme(
        legend.direction = "vertical", 
        legend.position = "right",
      )
  )
  
  height_enactment_year_plot <- drake_plan (
    p = custom_colour_scale(), 
    height_enactment_year_scatter = 
      ggscatter(height_enactment_year_data,
                x = "enactment_year", y = "canopy_height", 
                size = 0.1) +
      ggtitle("Canopy Height vs Enactment Year Distributions (Protected Forests)") +
      ylim(0,50) + 
      ylab("canopy height (m)") +
      xlab("year of declaration") +
      facet_grid(cols = vars(intact_forest_2013)) 
  )
  
  growth_rate_climate_plot <- drake_plan(
    growth_rate_climate_scatter = height_growth_modelling()
  )
  
  intact_non_intact_distributions_plot <- drake_plan(
    semi_deciduous_forest_split_density = 
      ggscatter(semi_deciduous_forest_data,
                x = "mean_annual_precipitation", y = "canopy_height", alpha=0) + 
      geom_bin2d(aes(x=mean_annual_precipitation, y=canopy_height), bins=50) +
      ggtitle("Intact vs Non-Intact Distributions of Closed to open (>15%) broadleaved evergreen or semi-deciduous forest (>5m)") + 
      scale_fill_distiller(palette= "Spectral", direction=-1, aesthetics = "fill") +
      xlab("mean annual precipitation (mm)") +
      ylab("canopy height (m)") +
      facet_grid(cols = vars(intact_forest_2013))+ 
      theme(
        legend.direction = "vertical", 
        legend.position = "right"
      ),
    
    all_vegetation_split_density = 
      ggscatter(intermediate_excluded_vegetation_data,
                x = "mean_annual_precipitation", y = "canopy_height", alpha=0) + 
      geom_bin2d(aes(x=mean_annual_precipitation, y=canopy_height), bins=50) +
      ggtitle("(Log-Scaled) Intact vs Non-Intact Distributions of All Vegetation ") + 
      scale_fill_distiller(trans = "log", palette= "Spectral", direction=-1, aesthetics = "fill", breaks = c(1,7,54)) +
      xlab("mean annual precipitation (mm)") +
      ylab("canopy height (m)") +
      facet_grid(cols = vars(intact_forest_2013))+ 
      theme(
        legend.direction = "vertical", 
        legend.position = "right"
      ),
    
    semi_deciduous_forest_density = 
      ggscatter(semi_deciduous_forest_data,
                x = "mean_annual_precipitation", y = "canopy_height", alpha=0) + 
      geom_bin2d(aes(x=mean_annual_precipitation, y=canopy_height), bins=50) +
      ggtitle("Total Distributions of Closed to open (>15%) broadleaved evergreen or semi-deciduous forest (>5m)") + 
      scale_fill_distiller(palette= "Spectral", direction=-1, aesthetics = "fill") +
      xlab("mean annual precipitation (mm)") +
      ylab("canopy height (m)") +
      theme(
        legend.direction = "vertical", 
        legend.position = "right"
      ), 
    
    all_vegetation_density = 
      ggscatter(intermediate_excluded_vegetation_data,
                x = "mean_annual_precipitation", y = "canopy_height", alpha=0) + 
      geom_bin2d( aes(x=mean_annual_precipitation, y=canopy_height), bins=50) +
      ggtitle("(Log-Scaled) Total Distributions of All Vegetation") + 
      scale_fill_distiller(trans = "log", palette= "Spectral", direction=-1, aesthetics = "fill", breaks = c(1,7,54)) +
      xlab("mean annual precipitation (mm)") +
      ylab("canopy height (m)") +
      theme(
        legend.direction = "vertical", 
        legend.position = "right"
      ), 
    gridded_plot_1 = ggarrange(semi_deciduous_forest_split_density, all_vegetation_split_density, 
                               nrow = 2),
    gridded_plot_2 = ggarrange(semi_deciduous_forest_density, all_vegetation_density, 
                               nrow = 2)
  )
  
  ##### 04 Output Final Figure Folder ----
  
  scheffer_extended_analyses_output <- drake_plan(
    growth_rate_climate_file = 
      ggsave("figure_growth_rate_climate.pdf", plot = growth_rate_climate_scatter,
             device = "pdf", path = "figures/Scheffer Extended"),
    height_enactment_year_figure = 
      ggsave("figure_height_enactment_year.pdf", plot = height_enactment_year_scatter,
             device = "pdf", path = "figures/Scheffer Extended", 
             width = 20, height = 13, units = "cm"), 
    intact_protected_distribution_figure = 
      ggsave("figure_intact_protected_distribution.pdf", plot = intact_protected_forest_distribution_plot,
             device = "pdf", path = "figures/Scheffer Extended", 
             width = 30, height = 20, units = "cm"),
    intact_non_intact_distributions_figure = 
      ggsave("figure_intact_non_intact_distributions.pdf", plot = gridded_plot_1,
             device = "pdf", path = "figures/Scheffer Extended", 
             width = 30, height = 20, units = "cm"), 
    combined_distributions = 
      ggsave("figure_combined_forest_distributions.pdf", plot = gridded_plot_2,
             device = "pdf", path = "figures/Scheffer Extended", 
             width = 25, height = 30, units = "cm"), 
  )
  
  ##### 05 Make ----
  
  scheffer_extended_analyses_plan <- rbind(deciduous_forests_analyses_plan, height_enactment_year_analyses_plan, forest_height_climate_distributions_plan, 
                                     deciduous_forests_plot, growth_rate_climate_plot, height_enactment_year_plot, intact_non_intact_distributions_plot, 
                                     scheffer_extended_analyses_output)
}