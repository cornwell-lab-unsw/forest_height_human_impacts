
##### Helper functions ---- 
convert_to_sf_points <- function(points, sf_object_src) {
  pts <- dplyr::select(points, long, lat) %>% 
    mutate(sf_formatted_pts = paste("POINT(",long, " ",lat,")"))
  
  pts_sfc <- st_as_sfc(unlist(pts$sf_formatted_pts)) %>% 
    st_sf(ID = paste0(1:length(pts$sf_formatted_pts)))
  st_crs(pts_sfc) <- st_crs(sf_object_src)
  pts_sfc
}


intact_precip_categorisation <- function(intact_status_list, mean_annual_precipitation_list) {
  unlist(lapply(1:length(intact_status_list), function(x) {
      intact_status <- intact_status_list[x]
      mean_annual_precipitation <- mean_annual_precipitation_list[x]
      if(intact_status == "Intact Forest") {
        if(mean_annual_precipitation < 1500) {
          "Intact 0mm-1500mm"
        } else if(mean_annual_precipitation >= 1500 & mean_annual_precipitation < 2000) {
          "Intact 1500mm-2000mm"
        } else if(mean_annual_precipitation >= 2000) {
          "Intact > 2000mm" 
        }
      } else {
        if(mean_annual_precipitation < 1500) {
          "Non-Intact 0mm-1500mm"
        } else if(mean_annual_precipitation >= 1500 & mean_annual_precipitation < 2000) {
          "Non-Intact 1500mm-2000mm"
        } else if(mean_annual_precipitation >= 2000) {
          "Non-Intact > 2000mm" 
        }
      }
    }
  ))
}

get_cell_boundaries <- function(grid_cons, data) {
  data$cells <- dgGEO_to_SEQNUM(grid_cons, data$long, data$lat)$seqnum
  cell_boundaries <- dgcellstogrid(grid_cons, data$cells, frame=TRUE, wrapcells=TRUE)
}

partition_data <- function(grid_cons, data) {
  partitioned_data <- unique(data$intact_precip_status) %>% 
    lapply(function(x) { get_cell_boundaries(grid_cons, data[data$intact_precip_status == x ,]) })
  names(partitioned_data) <- unique(data$intact_precip_status)
  partitioned_data
}

equal_area_projection <- function() {
  ##### Drake ----
  equal_area_projection <- drake_plan(
    # set up grid
    grid_cons = dgconstruct(projection = "FULLER", aperture = 3, area = 863.80061),
    grid = dgearthgrid(grid_cons, frame = T, wrapcells =  T),
    grid_list = split(grid[,c("long","lat")], grid$group),
    ps = sapply(grid_list, Polygon),
    p1 =  lapply(seq_along(ps), function(i) Polygons(list(ps[[i]]), ID = names(grid_list)[i])),
    grid_list_polys = SpatialPolygons(p1, proj4string = CRS("+proj=longlat +datum=WGS84")),
    
    # collect centres
    centroids = lapply(1:length(grid_list_polys@polygons), function(x) {grid_list_polys@polygons[[x]]@labpt}),
    long = unlist(lapply(centroids, function(x) x[1])),
    lat = unlist(lapply(centroids, function(x) x[2])),
    centroids_df = data.frame(long=long, lat=lat) %>% 
      filter(lat >= -60, 
             lat <= 60),
    
    # get raster data to do equal porjection analyses on 
    intact_forests = sf::st_read("data/IFL_2013/ifl_2013.shp"),
    precip_data = TempMapRaster(),
    globcover_data = raster("data/Globcover/Globcover_V2.2_Global/GLOBCOVER_200412_200606_V2.2_Global_CLA.tif"),
    height_data = raster("data/Scheffer_2018/90th_percentile_height.tif"),
    wdpa_data = sf::st_read("data/WDPA_Nov2019-shapefile/WDPA_Nov2019-shapefile-polygons.shp"), 
    globcover_legend = readxl::read_xls("data/Globcover/Globcover_V2.2_Global/Globcover_Legend.xls"),
    
    temp_pts = dplyr::select(centroids_df, long, lat), 
    centroid_pts_sfc = convert_to_sf_points(centroids_df, intact_forests),
    overlay_df = over(as(centroid_pts_sfc, "Spatial"), as(intact_forests, "Spatial")), 
    overlay_df_with_coords = cbind(overlay_df, long = temp_pts$long, 
                        lat = temp_pts$lat),
    protected_df = over(as(centroid_pts_sfc, "Spatial"), as(wdpa_data, "Spatial")),
    protected_df_with_coords = cbind(protected_df, long = temp_pts$long, 
                                   lat = temp_pts$lat),
    

    
    # set up main dataframe 
    scheffer_data_reprojected = left_join(overlay_df_with_coords, protected_df_with_coords, by=c("long"="long", "lat"="lat"), all.x=TRUE) %>% 
      mutate(intact_forest_2013 = ifelse(!is.na(IFL_ID), "Intact Forest", "Non-Intact Forest"),
            protected_status = ifelse(!is.na(WDPAID), "Protected Area", "Non-Protected Area"),
            mean_annual_precipitation=extract_raster_data(precip_data, long, lat),
            globcover_numeric = extract_raster_data(globcover_data, long, lat),
            canopy_height = extract_raster_data(height_data, long, lat)) %>%
      left_join(globcover_legend, by=c("globcover_numeric"="Value"), all.x=TRUE) %>%
      rename(globcover_label = Label) %>% 
      dplyr::select(long, lat, canopy_height, mean_annual_precipitation, globcover_label, globcover_numeric, intact_forest_2013, protected_status), 
    
    equal_area_projection_analyses = scheffer_data_reprojected[!is.na(scheffer_data_reprojected$mean_annual_precipitation) & !is.na(scheffer_data_reprojected$globcover_numeric) ,] %>% 
      intermediate_exclusion_filter(),
    tall_trees_analyses = scheffer_data_reprojected[!is.na(scheffer_data_reprojected$mean_annual_precipitation) & !is.na(scheffer_data_reprojected$globcover_numeric) ,] %>% 
      intermediate_exclusion_filter() %>% 
      filter(canopy_height > 25) %>%
      as_tibble() %>% 
      mutate(status = case_when(
              intact_forest_2013 == "Non-Intact Forest" & protected_status == "Non-Protected Area" & mean_annual_precipitation < 1500 ~ "Non-Intact Non-Protected 0mm-1500mm",  
              intact_forest_2013 == "Non-Intact Forest" & protected_status == "Non-Protected Area" & mean_annual_precipitation >= 1500 & mean_annual_precipitation < 2000 ~ "Non-Intact Non-Protected 1500mm-2000mm",  
              intact_forest_2013 == "Non-Intact Forest" & protected_status == "Non-Protected Area" & mean_annual_precipitation >= 2000 ~ "Non-Intact Non-Protected > 2000mm",  
              intact_forest_2013 == "Non-Intact Forest" & protected_status == "Protected Area" & mean_annual_precipitation < 1500 ~ "Non-Intact Protected 0mm-1500mm",  
              intact_forest_2013 == "Non-Intact Forest" & protected_status == "Protected Area" & mean_annual_precipitation >= 1500 & mean_annual_precipitation < 2000 ~ "Non-Intact Protected 1500mm-2000mm",  
              intact_forest_2013 == "Non-Intact Forest" & protected_status == "Protected Area" &  mean_annual_precipitation >= 2000  ~ "Non-Intact Protected > 2000mm",  
              intact_forest_2013 == "Intact Forest" & protected_status == "Non-Protected Area" & mean_annual_precipitation < 1500 ~ "Intact Non-Protected 0mm-1500mm",  
              intact_forest_2013 == "Intact Forest" & protected_status == "Non-Protected Area" & mean_annual_precipitation >= 1500 & mean_annual_precipitation < 2000 ~ "Intact Non-Protected 1500mm-2000mm",  
              intact_forest_2013 == "Intact Forest" & protected_status == "Non-Protected Area" & mean_annual_precipitation >= 2000 ~ "Intact Non-Protected > 2000mm",  
              intact_forest_2013 == "Intact Forest" & protected_status == "Protected Area" & mean_annual_precipitation < 1500 ~ "Intact Protected 0mm-1500mm",  
              intact_forest_2013 == "Intact Forest" & protected_status == "Protected Area" & mean_annual_precipitation >= 1500 & mean_annual_precipitation < 2000 ~ "Intact Protected 1500mm-2000mm",  
              intact_forest_2013 == "Intact Forest" & protected_status == "Protected Area" &  mean_annual_precipitation >= 2000  ~ "Intact Protected > 2000mm",   
            )
      ), 
    
    ordered_status_list = c("Intact Non-Protected 0mm-1500mm" ,         "Intact Non-Protected 1500mm-2000mm"   ,     "Intact Non-Protected > 2000mm"  ,  
                             "Intact Protected 0mm-1500mm"  ,            "Intact Protected 1500mm-2000mm"  ,          "Intact Protected > 2000mm"   ,      
                             "Non-Intact Non-Protected 0mm-1500mm"  ,    "Non-Intact Non-Protected 1500mm-2000mm"  ,  "Non-Intact Non-Protected > 2000mm",
                             "Non-Intact Protected 0mm-1500mm"  ,      "Non-Intact Protected 1500mm-2000mm"  ,      "Non-Intact Protected > 2000mm" ),  
    hexagonal_area = 863.80061, 
    tall_intact_area_summary = c(hexagonal_area*nrow(tall_trees_analyses[tall_trees_analyses$mean_annual_precipitation < 1500 & 
                                                                           tall_trees_analyses$intact_forest_2013 == "Intact Forest" ,]),
                                  hexagonal_area*nrow(tall_trees_analyses[tall_trees_analyses$mean_annual_precipitation >= 1500 & 
                                                                            tall_trees_analyses$mean_annual_precipitation < 2000 &
                                                                            tall_trees_analyses$intact_forest_2013 == "Intact Forest" ,]),
                                  hexagonal_area*nrow(tall_trees_analyses[tall_trees_analyses$mean_annual_precipitation >= 2000 & 
                                                                            tall_trees_analyses$intact_forest_2013 == "Intact Forest" ,])
                                ),
    tall_non_intact_area_summary = c(hexagonal_area*nrow(tall_trees_analyses[tall_trees_analyses$mean_annual_precipitation < 1500 & 
                                                                                tall_trees_analyses$intact_forest_2013 == "Non-Intact Forest" ,]),
                                      hexagonal_area*nrow(tall_trees_analyses[tall_trees_analyses$mean_annual_precipitation >= 1500 & 
                                                                                tall_trees_analyses$mean_annual_precipitation < 2000 &
                                                                                tall_trees_analyses$intact_forest_2013 == "Non-Intact Forest" ,]),
                                      hexagonal_area*nrow(tall_trees_analyses[tall_trees_analyses$mean_annual_precipitation >= 2000 & 
                                                                                tall_trees_analyses$intact_forest_2013 == "Non-Intact Forest" ,]) 
                              ),
    
    intact_area_summary = c(hexagonal_area*nrow(equal_area_projection_analyses[equal_area_projection_analyses$mean_annual_precipitation < 1500 & 
                                                                              equal_area_projection_analyses$intact_forest_2013 == "Intact Forest" ,]),
                            hexagonal_area*nrow(equal_area_projection_analyses[equal_area_projection_analyses$mean_annual_precipitation >= 1500 & 
                                                                               equal_area_projection_analyses$mean_annual_precipitation < 2000 &
                                                                                                       equal_area_projection_analyses$intact_forest_2013 == "Intact Forest" ,]),
                            hexagonal_area*nrow(equal_area_projection_analyses[equal_area_projection_analyses$mean_annual_precipitation >= 2000 & 
                                                                               equal_area_projection_analyses$intact_forest_2013 == "Intact Forest" ,])
                          ),
    non_intact_area_summary = c(hexagonal_area*nrow(equal_area_projection_analyses[equal_area_projection_analyses$mean_annual_precipitation < 1500 & 
                                                                                   equal_area_projection_analyses$intact_forest_2013 == "Non-Intact Forest" ,]),
                               hexagonal_area*nrow(equal_area_projection_analyses[equal_area_projection_analyses$mean_annual_precipitation >= 1500 & 
                                                                                   equal_area_projection_analyses$mean_annual_precipitation < 2000 &
                                                                                   equal_area_projection_analyses$intact_forest_2013 == "Non-Intact Forest" ,]),
                               hexagonal_area*nrow(equal_area_projection_analyses[equal_area_projection_analyses$mean_annual_precipitation >= 2000 & 
                                                                                  equal_area_projection_analyses$intact_forest_2013 == "Non-Intact Forest" ,])
                              ),
    total_area = sum(intact_area_summary, non_intact_area_summary),
    tall_total_area = sum(tall_intact_area_summary, tall_non_intact_area_summary),
    
    intact_percentage_summary = round(unlist(lapply(intact_area_summary, function(x) {(x/total_area)*100})), digits=2),
    non_intact_percentage_summary = round(unlist(lapply(non_intact_area_summary, function(x) {(x/total_area)*100})), digits=2),
    tall_intact_percentage_summary = round(unlist(lapply(tall_intact_area_summary, function(x) {(x/tall_total_area)*100})), digits=2),
    tall_non_intact_percentage_summary = round(unlist(lapply(tall_non_intact_area_summary, function(x) {(x/tall_total_area)*100})), digits=2),
    tall_intact_protected_area_summary = lapply(ordered_status_list, function(x) {tall_trees_analyses[tall_trees_analyses$status == x ,]}) %>% 
      lapply(function(x) {hexagonal_area * nrow(x)}) %>% 
      unlist(), 
    tall_intact_protected_percentage_summary = lapply(tall_intact_protected_area_summary, function(x) {(x/tall_total_area)*100}) %>% 
      unlist(),
    
    table_area_summary = rbind(round(intact_area_summary, digits=2), round(non_intact_area_summary, digits=2)),
    table_percentage_summary = rbind(intact_percentage_summary, non_intact_percentage_summary),
    table_area_summary_tall = rbind(round(tall_intact_area_summary, digits=2), round(tall_non_intact_area_summary, digits=2)),
    table_percentage_summary_tall = rbind(tall_intact_percentage_summary, tall_non_intact_percentage_summary),
    
    table_area_summary_tall_intersected = rbind(round(tall_intact_protected_area_summary[1:3], digits=2), 
                                                round(tall_intact_protected_area_summary[4:6], digits=2), 
                                                round(tall_intact_protected_area_summary[7:9], digits=2), 
                                                round(tall_intact_protected_area_summary[10:12], digits=2)),
    table_percentage_summary_tall_intersected = rbind(round(tall_intact_protected_percentage_summary[1:3], digits=2), 
                                                      round(tall_intact_protected_percentage_summary[4:6], digits=2), 
                                                      round(tall_intact_protected_percentage_summary[7:9], digits=2), 
                                                      round(tall_intact_protected_percentage_summary[10:12], digits=2)),
    
    ##### Global map of tall forests
    tall_trees_analyses_partitioned = partition_data(grid_cons, tall_trees_analyses) 
    # intact_forest_cell_boundaries = get_cell_boundaries(grid_cons, intact_forest_cells)
  )
  
  area_summary_table <- drake_plan(
    table_area_plot = 
      ggtexttable(table_area_summary,
                  theme=ttheme("classic"),
                  row=c("Intact Forests (km^2)", "Non-Intact Forests (km^2)"), 
                  col=c("0mm-1500mm", "1500mm-2000mm", "> 2000mm")), 
    table_percentage_plot =
      ggtexttable(table_percentage_summary,
                  theme=ttheme("classic"),
                  row=c("Intact Forests %", "Non-Intact Forests %"), 
                  col=c("0mm-1500mm", "1500mm-2000mm", "> 2000mm")),
    
    table_area_plot_tall = 
      ggtexttable(table_area_summary_tall,
                  theme=ttheme("classic"),
                  row=c("Tall Intact Forests (km^2)", "Tall Non-Intact Forests (km^2)"), 
                  col=c("0mm-1500mm", "1500mm-2000mm", "> 2000mm")), 
    table_percentage_plot_tall =
      ggtexttable(table_percentage_summary_tall,
                  theme=ttheme("classic"),
                  row=c("Tall Intact Forests %", "Tall Non-Intact Forests %"), 
                  col=c("0mm-1500mm", "1500mm-2000mm", "> 2000mm")),
    
    table_area_plot_tall_intersected = 
      ggtexttable(table_area_summary_tall_intersected, 
                  col=c("0mm-1500mm", "1500mm-2000mm", "> 2000mm"), 
                  row=c("Intact Non-Protected Forests (km^2)", 
                        "Intact Protected Forests (km^2)", 
                        "Non-Intact Non-Protected Forests (km^2)", 
                        "Non-Intact Protected Forests (km^2)")),
    table_percentage_plot_tall_intersected =
      ggtexttable(table_percentage_summary_tall_intersected, 
                  col=c("0mm-1500mm", "1500mm-2000mm", "> 2000mm"), 
                  row=c("Intact Non-Protected Forests (%)", 
                        "Intact Protected Forests (%)", 
                        "Non-Intact Non-Protected Forests (%)", 
                        "Non-Intact Protected Forests (%)")),
    
    countries = map_data("world")
    
    #tall_tree_distribution_map = ggplot() + 
    #  geom_polygon(data=countries, aes(x=long, y=lat, group=group), fill=NA, color="black") + 
    #  geom_polygon(data=tall_trees_analyses_partitioned[[1]], aes(x=long, y=lat, group=group, fill="Non-Intact 0mm-1500mm")) +
    #  geom_polygon(data=tall_trees_analyses_partitioned[[2]], aes(x=long, y=lat, group=group, fill="Non-Intact 2000mm")) +
    #  geom_polygon(data=tall_trees_analyses_partitioned[[3]], aes(x=long, y=lat, group=group, fill="Non-Intact 1500mm-2000mm")) +
    #  geom_polygon(data=tall_trees_analyses_partitioned[[4]], aes(x=long, y=lat, group=group, fill="Intact 0mm-1500mm")) +
    #  geom_polygon(data=tall_trees_analyses_partitioned[[5]], aes(x=long, y=lat, group=group, fill="Intact 1500mm-2000mm")) +
    #  geom_polygon(data=tall_trees_analyses_partitioned[[6]], aes(x=long, y=lat, group=group, fill="Intact > 2000mm")) + 
    #  scale_fill_manual(name="Forest Intactness / Annual Rainfall Status", 
    #                    values=c(`Non-Intact 0mm-1500mm`="yellow",
    #                            `Non-Intact 2000mm`="red",
    #                            `Non-Intact 1500mm-2000mm`="orange",
    #                            `Intact 0mm-1500mm`="blue",
    #                            `Intact 1500mm-2000mm`="green",
    #                            `Intact > 2000mm`="dark green"),
    #                    ) +
    #  theme_bw() + 
    #  scale_x_continuous(breaks=c(seq(-180, 180, by=60))) + xlab("longitude") +
    #  ylim(-60, 90) + ylab("latitude")
    
  )
  
  area_summary_table_pdf <- drake_plan(
    area_summary_table_pdf = 
      ggsave("figure_total_area_summary.pdf", plot = table_area_plot,
             device = "pdf", path = "figures/Scheffer Extended",
             width = 6, height = 2),
    percentage_summary_table_pdf = 
      ggsave("figure_total_percentage_summary.pdf", plot = table_percentage_plot,
             device = "pdf", path = "figures/Scheffer Extended",
             width = 6, height = 2), 
    
    area_summary_table_tall_pdf = 
      ggsave("figure_tall_area_summary.pdf", plot = table_area_plot_tall,
             device = "pdf", path = "figures/Scheffer Extended",
             width = 6, height = 2),
    percentage_summary_table_tall_pdf = 
      ggsave("figure_tall_percentage_summary.pdf", plot = table_percentage_plot_tall,
             device = "pdf", path = "figures/Scheffer Extended",
             width = 6, height = 2),
    
    area_summary_table_tall_intersected_pdf = 
      ggsave("figure_tall_area_intersected_summary.pdf", plot = table_area_plot_tall_intersected,
             device = "pdf", path = "figures/Scheffer Extended",
             width = 8, height = 2),
    percentage_summary_table_tall_intersected_pdf = 
      ggsave("figure_tall_percentage_intersected_summary.pdf", plot = table_percentage_plot_tall_intersected,
             device = "pdf", path = "figures/Scheffer Extended",
             width = 8, height = 2)
    
    # tall_tree_distribution_map_pdf = 
    #  ggsave("figure_tall_tree_distribution_map.pdf", plot = tall_tree_distribution_map, 
    #         device = "pdf", path = "figures/Scheffer Extended",
    #         width = 15, height = 7) 
  )
  
  combined_plan = rbind(equal_area_projection,
                        area_summary_table,
                        area_summary_table_pdf)
}
