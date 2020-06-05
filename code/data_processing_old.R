# data_processing.R 
# script containing function implementations 
# for manipulating/aggregating global datasets 

init_dataframe <- function() {
  # create a dataframe with all coordinates within 
  # 60S 60N latitude at 0.5 degree resolution
  #
  # returns:
  #   dataframe
  
  x <- c(seq(-179.75, 179.75, by = 0.5))
  
  y <- c(seq(-59.75, 59.75, by = 0.5))
  
  x.y = merge(data.frame(x=x) , data.frame(y=y), by=NULL)
}

join_height_data <- function(df) {
  
  height.dataframe <- get_height_df()
  
  df <- merge(x = df,
              y = height.dataframe, 
              by=c("x","y"), 
              all.x = TRUE) %>% 
    drop_na(canopy_height) %>%
    reset_index()
  
}

join_precip_data <- function(df) {
  
  precip.dataframe <- get_precip_df()
  
  df <- merge(x = df,
              y = precip.dataframe, 
              by=c("x","y"), 
              all.x = TRUE) %>% 
    drop_na(mean_annual_precipitation) %>%
    reset_index()
}

join_tree_cover_data <- function(df) {
  
  treecover.dataframe <- get_tree_cover_df()
  
  df <- merge(x = df,
              y = treecover.dataframe, 
              by=c("x","y"), 
              all.x = TRUE) %>% 
    drop_na(percentage_tree_cover) %>%
    reset_index()
}

join_globcover_data <- function(df) {
  
  globcover.dataframe <- get_area_type_df()
  
  df <- merge(x = df,
              y = globcover.dataframe, 
              by=c("x","y"), 
              all.x = TRUE) %>% 
    drop_na(area_type) %>%
    reset_index()
}

join_ifl_2013_data <- function(df) {
  
  ifl_2013.dataframe <- get_ifl_2013_df()
  
  df <- merge(x = df,
              y = ifl_2013.dataframe, 
              by=c("x","y"), 
              all.x = TRUE) %>% 
    reset_index()
}


get_height_df <- function() {
  # gather 90th percentile height data
  # into a dataframe by mapping a coordinate to a
  # height value from its respective freq distribution
  # 
  # returns: 
  #   90th percentile height data between -60S and 
  #   60N latitude at 0.5 degree resolution, in the form of a dataframe 
  
  precipheight.dataframe <- 1:141 %>% 
    lapply(function(x){load_band_i(x, "../data/Scheffer_2018/Field_hist_k2_v1.nc")}) %>%
    # lapply(function(x){load_band_i(x, "../data/global_vegetation_height_distribution.nc")}) %>% 
    bind_rows() %>% 
    as_tibble() %>% 
    filter(frequency != 0) %>%
    arrange(x,y) %>% 
    group_by(x,y) %>% 
    mutate(cumsum = cumsum(frequency), 
           quantile = cumsum/sum(frequency)) %>%
    slice((min(which(quantile > 0.9))-1):min(which(quantile > 0.9))) %>% 
    summarise(canopy_height = get_percentile(quantile, height), 
              temp=0.9*height[1])
  
  # workaround for cases where total frequency for a given 
  # coordinate is 1 
  precipheight.dataframe$canopy_height[is.na(precipheight.dataframe$canopy_height)] <-
    precipheight.dataframe$temp[is.na(precipheight.dataframe$canopy_height)]
  
  # return value 
  precipheight.dataframe <- select(precipheight.dataframe,
                                   x,
                                   y,
                                   canopy_height)
}

get_precip_df <- function() {
  e <- extent(-180, 180, -60, 60)
  
  precip.dataframe <- TempMapRaster() %>% 
    DownScaleSpatialData() %>%  
    crop(e) %>% 
    raster::as.data.frame(xy=TRUE) %>% 
    as_tibble() %>% 
    rename(mean_annual_precipitation = layer)
}

get_area_type_df <- function() {
  # gather globcover values
  # into a dataframe by mapping a coordinate to an
  # integer representing an area type 
  # 
  # returns: 
  #   globcover data between -60S and 
  #   60N latitude at 0.5 degree resolution, in the form of a dataframe 
  
  e <- extent(-180, 180, -60, 60)
  
  globcover.raster <- raster("../data/Globcover/downscaled/globcover_downscaled.gri") %>% 
    crop(e) %>% 
    raster::as.data.frame(xy=TRUE) %>% 
    as_tibble() %>% 
    mutate(x = as.numeric(format(.$x, digits=2)), 
           y = as.numeric(format(.$y, digits=2))) %>% 
    rename(area_type = GLOBCOVER_200412_200606_V2.2_Global_CLA)
}

get_tree_cover_df <- function() {
  # gather tree cover percentage values 
  # into a dataframe by mapping a coordinate to a
  # respective percentage
  # 
  # returns: 
  #   tree cover data between -60S and 
  #   60N latitude at 0.5 degree resolution, in the form of a dataframe 
  
  e <- extent(-180, 180, -60, 60)
  
  treecover.raster <- raster("../data/treecover/downscaled/treecover_downscaled.gri") %>% 
    crop(e) %>% 
    raster::as.data.frame(xy=TRUE) %>% 
    as_tibble() %>% 
    rename(percentage_tree_cover = layer)
  
}

get_ifl_2013_df <- function() {
  
  ifl_2013 <- rgdal::readOGR("../data/IFL_2013/ifl_2013.shp")
  
  coords <- init_dataframe()
  coords.temp <- init_dataframe()
  
  coordinates(coords) <- ~ x + y
  proj4string(coords) <- CRS(proj4string(ifl_2013))
  
  ifl_2013_df <- over(coords, ifl_2013) %>% 
    is.na() %>% 
    as_tibble() %>% 
    cbind(x = coords.temp$x, 
          y = coords.temp$y) %>% 
    mutate(Intact_Forest_2013_Logical = !(IFL_ID&AREA_HA)) %>% 
    mutate(Intact_Forest_2013 = ifelse(Intact_Forest_2013_Logical==TRUE, "Intact Forest", "Non-Intact Forest")) %>%
    select(x,y, Intact_Forest_2013)
}

load_band_i <- function(i, path) {
  # loads a band identified by value of i
  # from GLAS height netcdf file, then coverts
  # to a dataframe mapping frequency to a 
  # coordinate 
  #
  # returns: 
  #   dataframe mapping coordinate to frequency 
  #   of a single value of height 
  
  heights <- c(0.125, seq(0.5, 70, by=0.5)) 
  e <- extent(-180, 180, -60, 60)
  
  raster_band_i <- raster(path, band=i) %>% 
    crop(e) %>% 
    raster::as.data.frame(xy=TRUE) %>% 
    as_tibble() %>% 
    mutate(height = heights[i]) %>% 
    dplyr::rename("frequency" = "Frequency.distribution.of.Field.height")
}

get_percentile <- function(quantile, height) {
  # linearly interpolates the 90th percentile 
  # given 2 instances of quantile-height pairs
  # that nests the 90th percentile
  #
  # returns: 
  #   linearly interpolated 90th percentile value 
  
  factor <- (0.9-quantile[1])/(quantile[2]-quantile[1])
  diff <- height[2]-height[1]
  value <- (factor*diff)+height[1]
}

custom_colour_scale <- function() {
  p <- c("#0000AA", "#0714fa","#07defa","#07fa17","#FFFF80","#ff5e00","#ff0000","#ff0000")
}

custom_log_density_scale <- function() {
  palette[1:32] <- "#0714fa"
  palette[26:32] <- c("#00ffea","#07fa17","#FFFF80","#ff5e00","#ff0000","#ff0000","#9c0606")
  palette
}

# To be completed 
to_sf_polygon <- function(x, y) { 
  paste()
}

CropOnCommonExtent <- function(rl) {
  # Crops each raster in a list of rasters to 
  # keep consistent extent 
  #
  # Args: 
  #   rl: list of RasterLayers of various extent 
  # 
  # Returns: 
  #   list of RasterLayers of same extent()
  
  # Error handling 
  # (inefficient?) check to see if all elements are class RasterLayer 
  for(i in 1:length(rl)) {
    if(class(rl[[i]]) != "RasterLayer")
      stop() 
  } 
  
  # Get common extent boundaries
  
  # xmin
  xmin.boundary <- max(unlist(lapply(rl, function(x) {xmin(extent(x))}))) 
  
  # xmax
  xmax.boundary <- min(unlist(lapply(rl, function(x) {xmax(extent(x))})))
  
  # ymin
  ymin.boundary <- max(unlist(lapply(rl, function(x) {ymin(extent(x))}))) 
  
  # ymax 
  ymax.boundary <- min(unlist(lapply(rl, function(x) {ymax(extent(x))})))
  
  # create Extent object 
  e <- extent(xmin.boundary, xmax.boundary, ymin.boundary, ymax.boundary)
  
  # return a list of all cropped raster objects 
  rl <- lapply(rl, function(x) {crop(x, e)})
  
} 


DownScaleSpatialData <- function(rl, func="mean") {
  # Downscales a raster to 0.5 degree resolution 
  #
  # Args: 
  #   rl: RasterLayer 
  #   func: function to pass into aggregate -- DEFAULT mean of all cells 
  #
  # Returns: 
  #   Same RasterLayer at 0.5 degree resolution 
  #
  # Assumptions: RasterLayers have res greater than 0.5 degree res
  # AND extent values are in degrees (long/lat)
  # Error handling
  #if(rl@ncols/(xmax(extent(rl))-xmin(extent(rl))) < 2) { # factor 2 (2x2 cells per degree) 
  #  print("Error: resolution is too low")
  #  stop()
  #}
  
  # get factor to pass into aggregate()
  cells.per.degree <- rl@ncols/(xmax(extent(rl))-xmin(extent(rl)))
  factor <- cells.per.degree/2 
  
  # perform downscale and return resultant RasterLayer
  if(factor > 1) # it throws an error if factor is 1 ie. "nothing to aggregate"
    rl <- aggregate(rl, 
                    fact=factor, 
                    fun=func)
  rl
  
}

filter_to_holes <- function(element)  {
  
  is_hole <- lapply(element@Polygons, function(x) x@hole %>% unlist())
  
  element@Polygons <- element@Polygons[is_hole]
  element@plotOrder <- element@plotOrder[is_hole] 
  element@plotOrder <- unlist(lapply(element@plotOrder, function(x) decrement(x)))
  element 
}

remove_holes <- function(element) {
  isnt_hole <- lapply(element@Polygons, function(x) !(x@hole)) %>% unlist() 
  
  element@Polygons <- element@Polygons[isnt_hole]
  element@plotOrder <- element@plotOrder[isnt_hole] 
  element
}

#decrement <- function(x) {

#  if(x != 1) {
#    x <- x-1L
#  }
#  x

#}

reset_index <- function(df) {
  rownames(df) <- 1:nrow(df)
  df
}
