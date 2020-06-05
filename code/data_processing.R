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
    drop_na(globcover_numeric) %>%
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
  
  height_dataframe <- 1:141 %>% 
    lapply(function(x){load_band_i(x, "data/Scheffer_2018/Field_hist_k2_v1.nc")}) %>%
    # lapply(function(x){load_band_i(x, "data/global_vegetation_height_distribution.nc")}) %>% 
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
  height_dataframe$canopy_height[is.na(height_dataframe$canopy_height)] <-
    height_dataframe$temp[is.na(height_dataframe$canopy_height)]
  
  # return value 
  height_dataframe <- dplyr::select(height_dataframe, x, y, canopy_height)
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
  
  # Load label data 
  globcover.legend <- read_xls("data/Globcover/Globcover_V2.2_Global/Globcover_Legend.xls")
  
  # Load geospatial data convert to a data frame  
  globcover.raster <- raster("data/Globcover/downscaled/globcover_downscaled.gri") %>% 
    crop(e) %>% 
    raster::as.data.frame(xy=TRUE) %>% 
    as_tibble() %>% 
    mutate(x = as.numeric(format(.$x, digits=2)), 
           y = as.numeric(format(.$y, digits=2))) %>% 
    left_join(dplyr::select(globcover.legend, Value, Label), 
              by= c("GLOBCOVER_200412_200606_V2.2_Global_CLA" = "Value"),
              all.x = TRUE) %>% 
    rename(globcover_numeric = GLOBCOVER_200412_200606_V2.2_Global_CLA, 
           globcover_label = Label)
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
  
  treecover.raster <- raster("data/treecover/downscaled/treecover_downscaled.gri") %>% 
    crop(e) %>% 
    raster::as.data.frame(xy=TRUE) %>% 
    as_tibble() %>% 
    rename(percentage_tree_cover = layer)
  
}

get_ifl_2013_df <- function() {
  
  ifl_2013 <- rgdal::readOGR("data/IFL_2013/ifl_2013.shp")
  
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
    mutate(intact_forest_2013 = ifelse(Intact_Forest_2013_Logical==TRUE, "Intact Forest", "Non-Intact Forest")) %>%
    dplyr::select(x,y, intact_forest_2013)
}

height_growth_modelling <- function() {
  data<-read.csv("data/BAAD/baad_with_map.csv",stringsAsFactors = FALSE)
  
  # plot height vs age for trees < 200 years old
  data %>% 
    filter(age < 201) %>% 
    group_by(location) %>% 
    filter(max(age) > 10) %>% 
    filter(!is.na(mean_annual_precipitation))%>%
    ungroup()->smallish
  
  # random slope for each site
  m <- lmer(h.t~0+(0+age|location),data=smallish)
  # random term for species nested within site
  m2 <- lmer(h.t~0+(0+age|location/species),data=smallish)
  
  slopes<-coef(m2)$location
  
  
  ggplot(slopes,aes(x=age))+geom_histogram()+xlab("growth rate (m/yr)")
  ggplot(slopes,aes(x=20/age))+geom_histogram()+xlab("time to 20m tall (years)")+scale_x_log10()
  
  data.fitted <- broom::augment(m2) %>% arrange(location, age)
  
  ggplot(data.fitted, aes(age, h.t,colour=species)) + geom_point() + 
    geom_line(aes(age, .fitted,colour=species)) + 
    facet_wrap(~location)+theme(legend.position = "none")
  
  data.fitted$map<-data$mean_annual_precipitation[match(data.fitted$location,data$location)]
  
  data.fitted$growth.rate<-data.fitted$.fitted/data.fitted$age
  
  data.fitted %>%
    group_by(location,species)%>%
    summarize(map=mean(map),growth.rate=mean(growth.rate))->species_df
  
  
  plot <- ggplot(species_df,aes(x=map,y=growth.rate))+geom_point()+geom_smooth(method="lm")+theme_bw()+
    ggtitle("Climate Effects on Growth Rate of Trees") + 
    ylab("growth rate (m/year)") + 
    xlab("mean annual precipitation (mm)")
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

extract_raster_data <- function(raster, longitude, latitude) { 
  point <- SpatialPoints(data.frame(longitude = longitude, latitude = latitude),  
                         proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) 
  val <- raster::extract(raster, point) 
}

reset_index <- function(df) {
  rownames(df) <- 1:nrow(df)
  df
}

convert_pts_to_sfc <- function(coords_df, protected_layer) {
  pts_sfc <- st_as_sfc(unlist(coords_df$sf_formatted_pts)) %>% 
    st_sf(ID = paste0(1:length(coords_df$sf_formatted_pts)))
  
  st_crs(pts_sfc) <- st_crs(protected_layer)
  pts_sfc
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

intermediate_exclusion_filter <- function(data) {
  all_vegetation_filter_2 = c(40, 50, 60, 70, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 220)
  all_vegetation_2 <- data[data$globcover_numeric %in% all_vegetation_filter_2 ,]
}

strict_exclusion_filter <- function(data) {
  all_vegetation_filter_3 <- c(40, 50, 60, 70, 90, 100, 110, 120, 130, 140, 150)
  all_vegetation_3 <- data[data$globcover_numeric %in% all_vegetation_filter_3 ,]
}


get_precip_height_df <- function() {
  
  
  height.dataframe <- get_height_df()
  
  areaprecip.dataframe <- get_area_precip_df()
  
  # perform a left join from our globcover
  # precipitation dataframe onto our tree modes using 
  # coordinates as our key + filter out artificial, bare, 
  # and water areas 
  processed.dataframe <- merge(x = height.dataframe,
                               y = areaprecip.dataframe,
                               by = c("x","y"), 
                               all.x=TRUE) %>% 
    drop_na(canopy_height, mean_annual_precipitation, area_type) %>%
    filter(area_type != 190,
           area_type != 200, 
           area_type != 210,
           area_type != 220,
           area_type != 11,
           area_type != 14,
           area_type != 20, 
           area_type != 30,
           area_type != 230)
  
}

get_area_precip_df <- function() {
  # get a dataframe mapping a coordinate to 
  # to its mean annual precipitation and globcover
  # value (ocean, artificial land, areas of sparse vegetation)
  
  # Load files 
  globcover.raster <- raster("data/Globcover/downscaled/globcover_downscaled.gri")
  precip.raster <- TempMapRaster() %>% 
    DownScaleSpatialData()
  
  datasets <- list(crop(globcover.raster, extent(-180, 180, -60, 60)),
                   crop(precip.raster, extent(-180, 180, -60, 60)))
  
  # create a dataframe mapping a coordinate to 
  # to its mean annual precipitation and globcover
  # value (ocean, artificial land, areas of sparse vegetation)
  areaprecip.dataframe <- datasets %>% 
    stack(bands=NULL, native=FALSE, RAT=TRUE) %>% 
    raster::as.data.frame(xy = TRUE) %>% 
    as_tibble() %>% 
    dplyr::rename("mean_annual_precipitation" = "layer",
                  "area_type" = "GLOBCOVER_200412_200606_V2.2_Global_CLA") %>% 
    mutate(x = as.numeric(format(.$x, digits=2)), 
           y = as.numeric(format(.$y, digits=2))) %>% 
    drop_na(mean_annual_precipitation)
}


get_cover_height_df <- function() {
  # get dataframe mapping tree height value 
  # to a tree cover percentage per 
  # grid cell 
  #
  # returns: 
  #   dataframe mapping coordinates to height-cover 
  # pair
  
  # height data 
  height.dataframe <- get_height_df() 
  
  # MODIS vegetation cover data 
  treecover.dataframe <- get_tree_cover_df()
  
  return.dataframe <- merge(x = height.dataframe,
                            y = treecover.dataframe, 
                            by = c("x","y"),
                            all.x=TRUE)
}
