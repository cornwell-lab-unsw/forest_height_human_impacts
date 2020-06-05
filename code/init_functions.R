# function implentation of all initial functions 
# open tif file and return raster object (using relative paths),
# should work across different platforms  
TiftoRaster <- function(path_to_file) {
  raster(path_to_file) 
}

# open precip files and load them into Mean Annual Precip raster layer.  
# Total MAP should be the addition of rainfall across all months
MapRaster <- function(path_to_folder) { 

  # create list of rasters for opening 
  path <- "/data/wc2/"
  filelist <- list.files(path, full.names=TRUE, pattern=".tif")
  filelist <- lapply(filelist, function(x) {raster(x)}) 
  
  # convert list into a raster stack 
  precip_stack <- stack(filelist, bands=NULL, native=FALSE, RAT=TRUE) 
  
  Map_raster <- calc(precip_stack, sum) 
  # precip_brick <- brick(precip_stack, values=TRUE, 1, filename='')
  # merge all monthly precip data into a single raster layer
  # (sum of each cell). Should result in a map of mean annual
  # precipitation at 1km resolution 
  
  # mean_annual_precip <- stackApply(precip_stack, indices = c(1), fun=sum) 
  
}

# produce and return a  
# lower res mean annual precipitation raster layer 
TempMapRaster <- function()  {

  prec <- getData('worldclim', var="prec", res=10, lon=5, lat=45, path = "data")
  map <- sum(prec)
  
} 
