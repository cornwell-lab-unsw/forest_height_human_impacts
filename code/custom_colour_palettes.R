# custom_colour_palettes.R 

custom_colour_scale <- function() {
  p <- c("#0000AA", "#0714fa","#07defa","#07fa17","#FFFF80","#ff5e00","#ff0000","#ff0000")
}


# interpolating log distribution figure 1b) 
custom_log_density_scale <- function() {
  log_palette <- vector(length = 32L)
  log_palette[1:32] <- "#0714fa"
  log_palette[26:32] <- c("#00ffea","#07fa17","#FFFF80","#ff5e00","#ff0000","#ff0000","#9c0606")
  log_palette
}