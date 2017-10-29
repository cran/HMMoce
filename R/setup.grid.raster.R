#' Setup the discrete spatial grid for the HMM
#' 
#' @param grid.ras is a raster for which a grid is desired
#'   
#' @return a list
#' @export
#'   

setup.grid.raster <- function(grid.ras){
  
  ex <- raster::extent(grid.ras)
  
  # Find longitude extents
  il <- floor(ex[1])
  al <- ceiling(ex[2])
  lx <- 0.1 * (al - il)
  lonl <- il - lx
  lonu <- al + lx
  
  # Find latitude extents
  ila <- floor(ex[3])
  ala <- ceiling(ex[4])
  ly <- 0.1 * (ala - ila)
  latl <- ila - ly
  latu <- ala + ly
  
  # Create grid
  lo <- raster::xFromCol(grid.ras)
  la <- rev(raster::yFromRow(grid.ras))
  g <- meshgrid(lo, la)
  dlo <- raster::xres(grid.ras)
  dla <- raster::yres(grid.ras)
  
  list(lon = g$X, lat = g$Y, dlo = dlo, dla = dla)
  
}