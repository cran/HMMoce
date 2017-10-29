#' Download bathymetry data
#' 
#' Download ETOPO bathymetry. Resolution is either 30 second or one minute 
#' resolution
#' 
#' @param lonlow numeric indicating minimum longitude extent of desired download
#'   (-180 to 180).
#' @param lonhigh see lonlow
#' @param latlow see lonlow
#' @param lathigh see lonlow
#' @param folder is destination folder. Default is a temporary directory.
#' @param seaonly is logical indicating whether you want to download only
#'   bathymetry below sea level.
#' @param res is numeric indicating resolution in minutes. Choices currently are 0.5 or 1minute.
#' @param raster is logical indicating whether you want the function to return a
#'   raster or not (a list will be returned).
#' @return Downloads a NetCDF file containing ETopo bathymetry. If raster=TRUE, 
#'   a raster is generated from the downloaded NetCDF. Otherwise, the file is 
#'   just downloaded.
#' @examples
#' \dontrun{
#' # Not run to prevent actual data download
#' sp.lim <- list(lonmin = -82, lonmax = -25, latmin = 15, latmax = 50)
#' bathy <- get.bath.data(sp.lim$lonmin, sp.lim$lonmax, sp.lim$latmin, 
#' sp.lim$latmax, folder = tempdir())
#' }
#' @export
#' @importFrom curl curl_download
#' @note Be patient! The download can take a few minutes!

get.bath.data <- function(lonlow, lonhigh, latlow, lathigh, folder = tempdir(), seaonly = T, res = c(.5), raster=TRUE){

  fname = paste(folder, "bathy.nc", sep = "/")
  if(res==1){
    cat('ERDDAP downloading: Topography, Smith & Sandwell v11.1, 1/60-degree \n UCSD   (Dataset ID: usgsCeSS111)')
    opt = "https://coastwatch.pfeg.noaa.gov/erddap/griddap/usgsCeSS111.nc?topo[(LATHIGH):(LATLOW)][(LONLOW):(LONHIGH)]"
    bathid = 'topo'
  } else if(res == 0.5){
    cat('ERDDAP downloading: Topography, SRTM30+ Version 1.0, 30 arc second, Global \n 	Scripps   (Dataset ID: usgsCeSrtm30v1)')
    opt ="http://coastwatch.pfeg.noaa.gov/erddap/griddap/usgsCeSrtm30v1.nc?topo[(LATHIGH):(LATLOW)][(LONLOW):(LONHIGH)]"
    #opt = 'http://coastwatch.pfeg.noaa.gov/erddap/griddap/etopo180.nc?altitude[(LATLOW):1:(LATHIGH)][(LONLOW):1:(LONHIGH)]'
    bathid = 'topo'
  }
  
  opt <- sub("LATLOW", latlow, opt)
  opt <- sub("LATHIGH", lathigh, opt)
  opt <- sub("LONLOW", lonlow, opt)
  opt <- sub("LONHIGH", lonhigh, opt)
  
  cat(opt)
  curl::curl_download(opt, fname, quiet=FALSE)
  #utils::download.file(opt, fname)
  
  nc <- RNetCDF::open.nc(fname)
  lon <- as.numeric(RNetCDF::var.get.nc(nc, variable = "longitude"))
  lat <- as.numeric(RNetCDF::var.get.nc(nc, variable = "latitude"))
  bdata <- RNetCDF::var.get.nc(nc, variable = bathid)

  lat = lat[order(lat)]
  
  if(seaonly==T) bdata[bdata>=0] = NA
  
  bathy = list(x = lon, y = lat, data = bdata)
  
  if(raster){
    crs <- "+proj=longlat +datum=WGS84 +ellps=WGS84"
    ex <- raster::extent(bathy)
    bathy <- raster::raster(t(bathy$data), xmn = ex[1], xmx = ex[2], ymn = ex[3], ymx = ex[4], crs)
  }
  
  bathy
  
}

