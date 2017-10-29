#' Download GHR Sea Surface Temperature (SST) data
#' 
#' \code{get.ghr.sst} downloads sea surface temperature (SST) data for given
#' temporal and spatial constraints of your data.
#' 
#' The method may return before the download is completed. It will continue to 
#' display progress  until the download completes. Change the default 
#' \code{download.file} if the download is failing on your platform.
#' 
#' @param limits A list of length 4; minlon, maxlon, minlat, maxlat. Longitude values are -180,180
#' @param time A vector of length 2 with the minimum and maximum times in form 
#'   \code{as.Date(date)}.
#' @param filename An optional filename. If provided, then the data is 
#'   downloaded to that file. Otherwise the data is not downloaded and the url 
#'   is returned.
#' @param download.file Logical. Should use the default \code{download.file} to 
#'   query the server and download or use the optional \code{curl}. Some users 
#'   may need to use \code{curl} in order to get this to work.
#' @param dir is local directory where ncdf files should be downloaded to. 
#'   default is current working directory. if enter a directory that doesnt 
#'   exist, it will be created.
#' @return The url used to extract the requested data from the NetCDF subset 
#'   service.
#' @importFrom curl curl_download
#'   
#' @author   Function originally written for R by Ben Jones (WHOI) and modified 
#'   by Camrin Braun and Ben Galuardi.
#' @references \url{https://www.ncdc.noaa.gov/oisst}
#'   

get.ghr.sst <- function(limits, time, filename='', download.file=TRUE, dir = getwd()) {
  
  options(warn = -1)
  
  dir.create(file.path(dir), recursive = TRUE, showWarnings = FALSE)
  setwd(dir)
  
  ## Set the base URL based on the start date. If the ending date exceeds the
  ## period for this experiment, then print a warning and truncate the output
  ## early.
  
  expts = data.frame(
    start=c(as.Date('2010-06-09')),
    end=c(Sys.Date() + 1),
    url=c('http://upwell.pfeg.noaa.gov/erddap/griddap/jplG1SST.nc?SST')
    )
  
  if(time[1] < expts$start[1])
    stop('Data begins at %s and is not available at %s.',
         strftime(expts$start[1], '%d %b %Y'),
         strftime(time[1], '%d %b %Y'))
  if(time[1] > expts$end[nrow(expts)])
    stop('Data ends at %s and is not available at %s.',
         strftime(expts$end[nrow(expts)], '%d %b %Y'),
         strftime(time[1], '%d %b %Y'))
  for(i in seq(nrow(expts))) {
    if((time[1] >= expts$start[i]) & (time[1] <= expts$end[i]))
      url = expts$url[i]
  }
  
  ## Add the time domain.
  if(length(time) == 2){
    url = sprintf('%s[(%s:00:00Z):1:(%s:00:00Z)]',
                  url, strftime(time[1], '%Y-%m-%dT00'),
                  strftime(time[2], '%Y-%m-%dT00'))
  } else if(length(time) == 1){
    url = sprintf('%s[(%s:00:00Z):1:(%s:00:00Z)]',
                  url, strftime(time[1], '%Y-%m-%dT00'),
                  strftime(time[1], '%Y-%m-%dT00'))
  }
  
  ## Add the spatial domain.
  url = sprintf('%s[(%s):1:(%s)][(%s):1:(%s)]',
                url, limits[[3]], limits[[4]], limits[[1]], limits[[2]])
  print(url)
  
  ## Download the data if a filename was provided.
  if(filename != ''){
    if(download.file == TRUE){
      #download.file(url, filename, method = 'auto')
      curl_download(url, filename, quiet=FALSE)
    } else if(download.file == FALSE){
      system(sprintf('curl -o "%s" "%s"', filename, url))
    }
  }
  
  options(warn = 0)
  
  return(url)
}

