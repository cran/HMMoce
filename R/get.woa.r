#' Download World Ocean Atlas Climatology
#' 
#' \code{get.woa} downloads World Ocean Atlas 2013 mean climatological
#' temperature data from GitHub
#' 
#' @param save.dir is the directory to save the downloaded data to
#' @param resol is character describing the desired resolution in degrees. Choices are 'one' or 'quarter'.
#'   
#' @return name and directory of the downloaded data

get.woa <- function(save.dir = getwd(), resol = 'one'){

  if (resol == 'one'){
    url <- 'https://github.com/camrinbraun/camrinbraun.github.io/blob/master/woa.one.rda?raw=true'
    filename <- paste(save.dir, '/woa.one.rda', sep='')
  } else if (resol == 'quarter'){
    url <- 'https://github.com/camrinbraun/camrinbraun.github.io/blob/master/woa.quarter.rda?raw=true'
    filename <- paste(save.dir, '/woa.quarter.rda', sep='')
  }

  ## Download the data
  utils::download.file(url, filename, method = 'auto')
  
  return(filename)
  
}
