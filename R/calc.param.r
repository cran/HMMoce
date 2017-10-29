#' Calculates movement parameters used for behavior kernels
#' 
#' \code{calc.param} calculates movement parameters used to generate kernels
#' 
#' Movement parameters for each of two behavioral modes are generated using an
#' input grid and user-input speed (m s) for at least the migratory state.
#' 
#' @param migr.spd is numeric indicating movement speed of animal (in m s) while
#'   in migratory behavior state.
#' @param resid.frac is numeric indicating percent of migratory speed that
#'   should be used for resident-like movements. Default is 10\% of migratory
#'   speed.
#' @param g is grid from \code{setup.grid}
#'   
#' @export
#' @return a list of movement parameters

calc.param <- function(migr.spd, resid.frac = 0.1, g){
  
  migr.spd <- migr.spd / 1000 * 3600 * 24 / 111
  dims <- migr.spd / g$dla
  
  resid <- dims * resid.frac
  
  #resid.spd <- resid.spd / 1000 * 3600 * 24 / 111
  #resid <- resid.spd / g$dla
  
  return(list(migr = dims, sig1 = dims, resid = dims, sig2 = resid))
    
}