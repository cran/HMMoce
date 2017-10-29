#' \code{makePar} gets parameters for subsequent filter/smoother
#' 
#' Function builds movement kernels for 2 different behavior states and
#' calculates, if desired, switching probability using an expectation
#' maximization routine
#' 
#' @param migr.spd is numeric input to \code{calc.param}
#' @param grid is a grid output by \code{resample.grid} that corresponds to the
#'   extent and resolution of L.arr (below).
#' @param L.arr is the likelihood array used for state switch probability
#'   calculation (see \code{expmax}). This is typically the L.mle array returned
#'   from \code{make.L} because it's typically more coarse (and thus faster)
#'   than the higher-resolution L array.
#' @param p.guess is vector of length 2 indicating probability of staying in
#'   states 1 and 2, respectively
#' @param calcP is logical indicating whether to use \code{expmax} to calculate
#'   state-switching probabilities
#'   
#' @return list of parameters including movement kernels (K1, K2) and switch
#'   probability (P.final)
#' @examples 
#' \dontrun{
#' par0 <- makePar(migr.spd=2, grid=g.mle, L.arr=L.mle, p.guess=c(.9,.9), calcP=T)
#' }
#'       
#' @export
#' 

makePar <- function(migr.spd, grid, L.arr, p.guess = c(0.7, 0.8), calcP=FALSE){
  # PROVIDE FIXED KERNEL PARAMETERS
  par0 <- calc.param(migr.spd = migr.spd, g = grid)
  D1 <- unlist(par0[1:2]) # parameters for kernel 1. this is migratory behavior mode
  D2 <- unlist(par0[3:4]) # parameters for kernel 2. resident behavior mode
  
  # GENERATE MOVEMENT KERNELS. D VALUES ARE MEAN AND SD PIXELS
  K1 <- gausskern(D1[1], D1[2], muadv = 0)
  K2 <- gausskern(D2[1], D2[2], muadv = 0)
  
  # RUN EXPECTATION-MAXIMIZATION ROUTINE FOR MATRIX, P (STATE SWITCH PROBABILITY)
  
  if(calcP){
    P.final <- expmax(p.init=p.guess, g = grid, L = L.arr, K1, K2, save = T)
    save.p <- P.final[[2]]; P.final <- P.final[[1]]
    
    if (any(P.final == 1)) warning('Calculated switching probabilities equal 1. This usually indicates problems with this specification of the model. Try using a different set of likelihoods as input.')
    
  } else{
    P.final <- matrix(c(p.guess[1], 1 - p.guess[1], 1 - p.guess[2], p.guess[2]), 2, 2, byrow = TRUE)

  }
  
  return(list(K1 = K1, K2 = K2, P.final = P.final))
}

