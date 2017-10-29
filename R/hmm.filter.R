#' HMM filter functions
#' 
#' 
#' @param g grid from \code{\link{setup.grid}}
#' @param L is likelihood array output from \code{make.L}
#' @param K1 first movement (diffusion) kernel see \code{\link{gausskern}}
#' @param K2 second movement (diffusion) kernel see \code{\link{gausskern}}
#' @param P 2x2 probability matrix for transitions between states (K1 and K2)
#' @param maskL is logical indicating whether to mask the input L layer. See
#'   \code{mask.L} for details.
#' @param bound.thr is numeric indicating the percent threshold that is added 
#'   and subtracted from the bounding box of the filter output from the 
#'   previous day before masking. Default is .05 (5 percent).
#' @param minBounds is size (in degrees) of the minimum bounding box around the 
#'   previous days filter prediction that L data within that box will be 
#'   included. Outside this box (centered on t-1 filter prediction), L will be 
#'   masked out.
#'   
#' @return a list: list(phi = phi, pred = pred, psi = psi) where \itemize{ \item
#'   phi. is the probability for each state at each time step \item pred. is
#'   .... \item psi. is.... }
#' @references Pedersen MW, Patterson TA, Thygesen UH, Madsen H (2011)
#'   Estimating animal behavior and residency from movement data. Oikos
#'   120:1281-1290. doi: 10.1111/j.1600-0706.2011.19044.x
#' @export
#' @examples 
#' \dontrun{
#' # Not run as function relies on large arrays of likelihoods
#' # RUN THE FILTER STEP
#' f <- hmm.filter(g, L, K1, K2, maskL=T, P.final, minBounds = bnd)
#' nllf <- -sum(log(f$psi[f$psi>0])) # negative log-likelihood
#' 
#' }
#' 

hmm.filter <- function(g, L, K1, K2, P, maskL = T, bound.thr = 0.1, minBounds = 10){
  
  ## Filter data to estimate locations and behaviour
  
  T <- dim(L)[1] # dimension of time 
  row <- dim(g$lon)[1] # nrows
  col <- dim(g$lon)[2] # ncols
  m <- 2 # Number of behavioural states
  
  pred <- array(0, dim = c(m, T, col, row)) # empty array for prediction step. ordering in col before row emulates lon before lat
  phi  <- array(0, dim = c(m, T, col, row)) # posterior (final) step array
  
  # Start in resident state at the known initial location
  #phi[1,1,,]  <- L[1,,] # first position is known
  phi[2,1,,]  <- L[1,,] # first position is known
  #pred[1,1,,] <- L[1,,] # first position is known
  pred[2,1,,] <- L[1,,] # first position is known
  psi <- rep(0, T - 1) # sum of the probability of both states at each step
  
  # convert movement kernels from matrix to cimg for convolution
  K1 <- imager::as.cimg(K1)
  K2 <- imager::as.cimg(K2)
  
  # Start filter iterations
  for(t in 2:T){
    
    # convolve previous day's likelihood with movement kernels
    p1 = imager::as.cimg(t(phi[1, t-1,,]))
    p2 = imager::as.cimg(t(phi[2, t-1,,]))
    q1 = imager::convolve(p1, K1)
    q2 = imager::convolve(p2, K2)
    q1 = t(as.matrix(q1))
    q2 = t(as.matrix(q2))
    
    # multiply by transition probability 
    pred[1,t,,] <- P[1,1] * q1 + P[2,1] * q2
    pred[2,t,,] <- P[1,2] * q1 + P[2,2] * q2
    
    # is there a data-based likelihood observation for this day, t?
    sumL = sum(L[t,,])  
    if(sumL > 1e-6){
      if(maskL){
        post1 <- mask.L(pred.t = pred[1,t,,], L.t = L[t,,], lon = g$lon[1,], lat = g$lat[,1], par0 = dim(K1)[1], bound.thr = bound.thr, minBounds=minBounds)
        post2 <- mask.L(pred.t = pred[2,t,,], L.t = L[t,,], lon = g$lon[1,], lat = g$lat[,1], par0 = dim(K1)[1], bound.thr = bound.thr, minBounds=minBounds)
      } else{
        post1 <- pred[1,t,,] * L[t,,]
        post2 <- pred[2,t,,] * L[t,,]
      }
    }else{
      post1 <- pred[1,t,,]
      post2 <- pred[2,t,,]
    }
    
    psi[t-1] <- sum(as.vector(post1), na.rm=T) + sum(as.vector(post2), na.rm=T)
    
    phi[1,t,,] <- post1 / (psi[t-1] + 1e-15)
    phi[2,t,,] <- post2 / (psi[t-1] + 1e-15)
    
  }
  
  list(phi = phi, pred = pred, psi = psi)
  
}