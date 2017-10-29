#' Expectation-maximization framework for state-switching
#' 
#' \code{expmax} performs expectation-maximization for state switching 
#' probability
#' 
#' Light errors are parameterized using elliptical error values output in.
#' 
#' @param p.init a vector of length 2. The first is the probability of staying 
#'   in behavior state 1 if currently in state 1 and the second for staying in 
#'   state 2.
#' @param g grid from \code{\link{setup.grid}}
#' @param L final likelihood
#' @param K1 first movement (diffusion) kernel see \code{\link{gausskern}}
#' @param K2 second movement (diffusion) kernel see \code{\link{gausskern}}
#' @param niter is integer that determines number of iterations to perform
#' @param threshold is threshold of percent change that we consider satisfactory
#'   for convergence. Default is 1\%.
#' @param save is logical indicating whether the function should save and return
#'   a 2 col dataframe of the iterations it went through before crossing the
#'   convergence threshold. Defaults to not saving.
#'   
#' @return a 2x2 matrix of state switching probabilities. See P.init input for 
#'   more information.
#' @export
#' @references Woillez M, Fablet R, Ngo TT, et al. (2016) A HMM-based model to 
#'   geolocate pelagic fish from high-resolution individual temperature and 
#'   depth histories: European sea bass as a case study. Ecol Modell 321:10-22.
#' @examples 
#' 
#' # GENERATE MOVEMENT KERNELS. D VALUES ARE MEAN AND SD PIXELS
#' K1 <- gausskern(3, 1, muadv = 0)
#' K2 <- gausskern(10, 5, muadv = 0)
#' 
#' # MAKE A GUESS AT STATE SWITCHING PROBABILITY
#' # probability of staying in state 1 and 2, respectively
#' p.init <- c(0.7, 0.8)
#' 
#' \dontrun{
#' # Not run as it relies on L, a large likelihood grid
#' # RUN EXPECTATION-MAXIMIZATION ROUTINE FOR MATRIX, P (STATE SWITCH PROBABILITY)
#' P.final <- expmax(p.init, g = g, L = L, K1, K2)
#' 
#' }

expmax <- function(p.init, g, L, K1, K2, niter = 1000, threshold = .01, save = F){
  
  t1 <- Sys.time()
  options(warn = -1)
  
  print(paste('Starting EM for state switching...'))
  
  p.init <- matrix(c(p.init[1], 1 - p.init[1], 1 - p.init[2], p.init[2]), 2, 2, byrow = TRUE)
  
  if (niter < 25){
    stop('Maximum number of iterations (niter) must be > 25.')
  }
  
  if (class(K1) == 'matrix'){
    # convert movement kernels from matrix to cimg for convolution
    K1 <- imager::as.cimg(K1)
    K2 <- imager::as.cimg(K2)
    
  }

  save.p <- data.frame(matrix(NA, ncol = 2, nrow = niter))
  save.p[,1] <- p.init[1,1]; save.p[,2] <- p.init[2,2]

  # create progress bar
  pb <- utils::txtProgressBar(min = 1, max = niter, style = 3)
  
  for (i in 1:niter){
    
    # update progress bar
    Sys.sleep(0.001)
    utils::setTxtProgressBar(pb, i)
    
    if (i == 1){
      P <- p.init
    }
    
    # RUN THE FILTER STEP
    f <- hmm.filter(g, L, K1, K2, P=p.init, maskL=F)
    
    # RUN THE SMOOTHING STEP
    s <- hmm.smoother(f, K1, K2, L=L, P=p.init)
    
    #------------------------#
    # UPDATE P
    # get states from s
    sv <- apply(s[1,,,], 1, sum) > apply(s[2,,,], 1, sum)
    sv <- -sv + 2
    
    # difference it to find transitions
    svd <- rbind(sv, c(0, diff(sv)))
    
    # get a new transition probability matrix
    r1 <- rev(table(svd[2, svd[1,] == 1]) / sum(svd[1,] == 1))
    r2 <- rev(table(svd[2, svd[1,] == 2]) / sum(svd[1,] == 2))
    P <- matrix(rbind(r1, r2), 2, 2)
    
    # CALCULATE CHANGE IN PARAMETER VALUES OVER LAST SEVERAL ITERATIONS
    # We use "ratio betwen the average over the last 20 values of D and the new value
    # of D below 1%" (Woillez 2016)
    
    save.p[i,] <- c(P[1,1], P[2,2])

    if (i > 10){
      thr1 <- (save.p[i,1] - mean(save.p[i:(i-10),1])) / mean(save.p[i:(i-10),1])
      thr2 <- (save.p[i,2] - mean(save.p[i:(i-10),2])) / mean(save.p[i:(i-10),2])
    }
    
    # CHECK IF THRESHOLD IS MET. IF NOT, RE-ITERATE
    if(exists('thr1')){
      if (thr1 <= threshold & thr2 <= threshold){
        t2 <- Sys.time()
        print(paste('Convergence took ', round(as.numeric(difftime(t2, t1, units='mins')), 2),' minutes...',sep=''))
        break
      } else if (i == niter){
        stop(paste('Maximum iterations reached (', niter, ') without crossing percent change threshold...', sep=''))
      }
    }
    
    if (i == niter){
      stop('Error: expmax did not converge below change threshold.')
    }
  
  }
  
  options(warn = 0)
  
  if (save){
    return(list(P, save.p))
  } else{
    return(P)
  }
  
}

