#' Create Gaussian Kernel
#' 
#' \code{gausskern} calculates 2D Gaussian kernel based on kernel size, 
#' deviation, and advection
#' 
#' @param siz size of the kernel, siz x siz. Must be a positive integer.
#' @param sigma standard deviation of the kernel. Unit is cell width. Must be a 
#'   positive number.
#' @param muadv advection of the kernel. Unit of the input is cell width. 
#'   Defaults to 0.
#' @return Gaussian kernel as a 2D matrix of size (siz x siz)
#' @export
#' 
#' @examples
#' kern = gausskern(3, 0.5)
#' 
#' @references Pedersen, M.W., Righton, D., Thygesen, U.H., Andersen, K.H., and
#'   Madsen, H. 2008. Geolocation of North Sea cod (Gadus morhua) using hidden
#'   Markov models and behavioural switching. Canadian Journal of Fisheries and
#'   Aquatic Sciences 65(11): 2167-1377.
#' @author Function originally written for Matlab by Martin W. Pedersen,
#'   translated to R by Benjamin Galuardi

gausskern <- function(siz, sigma, muadv = 0){
  if(round(siz) < 1) siz = 1
  x = 1:round(siz)
  mu = c(mean(x), mean(x)) + muadv
  #fx = (matrix(exp((-0.5 * (x - mu[1]) / sigma) ^ 2)) / (sqrt(2 * pi) * sigma))
  
  options(digits = 5)
  fx = exp(-.5 * ((x - mu[1]) / sigma) ^ 2) / sqrt((2 * pi) * sigma)
  fy = exp(-.5 * ((x - mu[2]) / sigma) ^ 2) / sqrt((2 * pi) * sigma)
  #fx = exp(.5 * ((x - mu[1]) / sigma)) / sqrt((2 * pi) * sigma)
  #fy = exp(.5 * ((x - mu[2]) / sigma)) / sqrt((2 * pi) * sigma)
  
  fx[!is.finite(fx)] = 0
  #fy = (matrix(exp((-0.5*((x-mu[2])/sigma))^2))/(sqrt(2*pi)*sigma));
  fy[!is.finite(fy)] = 0
  kern = (fx %*% t(fy))
  kern = kern / (sum(kern, na.rm = T))
  kern[is.nan(kern)] = 0
  kern
}
