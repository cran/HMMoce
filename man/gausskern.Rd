% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gausskern.R
\name{gausskern}
\alias{gausskern}
\title{Create Gaussian Kernel}
\usage{
gausskern(siz, sigma, muadv = 0)
}
\arguments{
\item{siz}{size of the kernel, siz x siz. Must be a positive integer.}

\item{sigma}{standard deviation of the kernel. Unit is cell width. Must be a 
positive number.}

\item{muadv}{advection of the kernel. Unit of the input is cell width. 
Defaults to 0.}
}
\value{
Gaussian kernel as a 2D matrix of size (siz x siz)
}
\description{
\code{gausskern} calculates 2D Gaussian kernel based on kernel size, 
deviation, and advection
}
\examples{
kern = gausskern(3, 0.5)

}
\references{
Pedersen, M.W., Righton, D., Thygesen, U.H., Andersen, K.H., and
  Madsen, H. 2008. Geolocation of North Sea cod (Gadus morhua) using hidden
  Markov models and behavioural switching. Canadian Journal of Fisheries and
  Aquatic Sciences 65(11): 2167-1377.
}
\author{
Function originally written for Matlab by Martin W. Pedersen,
  translated to R by Benjamin Galuardi
}
