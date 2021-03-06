% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc.hycom.r
\name{calc.hycom}
\alias{calc.hycom}
\title{Hycom Profile LIkelihood}
\usage{
calc.hycom(pdt, filename, hycom.dir, focalDim = 9, dateVec, use.se = TRUE)
}
\arguments{
\item{pdt}{input PDT data output from \code{\link{read.wc}} and
\code{\link{extract.pdt}}}

\item{filename}{is the first part of the filename specified to the download 
function \code{\link{get.env}}. For example, if downloaded files were 
specific to a particular dataset, you may want to identify that with a name
like 'tuna' or 'shark1'. This results in a downloaded filename of, for 
example, 'tuna_date.nc'. This filename is required here so the calc 
function knows where to get the env data.}

\item{hycom.dir}{directory of downloaded hycom (or other) data}

\item{focalDim}{is integer for dimensions of raster::focal used to calculate 
sd() of temperature grid cell. Recommend focalDim = 9 for Hycom data at 
0.08deg resolution.}

\item{dateVec}{vector of complete dates (from tag to pop by day) for data 
range. This should be in 'Date' format}

\item{use.se}{is logical indicating whether or not to use SE when using 
regression to predict temperature at specific depth levels.}
}
\value{
a raster brick of Hycom profile likelihood
}
\description{
Calculate Hycom profile likelihood surface
}
\seealso{
\code{\link{calc.hycom.par}}
}
