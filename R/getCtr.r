#' Get specified contour coordinates
#' 
#' \code{getCtr} uses \code{hmm.smoother} output to calculate coordinates at a 
#' specified contour of each posterior distribution of the state
#' 
#' @param distr is output array from \code{hmm.smoother}
#' @param tr is output dataframe from \code{calc.track}
#' @param g grid from \code{\link{setup.grid}}
#' @param threshold numeric indicating the percent contour of interest. Default 
#'   is 50 percent.
#' @param makePlot is logical indicating whether or not to plot at each iteration
#'   
#' @return a list of length T, dim(distr)[2], containing 1) coordinates of the 
#'   contour at each time, t. 2) the x and y distance to that contour from the 
#'   mean of the distribution (lat/lon in track). 3) reference coordinate from
#'   mean of distribution. 4) the points of intersection of the contour by which
#'   distance is measured in x and y
#' @export
#' @importFrom grDevices colorRampPalette
#' @importFrom grDevices contourLines
#' @importFrom graphics axis box image layout lines mtext par plot.new points

getCtr <- function(distr, tr, g, threshold = 50, makePlot=FALSE){
  
  p.1 <- apply(distr[1,,,], 1, sum)
  p.2 <- apply(distr[2,,,], 1, sum)
  sv <- -(apply(distr[1,,,], 1, sum) > apply(distr[2,,,], 1, sum)) + 1
  #sv[sv == 0] <- NA
  
  # normalize for each behav at each time point
  norm <- array(NA, dim=dim(distr)[c(3,4,2,1)])
  for (i in 1:dim(norm)[3]){
    norm[,,i,1] <- (distr[1,i,,] / max(distr[1,i,,], na.rm=T)) * p.1[i]
    norm[,,i,2] <- (distr[2,i,,] / max(distr[2,i,,], na.rm=T)) * p.2[i]
  }
  
  pts <- apply(norm, 1:3, FUN=function(x) sum(x, na.rm=T))
  
  threshold <- threshold / 100
  lon <- g$lon[1,]
  lat <- g$lat[,1]
  
  out <- list()
  
  for (i in 1:dim(pts)[3]){
    #print(i)
    ctr <- contourLines(lon, lat, pts[,,i])
    idx <- which.min(lapply(ctr, FUN=function(x) which(round(x$level,1) == round(threshold, 1))) == 1)
    ctr.i <- data.frame(ctr[[idx]])
    sp::coordinates(ctr.i) <- ~x+y
    l1 <- sp::SpatialLines(list(sp::Lines(sp::Line(sp::coordinates(ctr.i)), "L1")))
    
    hl <- data.frame(matrix(c(tr$lon[i] - 10, tr$lon[i], tr$lon[i] + 10, rep(tr$lat[i], 3)), ncol=2))
    names(hl) <- c('x','y')
    sp::coordinates(hl) <- ~x+y
    hl <- sp::SpatialLines(list(sp::Lines(sp::Line(sp::coordinates(hl)), 'hl')))
    
    vl <- data.frame(matrix(c(rep(tr$lon[i], 3), tr$lat[i] - 10, tr$lat[i], tr$lat[i] + 10), ncol=2))
    names(vl) <- c('x','y')
    sp::coordinates(vl) <- ~x+y
    vl <- sp::SpatialLines(list(sp::Lines(sp::Line(sp::coordinates(vl)), 'vl')))
    
    inY <- rgeos::gIntersection(l1, vl)
    inX <- rgeos::gIntersection(l1, hl)
  
    if (is.null(inX) | is.null(inY)){
      out[[i]] <- list(ctr=l1, yDist=NA, xDist=NA, loc=tr[i,], inY=NA, inX=NA)
      
    } else{
      if(makePlot){
        xl <- c(ctr.i@bbox[1,1]-1, ctr.i@bbox[1,2]+1)
        yl <- c(ctr.i@bbox[2,1]-1, ctr.i@bbox[2,2]+1)
        
        fields::image.plot(lon, lat, pts[,,i], xlim=xl, ylim=yl)
        lines(l1)
        lines(hl)
        lines(vl)
        points(tr$lon[i], tr$lat[i])
        points(inY)
        points(inX)
        
        # get user input to proceed
        invisible(readline(prompt="Press [enter] to perform the next iteration and plot"))
      }
  
      if(dim(inY@coords)[1] == 1){
        tr.i <- data.frame(matrix(c(tr$lon[i], tr$lat[i]),ncol=2))
        names(tr.i) <- c('x','y')
        sp::coordinates(tr.i) <- ~x+y
        yDist <- sp::spDists(tr.i, inY)
      } else{
        yDist <- sp::spDists(inY)[1,2] / 2 # Euclidean, in degrees
      }
      
      if(dim(inX@coords)[1] == 1){
        tr.i <- data.frame(matrix(c(tr$lon[i], tr$lat[i]),ncol=2))
        names(tr.i) <- c('x','y')
        sp::coordinates(tr.i) <- ~x+y
        xDist <- sp::spDists(tr.i, inX)
      } else{
        xDist <- sp::spDists(inX)[1,2] / 2 # Euclidean, in degrees
      }
      
      out[[i]] <- list(ctr=l1, yDist=yDist, xDist=xDist, loc=tr[i,], inY=inY, inX=inX)
      
    }
    
  }
  
  return(out)
  
}



