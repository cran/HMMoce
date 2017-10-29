#' Combine individual source likelihoods
#' 
#' \code{make.L} combines individual likelihoods from various data sources (e.g.
#' SST, OHC) to make overall combined likelihoods for each time point
#' 
#' @param L1 a likelihood array
#' @param L2 a likelihood array
#' @param L3 a likelihood array
#' @param known.locs is data frame of known locations containing named columns 
#'   of date, lon, lat. Default is NULL.
#' @param L.mle.res is a coarse resolution array of dim(L1) that speeds up the 
#'   parameter estimation step later on
#' @param dateVec is vector of dates from tag to pop-up date by day. Only
#'   required if known.locs is not NULL.
#' @param locs.grid is output grid from \code{setup.locs.grid}. Only required if
#'   known.locs is not NULL.
#' @param iniloc is matrix of tag and pop locations. Default is NULL because
#'   this should be taken care of elsewhere.
#' @param bathy is bathymetry raster (likely from ETOPO1) as acquired by
#'   \code{get.bath.data}
#' @param pdt is data frame output from read.wc(type='pdt')

#' @return a list containing: L, the overall likelihood array and L.mle, a more 
#'   coarse version of L used later for parameter estimation
#' @export
#' @note This function currently only supports the use of 3 input likelihood 
#'   data sources. This will be expanded in the future based on user needs.
#'   

make.L <- function(L1, L2 = NULL, L3 = NULL, known.locs = NULL, L.mle.res, dateVec = NULL,
                       locs.grid = NULL, iniloc = NULL, bathy=NULL, pdt=NULL){

  if(class(L1) == 'list'){
    if(length(L1) == 3){
      L3 <- L1[[3]]; L2 <- L1[[2]]; L1 <- L1[[1]]
    } else if (length(L1) == 2){
      L2 <- L1[[2]]; L1 <- L1[[1]]
    }
  }
    
  if(!is.null(bathy) & is.null(pdt)) stop('Error: if bathy is not NULL then a maxDep vector must be supplied.')
  
  if(!is.null(known.locs)){
    print('Input known locations are being used...')
    # convert input date, lat, lon to likelihood surfaces with dim(L1)
    L.locs <- L1 * 0
    known.locs$date <- as.Date(known.locs$date)
    
    if(is.null(dateVec)){stop('Error: dateVec is null.')}
    if(is.null(locs.grid)){stop('Error: locs.grid is null.')}
    
    # need lat/lon vectors from locs.grid
    lon <- locs.grid$lon[1,]
    lat <- locs.grid$lat[,1]
    
    kn.idx <- which(dateVec %in% known.locs$date)
    for(i in kn.idx){
      known.locs.i <- known.locs[which(known.locs$date %in% dateVec[i]),]
      
      if(length(known.locs.i[,1]) > 1){
        # if multiple known locations are provided for a given day, only the first is used
        known.locs.i <- known.locs.i[1,]
      }
      
      x = which.min((known.locs.i$lon - lon) ^ 2)
      y = which.min((known.locs.i$lat - lat) ^ 2)

      # assign the known location for this day, i, as 1 (known) in likelihood raster
      L.locs[[i]][raster::cellFromXY(L.locs[[kn.idx]], known.locs.i[,c(2,3)])] <- 1
      
    }
    
  }
  
  if(is.null(L2) & is.null(L3)){
    print('One likelihood raster has been specified...')
    
    L <- L1
    
    # GET RID OF NA VALUES
    #L1[is.na(L1)] <- 0
    
    # ALL CELLS IN A LIKELIHOOD SURFACE == 0?
    #naLidx = raster::cellStats(L1, sum, na.rm=T) != 0
    
    #L <- L1[naLidx]
    
  } else if(!is.null(L2) & is.null(L3)){
    print('Two likelihood rasters have been specified...')
    
    # MAKE AN ARRAY OF ZEROS
    L <- L1 * 0
    
    # GET RID OF NA VALUES
    L1[is.na(L1)] <- 0
    L2[is.na(L2)] <- 0
    L1[is.infinite(L1)] <- 0
    L2[is.infinite(L2)] <- 0

    # ALL CELLS IN A LIKELIHOOD SURFACE == 0?
    naL1idx = raster::cellStats(L1, sum, na.rm=T) != 0
    naL2idx = raster::cellStats(L2, sum, na.rm=T) != 0
    
    # WHERE BOTH ARE ZEROS. THESE WILL BE INTERPOLTED IN THE FILTER
    naLidx = naL1idx + naL2idx
    
    # where naLidx==0, both likelihoods are zero
    #       naLidx==1, one has data
    #       naLidx==2, both have data
    idx1 = which(naLidx == 1)
    idx2 = which(naLidx == 2)
    
    # INPUTS GET ADDED/MULTIP TOGETHER FOR FINAL L
    for(ii in idx1){
      L[[ii]] = L1[[ii]] + L2[[ii]] # when only 1 has data
    }
    
    for(ii in idx2){
      L[[ii]] = L1[[ii]] * L2[[ii]] # when both have data
      
      if (raster::cellStats(L[[ii]], sum, na.rm=T) == 0){
        L[[ii]] = L2[[ii]] # when only 1 has data
      }

    }

  } else if(!is.null(L2) & !is.null(L3)){
    print('Three likelihood rasters have been specified...')
    
    # MAKE AN ARRAY OF ZEROS
    L <- L1 * 0
    
    # GET RID OF NA VALUES
    L1[is.na(L1)] <- 0
    L2[is.na(L2)] <- 0
    L3[is.na(L3)] <- 0
    L1[is.infinite(L1)] <- 0
    L2[is.infinite(L2)] <- 0
    L3[is.infinite(L3)] <- 0

    # ALL CELLS IN A LIKELIHOOD SURFACE == 0?
    naL1idx = raster::cellStats(L1, sum, na.rm=T) != 0
    naL2idx = raster::cellStats(L2, sum, na.rm=T) != 0
    naL3idx = raster::cellStats(L3, sum, na.rm=T) != 0
    
    # WHERE ALL ARE ZEROS. THESE WILL BE INTERPOLTED IN THE FILTER
    naLidx = naL1idx + naL2idx + naL3idx
    
    # where naLidx==0, both likelihoods are zero
    #       naLidx==1, one has data
    #       naLidx==2, both have data
    idx1 = which(naLidx == 1)
    idx2 = which(naLidx == 2)
    idx3 = which(naLidx == 3)
    
    # COMBINING LIKELIHOODS
    for(ii in idx1){
      L[[ii]] = L1[[ii]] + L2[[ii]] + L3[[ii]]# when only 1 has data
    }
    
    for(ii in idx3){
      L[[ii]] = L1[[ii]] * L2[[ii]] * L3[[ii]] # when all have data
      
      if (raster::cellStats(L[[ii]], sum, na.rm=T) == 0 & naL1idx[ii] & naL2idx[ii]){
        # revert to idx2 and need to figure out which to use
        L[[ii]] <- L1[[ii]] * L2[[ii]]
      }
      
      if(raster::cellStats(L[[ii]], sum, na.rm=T) == 0 & naL1idx[ii] & naL3idx[ii]){
        L[[ii]] <- L1[[ii]] * L3[[ii]]
      }
      
      if(raster::cellStats(L[[ii]], sum, na.rm=T) == 0 & naL2idx[ii] & naL3idx[ii]){
        L[[ii]] <- L2[[ii]] * L3[[ii]]
      }
      
      if (raster::cellStats(L[[ii]], sum, na.rm=T) == 0){
        # revert to idx1 if the 2 likelihoods we are using do not overlap and result in all 0's
        L[[ii]] = L1[[ii]] + L2[[ii]] + L3[[ii]]# when only 1 has data
      }
      
    }
    
    for(ii in idx2){
      if(raster::cellStats(L[[ii]], sum, na.rm=T) == 0 & naL1idx[ii] & naL2idx[ii]){
        L[[ii]] <- L1[[ii]] * L2[[ii]]
      } 
      
      if(raster::cellStats(L[[ii]], sum, na.rm=T) == 0 & naL1idx[ii] & naL3idx[ii]){
        L[[ii]] <- L1[[ii]] * L3[[ii]]
      }
      
      if(raster::cellStats(L[[ii]], sum, na.rm=T) == 0 & naL2idx[ii] & naL3idx[ii]){
        L[[ii]] <- L2[[ii]] * L3[[ii]]
      }
      
      if (raster::cellStats(L[[ii]], sum, na.rm=T) == 0){
        # revert to idx1 if the 2 likelihoods we are using do not overlap and result in all 0's
        L[[ii]] = L1[[ii]] + L2[[ii]] + L3[[ii]]# when only 1 has data
        
      }
      
    }
    
    
    
  }
  
  # normalize
  sumIdx <- which(raster::cellStats(L, sum, na.rm = T) != 0)
  for (i in sumIdx){
    L[[i]] <- L[[i]] / (raster::cellStats(L[[i]], max, na.rm = T) * 1.25)
  }
  
  if(!is.null(iniloc)){
    
    #print('Entering iniloc loop...')
    
    if(!exists('L.locs')){
      L.locs <- L1 * 0
      kn.idx <- c(1, length(dateVec))
    } else{
      kn.idx <- c(1, kn.idx, length(dateVec))
    }
    
    # need lat/lon vectors from locs.grid
    lon <- locs.grid$lon[1,]
    lat <- locs.grid$lat[,1]
    
    # tag location
    x = which.min((iniloc$lon[1] - lon) ^ 2)
    y = which.min((iniloc$lat[1] - lat) ^ 2)

    # assign the known location for this day, i, as 1 in likelihood raster
    L.locs[[1]][raster::cellFromXY(L.locs[[1]], iniloc[1, c(5,4)])] <- 1
    #print(raster::cellStats(L.locs[[1]], max))
    
    # pop up location
    x = which.min((iniloc$lon[2] - lon) ^ 2)
    y = which.min((iniloc$lat[2] - lat) ^ 2)

    # assign the known location for this day, i, as 1 in likelihood raster
    L.locs[[length(dateVec)]][raster::cellFromXY(L.locs[[length(dateVec)]], iniloc[2, c(5,4)])] <- 1
    
    #print(raster::cellStats(L.locs[[108]], max))
    #print(idx)
    # add known to L
    for(bb in kn.idx){
      L[[bb]] <- L.locs[[bb]]
    }
  }
  
  #========
  # Bathymetry mask
  #========
  if(!is.null(bathy)){
    print('Starting bathymetry mask...')
    
    maxDep <- data.frame(dplyr::summarise_(dplyr::group_by_(pdt, "Date"), "max(Depth)"))
    maxDep$Date <- as.Date(maxDep$Date)
    maxDep.df <- data.frame(Date = dateVec)
    maxDep.df <- merge(maxDep.df, maxDep, by = 'Date', all.x=T)
    maxDep.df[which(maxDep.df[,2] <= 0), 2] <- 1
    maxDep.df[which(is.na(maxDep.df[,2])), 2] <- 1
    #b.idx <- which(!is.na(maxDep.df[,2]))
    bathy <- raster::resample(bathy, L)
    naLidx = which(raster::cellStats(L, sum, na.rm=T) != 0)
    
    for (i in naLidx){
      #print(paste('iteration ',i,' of ', length(b.idx)))
      b.i <- bathy
      b.i[b.i <= -maxDep.df[i,2]] <- 1
      b.i[b.i != 1] <- NA
      L[[i]] <- L[[i]] * b.i
      #plot(L[[i]] * b.i); world(add=T)
    }
  }
  
  # CREATE A MORE COARSE RASTER FOR PARAMETER ESTIMATION LATER
  print('Starting raster resample...')
  L.mle <- raster::resample(L, L.mle.res)
  
  #----------------------------------------------------------------------------------#
  # MAKE ALL NA'S VERY TINY FOR THE CONVOLUTION
  # the previous steps may have taken care of this...
  #----------------------------------------------------------------------------------#
  L[L <= 1e-15] <- 1e-15
  L[is.na(L)] <- 1e-15
  L.mle[L.mle <= 1e-15] <- 1e-15
  L.mle[is.na(L.mle)] <- 1e-15
  
  # MAKE BOTH RASTERS (COARSE AND FINE RES L's) INTO AN ARRAY
  L <- aperm(raster::as.array(raster::flip(L, direction = 'y')), c(3, 2, 1))
  L.mle <- aperm(raster::as.array(raster::flip(L.mle, direction = 'y')), c(3, 2, 1))
  
  print('Finishing make.L...', sep='')
  
  return(list(L = L, L.mle = L.mle))
}

## END
