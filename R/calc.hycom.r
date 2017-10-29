#' Hycom Profile LIkelihood
#' 
#' Calculate Hycom profile likelihood surface
#' 
#' @param pdt input PDT data output from \code{\link{read.wc}} and
#'   \code{\link{extract.pdt}}
#' @param filename is the first part of the filename specified to the download 
#'   function \code{\link{get.env}}. For example, if downloaded files were 
#'   specific to a particular dataset, you may want to identify that with a name
#'   like 'tuna' or 'shark1'. This results in a downloaded filename of, for 
#'   example, 'tuna_date.nc'. This filename is required here so the calc 
#'   function knows where to get the env data.
#' @param hycom.dir directory of downloaded hycom (or other) data
#' @param focalDim is integer for dimensions of raster::focal used to calculate 
#'   sd() of temperature grid cell. Recommend focalDim = 9 for Hycom data at 
#'   0.08deg resolution.
#' @param dateVec vector of complete dates (from tag to pop by day) for data 
#'   range. This should be in 'Date' format
#' @param use.se is logical indicating whether or not to use SE when using 
#'   regression to predict temperature at specific depth levels.
#'   
#' @return a raster brick of Hycom profile likelihood
#' @export
#' @seealso \code{\link{calc.hycom.par}}
#' @importFrom foreach %dopar%
#'

calc.hycom <- function(pdt, filename, hycom.dir, focalDim = 9, dateVec, use.se = TRUE){
  
  options(warn=-1)
  
  t0 <- Sys.time()
  print(paste('Starting Hycom profile likelihood calculation...'))
  
  # calculate midpoint of tag-based min/max temps
  pdt$MidTemp <- (pdt$MaxTemp + pdt$MinTemp) / 2
  
  # get unique time points
  dateVec = lubridate::parse_date_time(dateVec, '%Y-%m-%d')
  
  udates <- unique(lubridate::parse_date_time(pdt$Date, orders = '%Y-%m-%d %H%:%M:%S'))
  T <- length(udates)
  
  print(paste0('Generating profile likelihood for ', udates[1], ' through ', udates[length(udates)]))
  
  # open nc and get the indices for the vars
  nc1 =  RNetCDF::open.nc(dir(hycom.dir, full.names = T)[1])
  ncnames = NULL
  nmax <- RNetCDF::file.inq.nc(nc1)$nvars - 1
  for(ii in 0:nmax) ncnames[ii + 1] <- RNetCDF::var.inq.nc(nc1, ii)$name
  temp.idx <- grep('temp', ncnames, ignore.case=TRUE) - 1
  lat.idx <- grep('lat', ncnames, ignore.case=TRUE) - 1
  lon.idx <- grep('lon', ncnames, ignore.case=TRUE) - 1
  dep.idx <- grep('dep', ncnames, ignore.case=TRUE) - 1
  
  # get attributes, if they exist
  ncatts <- NULL
  nmax <- RNetCDF::var.inq.nc(nc1, temp.idx)$natts - 1
  for(ii in 0:nmax) ncatts[ii + 1] <- RNetCDF::att.inq.nc(nc1, temp.idx, ii)$name
  scale.idx <- grep('scale', ncatts, ignore.case=TRUE) - 1
  if(length(scale.idx) != 0){
    scale <- RNetCDF::att.get.nc(nc1, temp.idx, attribute=scale.idx)
  } else{
    scale <- 1
  }
  off.idx <- grep('off', ncatts, ignore.case=TRUE) - 1
  if(length(off.idx) != 0){
    offset <- RNetCDF::att.get.nc(nc1, temp.idx, attribute=off.idx)
  } else{
    offset <- 1
  }
  
  # get and check the vars
  depth <- RNetCDF::var.get.nc(nc1, dep.idx)
  lon <- RNetCDF::var.get.nc(nc1, lon.idx)
  if(length(dim(lon)) == 2) lon <- lon[,1]
  if(!any(lon < 180)) lon <- lon - 360
  lat <- RNetCDF::var.get.nc(nc1, lat.idx)
  if(length(dim(lat)) == 2) lat <- lat[1,]
  
  # result will be array of likelihood surfaces
  L.hycom <- array(0, dim = c(length(lon), length(lat), length(dateVec)))
  
  print(paste('Starting iterations through deployment period ', '...'))
  
  for(i in 1:T){ 
    time <- as.Date(udates[i])
    pdt.i <- pdt[which(pdt$Date == time),]
    print(paste('Starting ', time,'...',sep=''))
    
    # open day's hycom data
    nc <- RNetCDF::open.nc(paste(hycom.dir, filename, '_', as.Date(time), '.nc', sep=''))
    dat <- RNetCDF::var.get.nc(nc, temp.idx) * scale + offset
    
    #extracts depth from tag data for day i
    y <- pdt.i$Depth[!is.na(pdt.i$Depth)] 
    y[y < 0] <- 0
    
    #extract temperature from tag data for day i
    x <- pdt.i$MidTemp[!is.na(pdt.i$Depth)]  
    
    # use the which.min
    depIdx = unique(apply(as.data.frame(pdt.i$Depth), 1, FUN = function(x) which.min((x - depth) ^ 2)))
    hycomDep <- depth[depIdx]
    
    # make predictions based on the regression model earlier for the temperature at standard WOA depth levels for low and high temperature at that depth
    suppressWarnings(
      fit.low <- locfit::locfit(pdt.i$MinTemp ~ pdt.i$Depth)
    )
    suppressWarnings(
      fit.high <- locfit::locfit(pdt.i$MaxTemp ~ pdt.i$Depth)
    )
    n = length(hycomDep)
    
    #suppressWarnings(
    pred.low = stats::predict(fit.low, newdata = hycomDep, se = T, get.data = T)
    #suppressWarnings(
    pred.high = stats::predict(fit.high, newdata = hycomDep, se = T, get.data = T)
    
    if (use.se){
      # data frame for next step
      df = data.frame(low = pred.low$fit - pred.low$se.fit * sqrt(n),
                      high = pred.high$fit + pred.high$se.fit * sqrt(n),
                      depth = hycomDep)
    } else{
      # data frame for next step
      df = data.frame(low = pred.low$fit,# - pred.low$se.fit * sqrt(n),
                      high = pred.high$fit,# + pred.high$se.fit * sqrt(n),
                      depth = hycomDep)
    }
    
    # calculate sd using Le Bris neighbor method and focal()
    sd.i = array(NA, dim = c(dim(dat)[1:2], length(depIdx)))
    
    for(ii in 1:length(depIdx)){
      r = raster::flip(raster::raster(t(dat[,,depIdx[ii]])), 2)
      f1 = raster::focal(r, w = matrix(1, nrow = focalDim, ncol = focalDim), fun = function(x) stats::sd(x, na.rm = T))
      f1 = t(raster::as.matrix(raster::flip(f1, 2)))
      sd.i[,,ii] = f1
    }
    
    # make index of dates for filling in lik.prof
    didx = base::match(udates, dateVec)
    
    # setup the likelihood array for each day. Will have length (dim[3]) = n depths
    lik.pdt = array(NA, dim = c(dim(dat)[1], dim(dat)[2], length(depIdx)))
    
    for (b in 1:length(depIdx)) {
      #calculate the likelihood for each depth level, b
      lik.try <- try(likint3(dat[,,depIdx[b]], sd.i[,,b], df[b, 1], df[b, 2]), TRUE)
      class.try <- class(lik.try)
      
      if(!any(which(lik.try > 0))) class.try <- 'try-error'
      
      if(class.try == 'try-error' & use.se == FALSE){
        df[b,1] <- pred.low$fit[b] - pred.low$se.fit[b] * sqrt(n)
        df[b,2] <- pred.high$fit[b] - pred.high$se.fit[b] * sqrt(n)
        
        lik.try <- try(likint3(dat[,,depIdx[b]], sd.i[,,b], df[b, 1], df[b, 2]), TRUE)
        class.try <- class(lik.try)
        
        if(!any(which(lik.try > 0))) class.try <- 'try-error'
        
        if (class.try == 'try-error'){
          lik.try <- dat[,,depIdx[b]] * 0
          warning(paste('Warning: likint3 failed after trying with and without SE prediction of depth-temp profiles. This is most likely a divergent integral for ', time, '...', sep=''))
        }
        
      } else if (class.try == 'try-error' & use.se == TRUE){
        lik.try <- dat[,,depIdx[b]] * 0
        warning(paste('Warning: likint3 failed after trying with and without SE prediction of depth-temp profiles. This is most likely a divergent integral for ', time, '...', sep=''))
      }
      
      lik.pdt[,,b] <- lik.try
      
    }
    
    lik.pdt0 <- lik.pdt
    lik.pdt0[is.na(lik.pdt0)] <- 0
    use.idx <- unique(which(lik.pdt0 != 0, arr.ind=T)[,3])
    
    # multiply likelihood across depth levels for each day
    lik.pdt <- apply(lik.pdt[,,use.idx], 1:2, FUN=function(x) prod(x, na.rm=F))
    
    if(i == 1){
      # result will be array of likelihood surfaces
      L.hycom <- array(0, dim = c(dim(lik.pdt), length(dateVec)))
    }
    
    idx <- which(dateVec == as.Date(time))
    L.hycom[,,idx] = lik.pdt / max(lik.pdt, na.rm=T)
    
  }
  
  #parallel::stopCluster(cl)
  
  # make index of dates for filling in L.hycom
  #didx <- base::match(udates, dateVec)
  
  # lapply to normalize
  #lik.pdt <- lapply(ans, function(x) x / max(x, na.rm = T))
  
  # fill in L.hycom from the list output
  #ii = 1
  #for(i in didx){
  #  L.hycom[,,i] = lik.pdt[[ii]]
  #  ii = ii+1  
  #}
  
  print(paste('Making final likelihood raster...'))
  
  crs <- "+proj=longlat +datum=WGS84 +ellps=WGS84"
  L.hycom <- raster::brick(L.hycom, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), transpose=T, crs)
  L.hycom <- raster::flip(L.hycom, direction = 'y')
  
  #L.hycom[L.hycom < 0] <- 0
  
  names(L.hycom) = as.character(dateVec)
  
  t1 <- Sys.time()
  print(paste('Hycom profile calculations took ', round(as.numeric(difftime(t1, t0, units='mins')), 2), 'minutes...'))
  
  options(warn=2)
  
  # return hycom likelihood surfaces
  return(L.hycom)
  
}
