#' Remove Pacific Ocean data from N. Atlantic analyses
#' 
#' \code{removePacific} removes Pacific Ocean from WOA and other forms of
#' array-based data. This is a specialized function to address the issue when
#' the Pacific side of Panama enters into the model bounding box of a North
#' Atlantic analysis.
#' @param dat is output from extract.woa
#' @param lat is output from extract.woa
#' @param lon is output from extract.woa
#' @return dat is WOA data grid with Pacific removed only tested when area of
#'   interest is N Atlantic
#' @examples 
#' \dontrun{
#' woa.dir <- getwd()
#' woa <- extract.woa(woa.dir, bbox = c(-90, -30, -10, 30), 'quarter')
#' woa <- removePacific(woa, lat, lon)
#' image.plot(woa[,,1,1])
#' }

removePacific <- function(dat, lat, lon){

  # run 1
  ylims = c(min(lat), 5)
  xlims = c(min(lon), -60)
  
  ymin = which.min((ylims[1] - lat) ^ 2)
  ymax = which.min((ylims[2] - lat) ^ 2)
  xmin = which.min((xlims[1] - lon) ^ 2)
  xmax = which.min((xlims[2] - lon) ^ 2)
  
  dat[xmin:xmax, ymin:ymax,,] = NA
  
  # run 2
  ylims = c(5, 9)
  xlims = c(min(lon), -70)
  
  ymin = which.min((ylims[1] - lat) ^ 2)
  ymax = which.min((ylims[2] - lat) ^ 2)
  xmin = which.min((xlims[1] - lon) ^ 2)
  xmax = which.min((xlims[2] - lon) ^ 2)
  
  dat[xmin:xmax, ymin:ymax,,] = NA
  
  # run 3
  ylims = c(9, 15)
  xlims = c(min(lon), -84)
  
  ymin = which.min((ylims[1] - lat) ^ 2)
  ymax = which.min((ylims[2] - lat) ^ 2)
  xmin = which.min((xlims[1] - lon) ^ 2)
  xmax = which.min((xlims[2] - lon) ^ 2)
  
  dat[xmin:xmax, ymin:ymax,,] = NA
  
  # run 4
  ylims = c(15, 18)
  xlims = c(min(lon), -90)
  
  ymin = which.min((ylims[1] - lat) ^ 2)
  ymax = which.min((ylims[2] - lat) ^ 2)
  xmin = which.min((xlims[1] - lon) ^ 2)
  xmax = which.min((xlims[2] - lon) ^ 2)
  
  dat[xmin:xmax, ymin:ymax,,] = NA
  
  # run 5
  ylims = c(9, 15)
  xlims = c(min(lon), -84)
  
  ymin = which.min((ylims[1] - lat) ^ 2)
  ymax = which.min((ylims[2] - lat) ^ 2)
  xmin = which.min((xlims[1] - lon) ^ 2)
  xmax = which.min((xlims[2] - lon) ^ 2)
  
  dat[xmin:xmax, ymin:ymax,,] = NA
  
  return(dat)
  
}

