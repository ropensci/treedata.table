#' Run a function on a \code{treedata.table} object
#'
#' @param tdObject A treedata.table object
#' @param ... A function call.
#'
#' @details This function allows R functions that use trees and data to be run on
#' \code{treedata.table} objects.
#'
#' @return Function output
#'
#' @examples
#' data(anolis)
#' td <- as.treedata.table(anolis$phy, anolis$dat)
#' \dontrun{
#' tdt(td, geiger::fitContinuous(phy, extractVector(td, SVL), model="BM", ncores=1))
#' tdt(td, phytools::phenogram(phy, extractVector(td, SVL), quiet=TRUE, spread.labels=FALSE))
#' }
#' @export

tdt <- function(tdObject, ...){
  if(!is.call(substitute(...))){
    call <- list(...)[[1]]
  } else {
    call <- substitute(...)
  }
  env <- new.env(parent = parent.frame(), size = 1L)
  env$phy <- tdObject$phy
  env$dat <- tdObject$dat
  out <- eval(call, env)
  if(is.null(out)){
    invisible()
  } else {
    return(out)
  }
}
