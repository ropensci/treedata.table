#' Run a function on a \code{treedata.table} object
#'
#' @param tdObject A treedata.table object
#' @param ... A function call.
#'
#' @details This function allows R functions that use trees and data to be run on
#' \code{treedata.table} objects.
#'
#' @return Function output for a single tree (phylo) or a list of function outputs (multiPhylo)
#'
#' @examples
#' data(anolis)
#' \dontrun{
#'
#' #A treedata.table object with a phylo $phy
#' td <- as.treedata.table(anolis$phy, anolis$dat)
#' tdt(td, geiger::fitContinuous(phy, extractVector(td, SVL), model="BM", ncores=1))
#' tdt(td, phytools::phenogram(phy, extractVector(td, SVL), quiet=TRUE, spread.labels=FALSE))
#'
#'
#' #A treedata.table object with a multiPhylo $phy
#' treesFM<-list(anolis$phy,anolis$phy)
#' class(treesFM) <- "multiPhylo"
#' td <- as.treedata.table(treesFM, anolis$dat)
#' tdt(td, geiger::fitContinuous(phy, extractVector(td, SVL), model="BM", ncores=1))
#' tdt(td, phytools::phenogram(phy, extractVector(td, SVL), quiet=TRUE, spread.labels=FALSE))
#'
#' }
#' @export

tdt <- function(tdObject, ...){
  if(!is.call(substitute(...))){
    call <- list(...)[[1]]
  } else {
    call <- substitute(...)
  }
  env <- new.env(parent = parent.frame(), size = 1L)
  env$dat <- tdObject$dat

  if(class(tdObject$phy) == "phylo"){
    env$phy <- tdObject$phy
    out <- eval(call, env)
    if(is.null(out)){
      invisible()
    } else {
      return(out)
    }
  }else{

    lapply(seq_along(tdObject$phy), function(x){
      env$phy <- tdObject$phy[[x]]
      out <- eval(call, env)
      if(is.null(out)){
        invisible()
      } else {
        return(out)
      }
    })
  }
}
