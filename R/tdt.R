#' Run a function on a `treedata.table` object
#'
#' @param tdObject A treedata.table object
#' @param ... A function call.
#'
#' @details This function allows R functions that use trees and data to be run
#' on`treedata.table` objects.
#'
#' @return Function output for a single tree (phylo) or a list of function
#' outputs (one per each tree in the MultiPhylo object)
#'
#' @examples
#' data(anolis)
#' \donttest{
#'
#' # A treedata.table object with a phylo $phy
#' td <- as.treedata.table(anolis$phy, anolis$dat)
#' tdt(td, geiger::fitContinuous(phy, extractVector(td, "SVL"),
#'   model = "BM", ncores = 1
#' ))
#'
#'
#' # A treedata.table object with a multiPhylo $phy
#' treesFM <- list(anolis$phy, anolis$phy)
#' class(treesFM) <- "multiPhylo"
#' td <- as.treedata.table(treesFM, anolis$dat)
#' tdt(td, geiger::fitContinuous(phy, extractVector(td, "SVL"),
#'   model = "BM",
#'   ncores = 1
#' ))
#' }
#' @export

tdt <- function(tdObject, ...) {
  if (!inherits(tdObject, c("treedata.table"))) {
    stop("Please use a class 'treedata.table' object \n")
  }

  if (!is.call(substitute(...))) {
    call <- list(...)[[1]]
  } else {
    call <- substitute(...)
  }
  env <- new.env(parent = parent.frame(), size = 1L)
  env$dat <- tdObject$dat

  if (inherits(tdObject$phy, c("phylo"))) {
    message("Phylo object detected. Expect a single function output")
    env$phy <- tdObject$phy
    out <- eval(call, env)
    if (is.null(out)) {
      invisible()
    } else {
      return(out)
    }
  } else {
    message("Multiphylo object detected. Expect a list of function outputs")
    lapply(seq_along(tdObject$phy), function(x) {
      env$phy <- tdObject$phy[[x]]
      out <- eval(call, env)
      if (is.null(out)) {
        invisible()
      } else {
        return(out)
      }
    })
  }
}
