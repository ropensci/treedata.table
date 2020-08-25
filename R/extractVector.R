#' Returning a named vector from a treedata.table object
#'
#' @param tdObject A treedata.table object
#' @param ... The name of the column or columns to select.
#'
#' @return A named vector or a list of named vectors
#'
#' @examples
#'
#' data(anolis)
#' td <- as.treedata.table(tree = anolis$phy, data = anolis$dat)
#' # extracts the named vector for SVL from the td object
#' extractVector(td, "SVL")
#' # extracts the named vector for SVL and ecomorph from the td object
#' extractVector(td, "SVL", "ecomorph")
#' @export

extractVector <- function(tdObject, ...) {
  if (!inherits(tdObject, c("treedata.table"))) {
    stop("Please use a class 'treedata.table' object \n")
  }

  dat <- tdObject$dat
  dots <- lazyeval::lazy_dots(...)
  vecs <- lapply(list(...), function(x) dat[[x]])
  vecs <- if (inherits(tdObject$phy, c("phylo"))) {
    lapply(vecs, function(x) stats::setNames(x, tdObject$phy$tip.label))
  } else {
    lapply(vecs, function(x) stats::setNames(x, tdObject$phy[[1]]$tip.label))
  }
  if (length(vecs) == 1) {
    vecs <- vecs[[1]]
  } else {
    names(vecs) <- lapply(dots, function(x) x[[1]])
  }
  return(vecs)
}
