#' Returns the character matrix or phylogeny from a treedata.table object
#'
#' @param tdObject A treedata.table object
#' @param type Whether the output of the function is a tree ('type="phylo"')
#' or a data.table ('type="dat"')
#' @return A `data.table` or `phylo` object from the original `treedata.table`
#' object
#' @examples
#' data(anolis)
#' td <- as.treedata.table(anolis$phy, anolis$dat)
#' pulltreedata.table(td, type = "phy")
#' pulltreedata.table(td, type = "dat")
#' @export

pulltreedata.table <- function(tdObject, type = c("dat", "phy") ) {
  if (!inherits(tdObject, c("treedata.table"))) {
    stop("Please use a class 'treedata.table' object \n")
  }
  type <- match.arg(type)
  full <-
    if (type == "dat") {
      matches <- vapply(tdObject$dat, function(x) {
        sum(x %in% tdObject$phy$tip.label)
      }, integer(1))
      if (any(matches == nrow(tdObject$dat))) {
        tdObject$dat
      } else {
        cbind(tip.label = tdObject$phy$tip.label, tdObject$dat)
      }
    } else {
      tdObject$phy
    }
  return(full)
}
