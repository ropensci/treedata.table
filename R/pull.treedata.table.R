#' Returns the character matrix or phylogeny from a treedata.table object
#'
#' @param tdObject A treedata.table object
#'
#' @return A \code{data.table} or \code{phylo} object from the original \code{treedata.table} object
#' @examples
#' data(anolis)
#' td <- as.treedata.table(anolis$phy, anolis$dat)
#' pull.treedata.table(td, type = "phy")
#' pull.treedata.table(td, type = "dat")
#' @export

pull.treedata.table <- function(tdObject, type = "dat") {
  full <-
    if (type == "dat") {
      cbind(tip.label = tdObject[[1]]$tip.label, tdObject$dat)
    } else{
      tdObject[[1]]
    }
  return(full)
}

