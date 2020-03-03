#' Returns the character matrix or phylogeny from a treedata.table object
#'
#' @param tdObject A treedata.table object
#' @param type Whether the output of the function is a tree ('type="phylo"') or a data.table ('type="dat"')
#'
#' @return A \code{data.table} or \code{phylo} object from the original \code{treedata.table} object
#' @examples
#' data(anolis)
#' td <- as.treedata.table(anolis$phy, anolis$dat)
#' pull.treedata.table(td, type = "phy")
#' pull.treedata.table(td, type = "dat")
#' @export

pull.treedata.table <- function(tdObject, type = "dat") {
  if(class(tdObject) != "treedata.table" ){
    stop("Please use a class 'treedata.table' object \n")
  }
  full <-
    if (type == "dat") {
      matches <- vapply(tdObject$dat, function(x) sum(x %in% tdObject$phy$tip.label), integer(1))
      if(any(matches==nrow(tdObject$dat))){
        tdObject$dat
      } else{
        cbind(tip.label = tdObject$phy$tip.label, tdObject$dat)
      }
    } else{
      tdObject$phy
    }
  return(full)
}

