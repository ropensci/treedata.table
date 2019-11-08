#' Function dropping taxa from an object of class \code{treedata.table}
#'
#' This function can be used to remove species from an object of class \code{\link{treedata.table}}. The resulting \code{treedata.table} will
#' include fully matching \code{$dat} and \code{$phy} elements. The user should confirm the changes before they are processed.
#'
#' @param .tdObject An object of class \code{treedata.table}
#' @param taxa A vector class \code{character} containing all taxa that needs to be dropped from the original \code{treedata.table} object
#' @return An object of class \code{treedata.table} with matching \code{$dat} and \code{$phy} elements based on whether \code{taxa} were dropped or not.
#' @examples
#' data(anolis)
#' td <- as.treedata.table(anolis$phy, anolis$dat)
#' droptreedata.table(tdObject=td, taxa=c("chamaeleonides" ,"eugenegrahami" ))
#' @export


droptreedata.table <- function(tdObject, taxa) {
  message("Make changes to the ORIGINAL data?")
  n <- readline(prompt = "Type: (1) YES, (2) NO: ")
  if (n == 1 | n == "yes" | n == "YES") {
    .dat <- tdObject$dat
    .phy <- tdObject$phy
    .dat <- .dat[!.phy$tip.label %in% taxa]
    .phy <- drop.tip(.phy, which(.phy$tip.label %in% taxa))
    tdObject$dat <- .dat
    tdObject$phy <- .phy
    attr(tdObject,'modified') <- "droptreedata.table was run on this object"
    return(tdObject)
    message("Changes were included to the ORIGINAL data")
  } else{
    message("NO changes made to the ORIGINAL data")
    return(tdObject)
  }
}
