#' Function dropping taxa from an object of class \code{treedata.table}
#'
#' This function can be used to remove species from an object of class \code{treedata.table}. The resulting \code{treedata.table} will
#' include fully matching \code{$dat} and \code{$phy} elements. The user should confirm the changes before they are processed.
#'
#' @param tdObject An object of class \code{treedata.table}
#' @param taxa A vector class \code{character} containing all taxa that needs to be dropped from the original \code{treedata.table} object
#' @return An object of class \code{treedata.table} with matching \code{$dat} and \code{$phy} elements based on whether \code{taxa} were dropped or not.
#' @examples
#' data(anolis)
#' # With a multiphylo object in the treedata.table object
#' td <- as.treedata.table(anolis$phy, anolis$dat)
#' droptreedata.table(tdObject = td, taxa = c("chamaeleonides", "eugenegrahami"))
#'
#' # With a multiphylo object in the treedata.table object
#' treesFM <- list(anolis$phy, anolis$phy)
#' class(treesFM) <- "multiPhylo"
#' td <- as.treedata.table(treesFM, anolis$dat)
#' droptreedata.table(tdObject = td, taxa = c("chamaeleonides", "eugenegrahami"))
#' @export


droptreedata.table <- function(tdObject, taxa) {
  if (class(tdObject) != "treedata.table") {
    stop("Please use a class 'treedata.table' object \n")
  }

  if (class(taxa) != "character") {
    stop("Please use a class 'character' object for taxa \n")
  }


  message("Please confirm that you would like to make changes to the ORIGINAL data?")
  n <- readline(prompt = "Type: (1) YES, (2) NO: ")
  if (n == 1 | n == "yes" | n == "YES") {
    .dat <- tdObject$dat
    .phy <- tdObject$phy

    if (inherits(.phy, c("phylo"))) {
      .dat <- .dat[!.phy$tip.label %in% taxa]
      .phy <- ape::drop.tip(.phy, which(.phy$tip.label %in% taxa))
    } else {
      .dat <- .dat[!.phy[[1]]$tip.label %in% taxa]
      .phy <- lapply(.phy, ape::drop.tip, tip = which(.phy[[1]]$tip.label %in% taxa))
      class(.phy) <- "multiPhylo"
    }


    tdObject$dat <- .dat
    tdObject$phy <- .phy
    attr(tdObject, "modified") <- 1
    return(tdObject)
    message("Changes were included to the ORIGINAL data")
  } else {
    message("NO changes made to the ORIGINAL data")
    return(tdObject)
  }
}
