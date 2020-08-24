#' Function dropping taxa from an object of class `treedata.table`
#'
#' This function can be used to remove species from an object of class
#' `treedata.table`. The resulting `treedata.table` will include fully matching
#' `$dat` and `$phy` elements. The user should confirm the changes before they
#' are processed.
#'
#' @param tdObject An object of class `treedata.table`
#' @param taxa A vector class `character` containing all taxa that needs to be
#' dropped from the original `treedata.table` object
#' @return An object of class `treedata.table` with matching `$dat` and `$phy`
#' elements based on whether `taxa` were dropped or not.
#' @examples
#' data(anolis)
#' # With a multiphylo object in the treedata.table object
#' td <- as.treedata.table(anolis$phy, anolis$dat)
#' droptreedata.table(
#'   tdObject = td, taxa =
#'     c("chamaeleonides", "eugenegrahami")
#' )
#'
#' # With a multiphylo object in the treedata.table object
#' treesFM <- list(anolis$phy, anolis$phy)
#' class(treesFM) <- "multiPhylo"
#' td <- as.treedata.table(treesFM, anolis$dat)
#' droptreedata.table(
#'   tdObject = td, taxa =
#'     c("chamaeleonides", "eugenegrahami")
#' )
#' @export


droptreedata.table <- function(tdObject, taxa) {
  if (!inherits(tdObject, c("treedata.table"))) {
    stop("Please use a class 'treedata.table' object \n")
  }

  if (!inherits(taxa, c("character"))) {
    stop("Please use a class 'character' object for taxa \n")
  }

  .phy <- tdObject$phy
  .dat <- tdObject$dat

  if (inherits(.phy, c("phylo"))) {
    .dat <- .dat[!.phy$tip.label %in% taxa]
    .phy <- ape::drop.tip(.phy, which(.phy$tip.label %in% taxa))
  } else {
    .dat <- .dat[!.phy[[1]]$tip.label %in% taxa]
    .phy <- lapply(.phy, ape::drop.tip,
      tip =
        which(.phy[[1]]$tip.label %in% taxa)
    )
    class(.phy) <- "multiPhylo"
  }


  tdObject$dat <- .dat
  tdObject$phy <- .phy
  attr(tdObject, "data_not_tree") <- if (attr(tdObject, "data_not_tree") ==
    "OK") {
    taxa
  } else {
    c(attr(tdObject, "data_not_tree"), taxa)
  }
  attr(tdObject, "tree_not_data") <- if (attr(tdObject, "tree_not_data") ==
    "OK") {
    taxa
  } else {
    c(attr(tdObject, "tree_not_data"), taxa)
  }
  attr(tdObject, "modified") <- 1
  message(length(taxa), " taxa were dropped from the treedata.table object")

  return(tdObject)
}
