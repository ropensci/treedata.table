#' Function for performing data.table operations on an object of class
#' `treedata.table`
#'
#' This function can be used to subset rows, select and compute on columns
#' [data.table][data.table::data.table].
#'
#' @param x An object of class `treedata.table`
#' @param ... Arguments in the structure of `data.table` used to perform changes
#' on the `treedata.table` object
#' @return A new object of class `treedata.table` with `$dat` and `$phy`
#' corresponding with the changes set to `$dat` using
#' [data.table][data.table::data.table]'s structure.
#' @seealso [data.table][data.table::data.table]
#' @examples
#'
#' data(anolis)
#' anolis2 <- anolis$phy
#' anolis2$tip.label[1] <- "NAA"
#' anolis1 <- anolis$phy
#' anolis1$tip.label[1] <- "NAA"
#' trees <- list(anolis1, anolis2)
#' class(trees) <- "multiPhylo"
#' treesFM <- list(anolis$phy, anolis$phy)
#' class(treesFM) <- "multiPhylo"
#'
#' # A phylo object that fully matches the data
#' td <- as.treedata.table(tree = anolis$phy, data = anolis$dat)
#' td <- as.treedata.table(anolis$phy, anolis$dat)
#' td[, SVL]
#' td[island == "Cuba" & ecomorph == "TG", .(ecomorph, island, SVL)]
#' td[, utils::head(.SD, 1), by = .(ecomorph, island)]
#'
#' # A multiphylo object that fully matches the data
#' td <- as.treedata.table(tree = treesFM, data = anolis$dat)
#' td <- as.treedata.table(treesFM, anolis$dat)
#' td[, SVL]
#' td[island == "Cuba" & ecomorph == "TG", .(ecomorph, island, SVL)]
#' td[, utils::head(.SD, 1), by = .(ecomorph, island)]
#'
#' # A phylo object that partially matches the data
#' td <- as.treedata.table(tree = anolis1, data = anolis$dat)
#' td <- as.treedata.table(anolis1, anolis$dat)
#' td[, SVL]
#' td[island == "Cuba" & ecomorph == "TG", .(ecomorph, island, SVL)]
#' td[, utils::head(.SD, 1), by = .(ecomorph, island)]
#'
#' # A multiphylo object that partially matches the data
#' td <- as.treedata.table(tree = trees, data = anolis$dat)
#' td <- as.treedata.table(trees, anolis$dat)
#' td[, SVL]
#' td[island == "Cuba" & ecomorph == "TG", .(ecomorph, island, SVL)]
#' td[, utils::head(.SD, 1), by = .(ecomorph, island)]
#' @importFrom lazyeval lazy_dots
#' @export

`[.treedata.table` <- function(x, ...) {
  .dat <- x$dat
  .dat <- base::cbind(.dat, "rowid" = seq_len(nrow(.dat)))
  # instead of :=
  dots <- lazyeval::lazy_dots(...)
  if ("by" %in% names(dots)) {
    .dat <- .dat[...]
  } else {
    if (nchar(dots[[2]]$expr)[1] != 0) {
      .dat <- .dat[..., by = "rowid"]
    } else {
      .dat <- .dat[...]
    }
  }

  .phy <- if (inherits(x$phy, c("phylo"))) {
    ape::drop.tip(x$phy, which(!seq_len(nrow(x$dat)) %in% unlist(.dat[, "rowid"])))
  } else {
    tr <- lapply(x$phy, ape::drop.tip, tip = which(!seq_len(nrow(x$dat))
    %in% unlist(.dat[, "rowid"])))
    class(tr) <- "multiPhylo"
    tr
  }

  x$phy <- .phy
  x$dat <- .dat[, !"rowid"]
  return(x)
}



#' Function for extract a named vector from an object of class `treedata.table`
#'
#' This function extracts a named vector for any  trait from an object of class
#' `treedata.table`.
#'
#' @param x An object of class `treedata.table`
#' @param ... Column name in class `character`
#' @param exact whether exact search should be conducted
#' @return A new object of class `vector` with names set to labels corresponding
#' to tip labels in the provided `treedata.table` object.
#' @seealso [data.table()]
#' @examples
#' data(anolis)
#' # With a phylo object
#' td <- as.treedata.table(anolis$phy, anolis$dat)
#' td[["SVL"]]
#'
#' # With a multiPhylo object
#' treesFM <- list(anolis$phy, anolis$phy)
#' class(treesFM) <- "multiPhylo"
#' td <- as.treedata.table(treesFM, anolis$dat)
#' td[["SVL"]]
#' @export

`[[.treedata.table` <- function(x, ..., exact = TRUE) {
  y <- x$dat
  res <- `[[.data.frame`(y, ..., exact = exact)
  if (length(res) != nrow(y)) {
    stop("Use '[' for selecting multiple columns")
  }
  return(stats::setNames(res, if (inherits(x$phy, c("phylo"))) {
    x$phy$tip.label
  } else {
    x$phy[[1]]$tip.label
  }))
}
