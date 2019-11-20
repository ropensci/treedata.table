#' Function for perfoming data.table operations on an object of class \code{treedata.table}
#'
#' This function can be used to subset rows, select and compute on columns (\code{\link{data.table}}.
#'
#' @param x An object of class \code{treedata.table}
#' @param ... Arguments in the structure of \code{data.table} used to perform changes on the \code{treedata.table} object
#' @return A new object of class \code{treedata.table} with \code{$dat} and \code{$phy} corresponding with the changes set to \code{$dat} using (\code{\link{data.table}})'s structure.
#' @seealso \code{\link{data.table}}
#' @examples
#' data(anolis)
#' td <- as.treedata.table(anolis$phy, anolis$dat)
#' td[,SVL]
#' td[island == "Cuba" & ecomorph == "TG", .(ecomorph, island, SVL)]
#' td[, head(.SD, 1), by = .(ecomorph, island)]
#' @export

`[.treedata.table` <- function(x, ...) {
  .dat <- x$dat
  .dat<-base::cbind(.dat, "rowid"=seq_len(nrow(.dat))) #CRP: using cbind instead of :=
  dots <- lazyeval::lazy_dots(...)
  if ("by" %in% names(dots)) {
    .dat <- .dat[...]
  } else{
    if (nchar(dots[[2]]$expr)[1] != 0) {
      .dat <- .dat[..., by = "rowid"] #CRP: using "rowid" instead of rowid
    } else{
      .dat <- .dat[...]
    }
  }
  .phy <- ape::drop.tip(x$phy, which(!1:nrow(x$dat) %in% unlist(.dat[, "rowid"]))) #CRP: using "rowid" instead of rowid & unlist
  x$phy <- .phy
  x$dat <- .dat[, !"rowid"]
  return(x)
}


#' Function for extract a named vector from an object of class \code{treedata.table}
#'
#' This function extracts a named vector for any  trait from an object of class \code{treedata.table}.
#'
#' @param x An object of class \code{treedata.table}
#' @param ... Column name in class \code{character}
#' @param exact whether exact search should be conducted
#' @return A new object of class \code{vector} with names set to labels corresponding to tip labels in the provided \code{treedata.table} object.
#' @seealso \code{\link{data.table}}
#' @examples
#' data(anolis)
#' td <- as.treedata.table(anolis$phy, anolis$dat)
#' td[["SVL"]]
#' @export

`[[.treedata.table` <- function (x, ..., exact = TRUE){
  y <- x$dat
  res <- `[[.data.frame`(y, ..., exact = exact)
  if (length(res) != nrow(y)) {
    stop("Use '[' for selecting multiple columns")
  }
  return(stats::setNames(res, x$phy$tip.label))
}

