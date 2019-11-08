`[.treedata.table` <- function(x, ...) {
  .dat <- x$dat
  .dat[, rowid := seq_len(nrow(.dat))]
  dots <- lazyeval::lazy_dots(...)
  if ("by" %in% names(dots)) {
    .dat <- .dat[...]
  } else{
    if (nchar(dots[[2]]$expr)[1] != 0) {
      .dat <- .dat[..., by = rowid]
    } else{
      .dat <- .dat[...]
    }
  }
  .phy <- drop.tip(x$phy, which(!1:nrow(x$dat) %in% .dat[, rowid]))
  x$phy <- .phy
  x$dat <- .dat[, !"rowid"]
  return(x)
}

`[[.treedata.table` <- function (x, ..., exact = TRUE)
{
  y <- x$dat
  res <- `[[.data.frame`(y, ..., exact = exact)
  if (length(res) != nrow(y)) {
    stop("Use '[' for selecting multiple columns")
  }
  return(setNames(res, x$phy$tip.label))
}






