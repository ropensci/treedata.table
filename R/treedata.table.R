`[.treedata.table` <- function(x, ...){
  rownames(x$dat) <- 1:nrow(x$dat)
  .dat <- x$dat
  .dat <- .dat[...]
  .phy <- drop.tip(x$phy, which(!1:nrow(x$dat) %in% rownames(.dat)))
  x$phy <- .phy
  x$dat <- .dat
  return(x)
}

