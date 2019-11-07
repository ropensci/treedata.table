`[.treedata.table` <- function(x, ...){
  .dat <- x$dat
  .dat[,rowid := seq_len(nrow(.dat))]
  .dat <- .dat[..., by=rowid]
  .phy <- drop.tip(x$phy, which(!1:nrow(x$dat) %in% .matched[,rowid]))
  x$phy <- .phy
  x$dat <- .dat
  return(x)
}
