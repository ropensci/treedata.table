library(data.table)
library(treeplyr)
data(anolis)
td <- make.treedata(anolis$phy, anolis$dat)
td$dat <- data.table(td$dat)
data.table:::`[.data.table`()
class(td) <- c("treedata.table", "list")


`[.treedata.table` <- function(x, ...){
  .dat <- x$dat[,".XID." := 1:nrow(x$dat)]
  .dat <- .dat[...]
  .phy <- drop.tip(x$phy, which(!1:nrow(x$dat) %in% .dat[,.XID.]))
  x$phy <- .phy
  x$dat <- .dat
  ## Need to drop ".XID."
  return(x)
}

td[10:20, ]

td$dat[,SVL]
