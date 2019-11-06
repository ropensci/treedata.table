library(data.table)
library(treeplyr)
data(anolis)
td <- make.treedata(anolis$phy, anolis$dat)
td$dat <- data.table(td$dat)
data.table:::`[.data.table`()
class(td) <- c("treedata.table", "list")


`[.treedata.table` <- function(x, ...){
  .dat <- x$dat[,".XID." := 1:nrow(x$dat)]
  .dat <- x$dat[...]
  .phy <- drop.tip(x$phy, which(!.dat[,".XID."] %in% 1:nrow(x$dat)))
  x$phy <- .phy
  x$dat <- .dat
  ## Need to drop ".XID."
  return(x)
}

td[ecomorph=="TG",]
