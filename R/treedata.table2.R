`[.treedata.table` <- function(x, ...){
  .dat <- x$dat
  .dat[,rowid := seq_len(nrow(.dat))]
  .dat <- .dat[..., by=rowid]
  .phy <- drop.tip(x$phy, which(!1:nrow(x$dat) %in% .dat[,rowid]))
  x$phy <- .phy
  x$dat <- .dat
  return(x)
}


##Examples
library(data.table)
library(treeplyr)
data(anolis)
td <- make.treedata(anolis$phy, anolis$dat)
td$dat <- data.table(td$dat)
class(td) <- c("treedata.table", "list")

td[,SVL]
td[island == "Cuba" & ecomorph == "TG", .(ecomorph, island, SVL)]
td[island == "Cuba" & ecomorph == "TG",]

