`[.treedata.table` <- function(x, ...){
  .dat <- x$dat
  .dat[,rowid := seq_len(nrow(.dat))]
  dots <- lazyeval::lazy_dots(...)
  if(length(dots)>1){
    if(length(dots[[2]]$expr)!=0){
      .dat <- .dat[..., by=rowid] #Column select so need to preserve rowid
    } else{dat <- .dat[...]}
  }  else {dat <- .dat[...]}

  .phy <- drop.tip(x$phy, which(!1:nrow(x$dat) %in% .dat[,rowid]))
  x$phy <- .phy
  x$dat <- .dat[,!"rowid"]
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
tmp <- td[island == "Hispaniola",]
td[order(island)]

td[island == "Cuba" & ecomorph == "TG" ]

