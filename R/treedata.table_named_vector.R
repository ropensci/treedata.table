`[[.treedata.table` <- function (x, ..., exact = TRUE)
{
  y <- x$dat
  res <- `[[.data.frame`(y, ..., exact = exact)
  if (length(res) != nrow(y)) {
    stop("Use '[' for selecting multiple columns")
  }
  return(setNames(res, x$phy$tip.label))
}


##Example
library(data.table)
library(treeplyr)
data(anolis)
td <- make.treedata(anolis$phy, anolis$dat)
td$dat <- data.table(td$dat)
class(td) <- c("treedata.table", "list")
ex<-td[island == "Cuba" & ecomorph == "TG",] ##Dependes on [..treedata.table

ex[["attitude"]]

