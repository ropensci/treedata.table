#' Function for performing data.table operations on an object of class \code{treedata.table}
#'
#' This function can be used to subset rows, select and compute on columns (\code{\link{data.tabe}}.
#'
#' @param x An object of class \code{treedata.table}
#' @param ... Arguments in the structure of \code{data.table} used to perform changes on the \code{treedata.table} object
#' @return A new object of class \code{treedata.table} with \code{$dat} and \code{$phy} corresponding with the changes set to \code{$dat} using (\code{\link{data.table}})'s structure.
#' @seealso \code{\link{data.table}}
#' @examples
#'
#' data(anolis)
#' anolis2<-anolis$phy
#' anolis2$tip.label[1]<-'NAA'
#' anolis1<-anolis$phy
#' anolis1$tip.label[1]<-'NAA'
#' trees<-list(anolis1,anolis2)
#' class(trees) <- "multiPhylo"
#' treesFM<-list(anolis$phy,anolis$phy)
#' class(treesFM) <- "multiPhylo"
#'
#' #A phylo object that fully matches the data
#' td <- as.treedata.table(tree=anolis$phy, data=anolis$dat)
#' td <- as.treedata.table(anolis$phy, anolis$dat)
#' td[,SVL]
#' td[island == "Cuba" & ecomorph == "TG", .(ecomorph, island, SVL)]
#' td[, head(.SD, 1), by = .(ecomorph, island)]
#'
#' #A multiphylo object that fully matches the data
#' td <- as.treedata.table(tree=treesFM, data=anolis$dat)
#' td <- as.treedata.table(treesFM, anolis$dat)
#' td[,SVL]
#' td[island == "Cuba" & ecomorph == "TG", .(ecomorph, island, SVL)]
#' td[, head(.SD, 1), by = .(ecomorph, island)]

#' #A phylo object that partially matches the data
#' td <- as.treedata.table(tree=anolis1, data=anolis$dat)
#' td <- as.treedata.table(anolis1, anolis$dat)
#' td[,SVL]
#' td[island == "Cuba" & ecomorph == "TG", .(ecomorph, island, SVL)]
#' td[, head(.SD, 1), by = .(ecomorph, island)]
#'
#' #A multiphylo object that partially matches the data
#' td <- as.treedata.table(tree=trees, data=anolis$dat)
#' td <- as.treedata.table(trees, anolis$dat)
#' td[,SVL]
#' td[island == "Cuba" & ecomorph == "TG", .(ecomorph, island, SVL)]
#' td[, head(.SD, 1), by = .(ecomorph, island)]
#'
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
  #.phy <- ape::drop.tip(x$phy, which(!1:nrow(x$dat) %in% unlist(.dat[, "rowid"]))) #CRP: using "rowid" instead of rowid & unlist

  .phy<- if(class(x$phy)=='phylo'){
    ape::drop.tip(x$phy, which(!1:nrow(x$dat) %in% unlist(.dat[, "rowid"])))
  }else{
    tr<-lapply(x$phy,ape::drop.tip,tip=which(!1:nrow(x$dat) %in% unlist(.dat[, "rowid"])))
    class(tr)<-'multiPhylo'
    tr
  }

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
#' #With a phylo object
#' td <- as.treedata.table(anolis$phy, anolis$dat)
#' td[["SVL"]]
#'
#' #With a multiPhylo object
#' treesFM<-list(anolis$phy,anolis$phy)
#' class(treesFM) <- "multiPhylo"
#' td <- as.treedata.table(treesFM, anolis$dat)
#' td[["SVL"]]
#' @export

`[[.treedata.table` <- function (x, ..., exact = TRUE){
  y <- x$dat
  res <- `[[.data.frame`(y, ..., exact = exact)
  if (length(res) != nrow(y)) {
    stop("Use '[' for selecting multiple columns")
  }
  return(stats::setNames(res, if(class(x$phy)=='phylo'){ x$phy$tip.label} else{x$phy[[1]]$tip.label}))
}


