#' Combine tree and data.frame into a single treedata.table object
#'
#' This function takes as input a tree of class \code{phylo} and a \code{data.frame}
#' and combines them into a treedata.table. A \code{treedata.table} object is
#' sorted such that the rows in the data.table are matched to the tip.labels
#' of the phylogeny. Tip.labels on the tree must match a column of tip
#' names in the input data.frame. The output of this function will be a
#' treedata.table, which can be manipulated as a data.table.
#'
#' @importFrom data.table setDT as.data.table
#' @importFrom geiger name.check
#' @param tree A tree of class \code{phylo}
#' @param data A dataset in format \code{data.frame}
#' @return treedata.table An object of type \code{treedata.table} containing the tree and data.table
#' @examples
#' data(anolis)
#' td <- as.treedata.table(anolis$phy, anolis$dat)
#' @export

as.treedata.table<-function(tree, data){
  if(class(tree) != "phylo"){
    stop("Please use a class 'phylo' tree \n")
    }
  if(class(data) != "data.frame"){
    stop("Your data MUST be of class data.frame")
    }
  if(dim(data)[2] < 2){
    stop("Your data MUST have at least two columns (tip.names; nstates)")
    }

  if(geiger::name.check(tree, data.names = data[,1] )[1] != "OK"){
    data_not_tree <- setdiff(as.character(data[,1]), tree$tip.label)
    tree_not_data <- setdiff(tree$tip.label, data[,1])
  }
  else{
    data_not_tree <- "OK"
    tree_not_data <- "OK"
  }

  comb<-list(phy=tree, dat=data.table::as.data.table(data))
  data.table::setDT(comb$dat)[ , name := factor(comb$phy$tip.label,
                                    levels = comb$phy$tip.label)]
  attr(comb,'data_not_tree') <- data_not_tree
  attr(comb,'tree_not_data') <- tree_not_data

  class(comb)<-"treedata.table"
  return(comb)
}