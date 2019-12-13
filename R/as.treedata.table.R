#' Combine tree and data.frame into a single treedata.table object
#'
#' This function takes as input a tree of class \code{phylo} or \code{multiPhylo} and a \code{data.frame}
#' and combines them into a treedata.table. If a \code{multiPhylo} is provided, all trees must have the same tip.labels.
#' \code{treedata.table} object is sorted such that the rows in the data.table are matched to the tip.labels
#' of the phylogeny. Tip.labels on the tree must match a column of tip
#' names in the input data.frame. The output of this function will be a
#' treedata.table, which can be manipulated as a data.table.
#'
#' @importFrom data.table setDT as.data.table
#' @param tree A tree of class \code{phylo} or multiple trees of class \code{multiPhylo}
#' @param data A dataset in format \code{data.frame}
#' @param name_column A character indicating the name of taxa in \code{data.frame}
#' @return treedata.table An object of type \code{treedata.table} containing the tree and data.table
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
#' #A multiphylo object that fully matches the data
#' td <- as.treedata.table(tree=treesFM, data=anolis$dat)
#' #A phylo object that partially matches the data
#' td <- as.treedata.table(tree=anolis1, data=anolis$dat)
#' #A multiphylo object that partially matches the data
#' td <- as.treedata.table(tree=trees, data=anolis$dat)
#'
#' @export

as.treedata.table<-function(tree, data, name_column="detect"){
  if(class(tree) %in%  c("phylo", 'multiPhylo')==F ){
    stop("Please use a class 'phylo' or 'multiPhylo' tree \n")
  }
  if( class(tree) == 'multiPhylo' ){
    equal_T<-length(unique(lapply(seq_along(tree), function(x) tree[[x]]$tip.label))) == 1
    if( equal_T == F ) {stop("Please make sure that tip labels are equivalent across trees in the multiPhylo object \n")}
  }


  #if(class(data) != "data.frame"){
  #  stop("Your data MUST be of class data.frame")
  #  }
  #if(dim(data)[2] < 2){
  #  stop("Your data MUST have at least two columns (tip.names; nstates)")
  #  }
  if(is.vector(data)){
    data <- data.frame(as.matrix(data))
    colnames(data) <- "trait"
  }
  if(is.null(colnames(data))){
    colnames(data) <- paste("trait", 1:ncol(data), sep="")
  }
  coln <- colnames(data)
  if(name_column=="detect"){
    if(is.null(rownames(data))){
      tmp.df <- data.frame(data)
      offset <- 0
    } else {
      tmp.df <- data.frame(rownames(data), data)
      offset <- 1
    }

    matches <- sapply(tmp.df, function(x) sum(x %in% if( class(tree) == 'phylo'){tree$tip.label }else{ tree[[1]]$tip.label } ) )

    if(all(matches==0)) stop("No matching names found between data and tree")
    name_column <- which(matches==max(matches))-offset
  } else{
    if(is.character(name_column)){
      name_column <- which(name_column==coln)[1]
    }
  }


  if(class(tree)== 'phylo' ){

    if(geiger::name.check(tree, data.names = data[,1] )[1] != "OK"){
      data_not_tree <- setdiff(as.character(data[,1]), tree$tip.label)
      tree_not_data <- setdiff(tree$tip.label, data[,1])
      message(paste0("\n", length(c(tree_not_data)) ," tip(s) dropped from your tree",
                     "\n", length(c(data_not_tree)) ," tip(s)  dropped from your dataset"))
      tree<- ape::drop.tip(tree, tree_not_data)
      data<-data[! as.character(data[,1]) ==data_not_tree   ,]
    }else{
      data_not_tree <- "OK"
      tree_not_data <- "OK"
    }

  }else{


    if( geiger::name.check(tree[[1]], data.names = data[,1] )[1]    != "OK"   ){
      data_not_tree <- setdiff(as.character(data[,1]), tree[[1]]$tip.label)
      tree_not_data <- setdiff(tree[[1]]$tip.label, data[,1])

      tree<- lapply(tree,ape::drop.tip,tip=tree_not_data)
      class(tree)<-"multiPhylo"
      data<-data[! as.character(data[,1]) ==data_not_tree   ,]
      message(paste0("\n", length(c(tree_not_data)) ," tip(s) dropped from ", length(tree) ," trees",
                     "\n", length(c(data_not_tree)) ," tip(s)  dropped from your dataset"))

    }else{
      data_not_tree <- "OK"
      tree_not_data <- "OK"
    }

  }

  i <- sapply(data, is.factor);data[i] <- lapply(data[i], as.character) ##Tranform factors into character vectors

  data<- if( class(tree) == 'phylo'){ data[match(tree$tip.label, data[,name_column]),] }else{
    data[match(tree[[1]]$tip.label, data[,name_column]),]
  }


  colnames(data)[name_column] <- "tip.label"
  dr<-which(tree$tip.label %in% c(tree_not_data,data_not_tree))

  tree<- if( class(tree) == 'phylo'){ ape::drop.tip(tree, dr) }else{
    nt<-lapply(tree,ape::drop.tip,tip=dr)
    class(nt)<-"multiPhylo"
    nt
  }
  data<-data.table::as.data.table(data)[!dr]
  comb<-list(phy=tree, dat=data)
  attr(comb,'data_not_tree') <- data_not_tree
  attr(comb,'tree_not_data') <- tree_not_data
  class(comb)<-"treedata.table"
  return(comb)
}
