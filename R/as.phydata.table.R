as.phydata.table<-function(tree, data){
  if(class(tree) != "phylo"){stop("Please use a class 'phylo' tree \n")}
  if(class(data) != "data.frame"){stop("Your data MUST be of class data.frame")}
  if(dim(data)[2] < 2){stop("Your data MUST have at least two columns (tip.names; nstates)")}
  if(name.check(tree, data.names = data[,1] )[1] != "OK"){
    data_not_tree <- setdiff(as.character(data[,1]), tree$tip.label)
    tree_not_data <- setdiff(tree$tip.label, data[,1])
  }else{
    data_not_tree <- "OK"
    tree_not_data <- "OK"
  }
  comb<-list(phylo=tree, dat=as.data.table(data))
  attr(comb,'data_not_tree') <- data_not_tree
  attr(comb,'tree_not_data') <- tree_not_data

  class(comb)<-"phydata.table"
  return(comb)
}
