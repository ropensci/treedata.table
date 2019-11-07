library(data.table)
library(geiger)
library(diversitree)
library(treeplyr)
set.seed(4)

##Sample data from data.table
input <- if (file.exists("flights14.csv")) {
  "flights14.csv"
} else {
  "https://raw.githubusercontent.com/Rdatatable/data.table/master/vignettes/flights14.csv"
}
flights <- fread(input)

##Sample tree + data
pars <- c(0.1, 0.2, 0.03, 0.03, 0.01, 0.01)
phy <- tree.bisse(pars, max.t=30, x0=0)
df<-data.frame(tip.labels=names(phy$tip.state),one=sample(1:5,length(phy$tip.state), replace = T),
               two=sample(1:5,length(phy$tip.state), replace = T), three=sample(1:7,length(phy$tip.state), replace = T),
               four=sample(1:5,length(phy$tip.state), replace = T))

##phy.data.table functions
as.phy.data.table<-function(tree, data){
  if(class(tree) != "phylo"){stop("Please use a class 'phylo' tree \n")}
  if(class(data) != "data.frame"){stop("Your data MUST be of class data.frame")}
  if(dim(data)[2] < 2){stop("Your data MUST have at least two columns (tip.names; nstates)")}
  ##Name check between tree and data.frame
  if(name.check(tree, data.names = data[,1] )[1] != "OK"){
    data_not_tree <- setdiff(as.character(data[,1]), tree$tip.label)
    tree_not_data <- setdiff(tree$tip.label, data[,1])
  }else{
    data_not_tree <- "OK"
    tree_not_data <- "OK"
  }
  comb<-list(phylo=tree, dat=as.data.table(data),
             data_not_tree=data_not_tree,
             tree_not_data=tree_not_data,
             #I need an ID for original/mod objects
             classD="original")
  class(comb)<-"phy.data.table"
  return(comb)
}
summary.phy.data.table<- function(object, ...){
  cat('A phy.data.table treedata object', "\n")
  cat(paste('The dataset contains ', ncol(object$dat), ' traits'), "\n")
  types <- setNames(suppressWarnings(detectAllCharacters(as.matrix(object$dat))), colnames(object$dat))
  cat("Continuous traits: ", names(types)[which(types=="continuous")], "\n")
  cat("Discrete traits: ", names(types)[which(types=="discrete")], "\n")
  cat(paste("The following traits have missing values:", paste(names(types)[apply(object$dat, 2, function(y) any(is.na(y)))], collapse=", "), "\n"))
  if(object$classD == "original"){ cat(paste("The tips are in the tree but not the matrix:", paste(object$tree_not_data , collapse=", "), "\n")) }
  if(object$classD == "original"){ cat(paste("The tips are in the matrix but not in tree:", paste(object$data_not_tree , collapse=", "), "\n")) }
  if(object$classD != "original"){cat(paste("These taxa were dropped from the tree:", paste(attributes(object)$dropped$dropped_from_tree, collapse=", "), "\n"))}
  if(object$classD != "original"){cat(paste("These taxa were dropped from the data:", paste(attributes(object)$dropped$dropped_from_data, collapse=", "), "\n"))}
  cat("\n$phy \n")
  print(object$phy)
  cat("\n$dat \n")
  object$dat
}
DT.phylo<-function(object, i=NULL, j=NULL, by=NULL ){

  ##Create the subset

  if(!is.null(i)){
    s = parse(text=paste("(",i,")"))
    min_data<-object$dat[eval(s)]
    if(!is.null(j)){
      s = parse(text=paste("(",j,")"))
      min_data<-min_data[,eval(s)]
    }
  }

  if( is.null(i) & !is.null(j)){
    s = parse(text=paste("(",j,")"))
    min_data<-cbind(object$dat[,1], object$dat[,eval(s)])
  }


  return(min_data)

}



##tests
object<-as.phy.data.table(tree=phy, data=df)
summary.phy.data.table(object)
DT.phylo(object = object, i = c("two == 2"))
DT.phylo(object = object, i = c("3"))
DT.phylo(object = object, i = c("order(two, - four)"))
DT.phylo(object = object, i = c("two"))
DT.phylo(object = object, j = c("one"))
DT.phylo(object = object, j = c(" .(one,two)"))
DT.phylo(object = object, j = c(" one+three"))
DT.phylo(object = object, j = c(" sum( (one - two))> 1000"))
DT.phylo(object = object, i=c("three == 2L"),
         j = c(".(m_arr = mean(three), m_dep = mean(two))"))
DT.phylo(object = object, i = c("one == 1L"), j= c("length(two)"))



