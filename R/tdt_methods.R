##Need to check these two functions

summary.treedata.table <- function(object, ...){
  cat("A treedata.table object", "\n")
  cat(paste("The dataset contains ", ncol(object$dat), " traits"),
      "\n")
  types <- stats::setNames(suppressWarnings(detectAllCharacters(as.matrix(object$dat))),
                    colnames(object$dat))
  cat("Continuous traits: ", names(types)[which(types == "continuous")],
      "\n")
  cat("Discrete traits: ", names(types)[which(types == "discrete")],
      "\n")
  cat(paste("The following traits have missing values:", paste(names(types)[apply(object$dat,
                                                                                  2, function(y) any(is.na(y)))], collapse = ", "), "\n"))
  cat(paste("These taxa were dropped from the tree:", paste(attributes(object)$tree_not_data,
                                                            collapse = ", "), "\n"))
  cat(paste("These taxa were dropped from the data:", paste(attributes(object)$data_not_tree,
                                                            collapse = ", "), "\n"))
  cat("$phy \n")
  print(object$phy)
  cat("\n$dat \n")
  print(object$dat)
  if( is.null(attr(object, "modified"))==F ){ message("\n    This is NOT the original treedata.table object") } ##Include warning for treedata.objects that were modified
}

print.treedata.table <- function(x, ...){
  cat("$phy \n")
  print(x$phy)
  cat("\n$dat \n")
  print(x$dat)
}


head.treedata.table <- function(x, ...){
  fun = utils::getFromNamespace("head.data.table", "data.table")
  fun(x$dat)
  #data.table:::head.data.table(x$dat, ...)
}


