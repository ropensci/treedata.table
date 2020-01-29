#' Return the first or last part of an treedata.table object
#'
#' @param x a treedata.table object
#' @param n a single integer. If positive or zero, number of rows
#' for the resulting object. If negative, all but the n last/first
#' rows of x.
#' @param ... Additional arguments passed to head.data.table
#'
#' @export
head.treedata.table <- function(x, n=6L, ...){
  uhead <- utils::head
  stopifnot(length(n) == 1L)
  i = seq_len(if (n < 0L) max(nrow(x$dat) + n, 0L) else min(n,
                                                        nrow(x$dat)))
  x$dat[i, , ]
  #fun = utils::getFromNamespace("head.data.table", "data.table")
  #fun(x$dat, n=n, ...)
  #data.table:::head.data.table(x$dat, ...)
}

#' Print method treedata.table objects
#'
#' @param x an object of class "treedata.table"
#' @param ... additional arguments passed to "head.treedata.table"
#'
#' @return Function uses prints the tree and the first lines of the
#' data.table object.
#'
#' @export
print.treedata.table <- function(x, ...){
  cat("$phy \n")
  print(x$phy)
  cat("\n$dat \n")
  print(head(x, ...))
}


#' Summarizing treedata.table objects
#'
#' @param object an object of class "treedata.table"
#' @param ... additional arguments passed to "head.treedata.table"
#'
#' @return Function tries to be smart about summarizing the data
#' and detecting continuous vs. discrete data, as well as whether
#' any data have missing data. Also returns the taxa that are
#' dropped from either the original tree or the original data.
#'
#' @export
summary.treedata.table <- function(object, ...){
  if(class(object) != "treedata.table" ){
    stop("Please use a class 'treedata.table' object \n")
  }

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
  print(object, ...)
  if( is.null(attr(object, "modified"))==F ){ message("\n    This is NOT the original treedata.table object") } ##Include warning for treedata.objects that were modified
}




