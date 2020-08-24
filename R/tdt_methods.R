#' Return the first part of an treedata.table object
#'
#' @param x a treedata.table object
#' @param ... Additional arguments passed to head.data.table
#' @examples
#' data(anolis)
#' td <- as.treedata.table(anolis$phy, anolis$dat)
#' head(td)
#' @importFrom utils head
#' @export
head.treedata.table <- function(x, ...) {
  utils::head(x$dat, ...)
}


#' Return the last part of an treedata.table object
#'
#' @param x a treedata.table object
#' @param ... Additional arguments passed to head.data.table
#' @examples
#' data(anolis)
#' td <- as.treedata.table(anolis$phy, anolis$dat)
#' tail(td)
#' @importFrom utils tail
#' @export

tail.treedata.table <- function(x, ...) {
  utils::tail(x$dat, ...)
}

#' Print method treedata.table objects
#'
#' @param x an object of class "treedata.table"
#' @param ... additional arguments passed to "head.treedata.table"
#' @return Function uses prints the tree and the first lines of the
#' data.table object.
#' @importFrom utils head
#'
#' @export
print.treedata.table <- function(x, ...) {
  cat("$phy \n")
  print(x$phy)
  cat("\n$dat \n")
  print(utils::head(x$dat, ...))
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
#' @examples
#' data(anolis)
#' td <- as.treedata.table(anolis$phy, anolis$dat)
#' summary(td)
#' @export
summary.treedata.table <- function(object, ...) {
  if (!inherits(object, c("treedata.table"))) {
    stop("Please use a class 'treedata.table' object \n")
  }

  message("A treedata.table object", "\n")
  message(
    "The dataset contains ", ncol(object$dat), " traits",
    "\n"
  )
  types <- stats::setNames(
    suppressWarnings(detectAllCharacters(as.matrix(object$dat))),
    colnames(object$dat)
  )
  message(
    "Continuous traits: ", paste(names(types)[which(types == "continuous")],
      collapse = ", "
    ), "\n"
  )
  message(
    "Discrete traits: ", paste(names(types)[which(types == "discrete")],
      collapse = ", "
    ), "\n"
  )

  obswm <- names(types)[apply(
    object$dat,
    2, function(y) any(is.na(y))
  )]

  message(
    "The following traits have missing values:",
    ifelse(length(obswm) == 0, " 0",
      paste(obswm, collapse = " ,")
    ), "\n"
  )



  message(
    "Taxa dropped from the tree: ",
    ifelse(attributes(object)$tree_not_data[1] == "OK",
      " 0", paste(attributes(object)$tree_not_data,
        collapse = ", "
      )
    ), "\n"
  )
  message(
    "Taxa dropped from the data: ",
    ifelse(attributes(object)$data_not_tree[1] == "OK",
      " 0", paste(attributes(object)$data_not_tree,
        collapse = ", "
      )
    ),
    "\n"
  )
  print(object, ...)
  if (!is.null(attr(object, "modified"))) {
    message("\n    This is NOT the original treedata.table object")
  }
}
