#' Function to detect whether a character is continuous or discrete
#'
#' This function detects whether a given vector is a continuous
#' (e.g., with values 2.45, 9.35, and so on) or a discrete
#' (e.g., with values blue, red, yellow) character.
#'
#' @param dat A vector of data
#' @param cutoff Cutoff value for deciding if numeric data might actually be
#' discrete: if nlev is the number of levels and n the length of dat, then
#' nlev / n should exceed cutoff, or the data will be classified as discrete
#' @return Either "discrete" or "continuous"
#' @examples
#' data(anolis)
#' detectCharacterType(anolis$dat[, 1])
#' @export
detectCharacterType <- function(dat, cutoff = 0.1) {
  if (is.factor(dat)) {
    charType <- "discrete"
  } else if (nlevels(as.factor(dat)) / length(dat) < cutoff) {
    warning("Guessing this is a discrete character based on repeated values")
    charType <- "discrete"
  } else {
    charType <- "continuous"
  }
  return(charType)
} # needless to say, this is not yet robust

#' Apply detectCharacterType over an entire matrix
#'
#' This function detects whether each column in a matrix is a continuous
#' (e.g., with values 2.45, 9.35, and so on) or a discrete character
#' (e.g., with values blue, red, yellow).
#'
#' @param mat A matrix of data
#' @param cutoff Cutoff value for deciding if numeric data might actually be
#' discrete: if nlev is the number of levels and n the length of dat, then
#' nlev / n should exceed cutoff, or the data will be classified as discrete
#' @return Vector of either "discrete" or "continuous" for each variable in
#' matrix
#' @examples
#' data(anolis)
#' detectAllCharacters(anolis$dat)
#' @export
detectAllCharacters <- function(mat, cutoff = 0.1) {
  nchar <- dim(mat)[2]
  result <- numeric(nchar)
  for (i in seq_len(nchar)) {
    result[i] <- detectCharacterType(mat[, i], cutoff)
  }
  return(result)
}

#' Filter a character matrix, returning either all continuous or all discrete characters
#'
#' This function filters a character matrix based on continuous
#' (e.g., with values 2.45, 9.35, and so on) or discrete characters
#' (e.g., with values blue, red, yellow).
#'
#' @param mat A character matrix of class data.frame
#' @param returnType Either discrete or continuous
#' @return data.frame with only discrete (default) or continuous characters
#' @examples
#' data(anolis)
#' filterMatrix(anolis$dat, "discrete")
#' @export

filterMatrix <- function(mat, returnType = "discrete") {
  rType <- match.arg(returnType, c("discrete", "continuous"))
  columnFilter <- detectAllCharacters(mat) == rType
  result <- mat[, columnFilter]
  return(result)
}

#' Row and column name check
#'
#' This function checks whether a given `data.frame` or `matrix` has
#' column names (`colnames`), row.names (`row.names`), or both.
#'
#' @param dat A vector of data
#' @param nameType, either:
#' \describe{
#'     \item{"row"}{Rows (default)}
#'   	 \item{"col"}{Columns}
#' 		 \item{"rowcol"}{Both rows and columns}
#' 	}
#' @return `TRUE` or `FALSE` indicating if the object has names (`columns`,
#'                                                               `rows`, or
#'                                                               `both`)
#' @examples
#' data(anolis)
#' hasNames(anolis$dat, "row")
#' @export
hasNames <- function(dat, nameType = "row") {
  nType <- match.arg(nameType, c("row", "col", "rowcol"))
  if (nType == "row") {
    res <- !is.null(rownames(dat))
  }
  if (nType == "col") {
    res <- !is.null(colnames(dat))
  }
  if (nType == "rowcol") {
    res <- !is.null(rownames(dat)) & !is.null(colnames(dat))
  }
  names(res) <- nameType
  res
}

#' Force names for rows, columns, or both
#'
#' This function creates column names (`colnames`), row.names (`row.names`),
#' or both in an unnamed `data.frame` or `matrix`.
#'
#' @param dat A vector of data
#' @param nameType, either:
#' \describe{
#'     \item{"row"}{Rows (default)}
#' 		 \item{"col"}{Columns}
#' 		 \item{"rowcol"}{Both rows and columns}
#' 	}
#' @examples
#' data(anolis)
#' forceNames(anolis$dat, "row")
#' @return An object of type `data.frame with labeled columns, rows, or both.
#' @export
forceNames <- function(dat, nameType = "row") {
  nType <- match.arg(nameType, c("row", "col", "rowcol"))
  if (nType == "row" | nType == "rowcol") {
    if (!hasNames(dat, nameType = "row")) {
      nrows <- dim(dat)[1]
      rownames(dat) <- paste("n", seq_along(dat[, 1]), sep = "")
    }
  }
  if (nType == "col" | nType == "rowcol") {
    if (!hasNames(dat, nameType = "col")) {
      ncols <- dim(dat)[2]
      colnames(dat) <- paste("n", seq_along(dat[1, ]), sep = "")
    }
  }

  dat
}
