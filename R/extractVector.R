#' Returning a named vector from a treedata.table object
#'
#' @param td A treedata.table object
#' @param ... The name of the column to select
#'
#' @return A named vector
#' @export

extractVector <- function(td, ...){
  dat <- td$dat
  args <- as.character(substitute(list(...)))[-1L]
  arg_sub <- type.convert(args)
  if(is.numeric(arg_sub) | is.integer(arg_sub)) args <- arg_sub
  vecs <- lapply(args,function(x) dat[[x]])
  vecs <- lapply(vecs, function(x) setNames(x, td$phy$tip.label))
  if(length(vecs)==1){
    vecs = vecs[[1]]
  } else {names(vecs) <- args}
  return(vecs)
}
