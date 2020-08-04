#' Returning a named vector from a treedata.table object
#'
#' @param tdObject A treedata.table object
#' @param ... The name of the column to select
#'
#' @return A named vector
#'
#' @examples
#'
#' data(anolis)
#' td <- as.treedata.table(tree=anolis$phy, data=anolis$dat)
#' extractVector(td, "SVL") #extracts the named vector for SVL from the td object
#'
#' @export

extractVector <- function(tdObject, ...){

  if(!inherits(tdObject, c('treedata.table'))){
    stop("Please use a class 'treedata.table' object \n")
  }

  dat <- tdObject$dat
  args <- as.character(substitute(list(...)))[-1L]
  arg_sub <- utils::type.convert(args)
  if(is.numeric(arg_sub) | is.integer(arg_sub)) args <- arg_sub
  vecs <- lapply(args,function(x) dat[[x]])
  vecs <- if(inherits(tdObject$phy, c('phylo')) ){
    lapply(vecs, function(x) stats::setNames(x, tdObject$phy$tip.label)) }else{
      lapply(vecs, function(x) stats::setNames(x, tdObject$phy[[1]]$tip.label))
    }
  if(length(vecs) == 1){
    vecs <- vecs[[1]]
  } else {names(vecs) <- args}
  return(vecs)
}
