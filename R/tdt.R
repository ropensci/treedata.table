treedata.table <- function(tdObject, ...){
  UseMethod("treedply")
}

treedata.table <- function(tdObject, ...){
  if(!is.call(substitute(...))){
    call <- list(...)[[1]]
  } else {
    call <- substitute(...)
  }
  env <- new.env(parent = parent.frame(), size = 1L)
  env$phy <- tdObject$phy
  env$dat <- tdObject$dat
  out <- eval(call, env)
  if(is.null(out)){
    invisible()
  } else {
    return(out)
  }
}


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


treedata.table(td, geiger::fitContinuous(phy, extractVector(td, SVL), model="BM", ncores=1))
treedata.table(td, geiger::fitContinuous(phy, extractVector(td, awesomeness), model="BM", ncores=1))
treedata.table(td, phytools::phenogram(phy, extractVector(td, SVL), quiet=TRUE, spread.labels=FALSE))
treedata.table(td, list("K" = phytools::phylosig(phy, extractVector(td, SVL), "K"),
                  "lambda" = phytools::phylosig(phy, extractVector(td, SVL), "lambda")))

treedata.table(td, geiger::rescale, model="OU", 10) ##I'm still wotking on this one




