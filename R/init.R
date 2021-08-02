.onAttach <- function(libname, pkgname) {
  message <- c(
    "       ",  "  Thank you for using the {treedata.table} R package!   ",
    "\n", "\n                        ", "\U0001F642",
    "Happy coding!!", "\U0001F642"
  )
  packageStartupMessage(paste(message, collapse = ""))
}
