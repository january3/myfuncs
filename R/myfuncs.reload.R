#' Reload the myfuncs package
#'
#' Reload the myfuncs package
#'
#' Reload the myfuncs package
#' @export
myfuncs.reload <- function() {
  unloadNamespace( "myfuncs" )
  require( myfuncs )
}
