#' Browse a large object
#'
#' Browse a large object
#'
#' Browse a large object
#'@param x object to display
#'@param limit.width whether use the current width option
#'@export
browse <- function( x, limit.width= TRUE ) {

  mp <- options( c( "max.print", "width" ) )
  on.exit( options( mp ) )
  options( max.print=1e9 )
  if( ! limit.width ) options( width=1000 )

  tf <- tempfile()

  sink( tf )
  print( x )
  sink()
  file.show( tf )

}
