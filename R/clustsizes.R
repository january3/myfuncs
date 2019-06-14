#' calculate sizes of clusters
#'
#' calculate sizes of clusters
#'
#' calculate sizes of clusters
#'@export
clustsizes <- function( clusters ) {

  n <- unique( clusters )

  s <- sapply( n, function( x ) length( which( clusters == x ) ) )
  names( s ) <- n
  return( s )
}
