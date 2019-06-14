#' @rdname pca2var
#' @export
plotPcaVar <- function( p2v, add= FALSE, ... ) {

  l <- length( p2v )

  ylab <- "Fraction of variance explained" 
  if( attr( p2v, "cumulative" ) ) ylab <- "Cummulative faction of variance explained" 

  if( add ) {
    points( 1:l, p2v, type= "b", xlab= "Component", ylab= ylab, ... )
  } else {
    plot( 1:l, p2v, type= "b", xlab= "Component", ylab= ylab, ... )
  }

}

#' Calculate the variance explained for prcomp objects
#'
#' Calculate the variance explained for prcomp objects
#'
#' Calculate the variance explained for prcomp objects
#' @param pca a prcomp object
#' @param cumulative whether a cumulative fraction of variance should be calculated
#' @param p2v object returned by \code{pca2var}
#' @export
pca2var <- function( pca, cumulative= FALSE ) {

  if( class( pca ) != "prcomp" ) stop( "pca must be a prcomp object" )
  v <- pca$sdev ^ 2
  if( cumulative ) {
    ret <- cumsum( v ) / sum( v )
    attr( ret, "cumulative" ) <- TRUE
  } else {
    ret <- v / sum( v )
    attr( ret, "cumulative" ) <- FALSE
  }

  class( ret ) <- c( class( ret ), "pca2var" )
  return( ret )
}
