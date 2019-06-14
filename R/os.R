#' Report memory usage
#' 
#' Report memory usage
#' 
#' Report memory usage for the largest objects defined.
#' @param n Number of largest objects to show in output
#' @param x Name of the object for which to show the output
#' @export
os <- function( n= 10, x= NULL ) { 
  if( missing( x ) ) x <- ls( envir=.GlobalEnv ) ; 

  z  <- sapply( x, function( xx ) as.integer( object.size( get( xx ) ) / 1024 ) ) ; 
  z2 <- as.matrix(rev(sort(z)))
  colnames( z2 ) <- "Kb"
  n <- min( nrow( z2 ), n )

  cat( "-------------------------------------\n" )
  print( z2[1:n,,drop= F] )

  cat( "-------------------------------------\n" )
  cat( sprintf( "Total: %d Kb\n", sum( z2[,1] ) ) )
}
