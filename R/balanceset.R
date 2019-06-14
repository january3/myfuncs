#' Balance samples
#'
#' Balance samples
#'
#' Given a factor, sample the overrepresented groups such that each factor
#' level is represented the same number of times.
#' The function counts the number of occurences of the factor levels, and
#' resamples each group with number of occurences higher than minimal.
#' @param n optionally, how many samples should be in each group
#' @param x a factor
#' @param replace whether sampling with replacement
#' @export
balanceset <- function( x, n=NULL, replace=FALSE ) {

  if( ! is.factor( x ) ) x <- factor( x )

  counts <- summary( x )
  min.c  <- min( counts )
  min.c.n <- names( counts )[ which.min( counts ) ]

  sel <- which( x == min.c.n )
  num.min <- length( sel )
  if(!is.null(n)) 
    n <- num.min

  # catf( "\n%s %d %d\n", min.c.n, min.c, num.min )

  for( l in levels( x )[ levels( x ) != min.c.n ] ) {
    sel <- c( sel, sample( which( x == l ), num.min, replace ) )
  }

  return( sel )
}

