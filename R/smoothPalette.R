#' Create a gradient corresponding to a numeric vector
#'
#' Create a gradient corresponding to a numeric vector
#'
#' This function "converts" numbers to colors. It generates a gradient based on a particular palette
#' function in which each color corresponds to a value or value range in a
#' numeric vector. The lowest value (or the value equal to min, if
#' specified) corresponds to one extreme of the gradient, and the highest (or
#' the value equal to max, if specified) to the other extreme of the gradient.
#' @param x a numeric vector
#' @param max value which corresponds to the right-hand side of the color
#' gradient
#' @param min value which corresponds to the left-hand side of the color
#' @param palfunc palette function as returned by colorRampPalette. By
#' default, it is a function generating a gradient from blue through white to
#' red.
#' @param n how many steps in the gradient
#' @param symmetrical whether the palette should be symmetrical around 0
#' (with default palette, that means that "0" corresponds to the color "white"
#' @export
smoothPalette <- function( x, max= NULL, min= NULL, palfunc= NULL, n= 10, symmetrical= FALSE ) {

  require( RColorBrewer )
  if( missing( palfunc ) ) {
    palfunc <- colorRampPalette( c( "blue", "white", "red" ) )
  }

  pal <- palfunc( n )
  nas <- which( is.na( x ) )

  x[ nas ] <- mean( x, na.rm= T )

  if( missing( max ) ) max <- max( x )
  x[ x >= max ] <- max

  if( missing( min ) ) min <- min( x )
  x[ x <= min ] <- min

  if( symmetrical ) {
    mm <- max( abs( c( min, max ) ) )
    max <- mm
    min <- -mm
  }


  ret <- findInterval( x, seq( min, max, length.out= n + 1 ), rightmost.closed= T )
  ret <- pal[ ret ]
  ret[ nas ] <- "white"
  return( ret )
}
