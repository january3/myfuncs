#' @rdname lineq
#' @export
lineq.above <- function( lineq, m ) {

  x <- m[,1]
  y <- m[,2]

  sel <- y > lineq$a * x + lineq$b
  return( sel )
}


#' Parameters of a line drawn on a plot
#'
#' Parameters of a line drawn on a plot
#'
#' Interactively draw a line on the plot and return the parameters of that
#' line. Useful for determining the points that are above or below the line
#'
#' \code{lineq.above} takes the output of \code{lineq} and coordinates of
#' some points on the plot and returns a logical vector indicating the points
#' that lie above that line
#' @param lineq List returned by \code{lineq}
#' @param m Matrix with two columns giving the coordinates of points on a
#' plot
#' @return \code{lineq} returns a list with intercept and coefficient of
#' the line drawn between the two clicks of the user. \code{lineq.above}
#' returns a logical vector indicating the points
#' that lie above that line.
#' @export
lineq <- function( ) {

  cat( "Click twice on the plot\n" )
  p <- locator( 2 )
  cat( "Thank you" )

  a <- ( p$y[2] - p$y[1] ) / ( p$x[2] - p$x[1] )
  b <- p$y[2] - a * p$x[2]

  abline( b, a )
  return( list( a= a, b= b ) )

}
