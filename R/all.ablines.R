#' Plot vertical, horizontal and diagonal ablines
#'
#' Plot vertical, horizontal and diagonal ablines
#'
#' Plot vertical, horizontal and diagonal ablines
#'@param h height at which the horizontal abline is plotted
#'@param v x position at which the vertical abline is plotted
#'@param a intercept for the diagonal abline
#'@param b slope for the diagonal abline
#'@param col color
#'@param ... additional parameters passed to abline
#'@examples
#' plot(-10:10, -10:10)
#' all_ablines()
#'@export
all_ablines <- function( h=0, v=0, a=0, b=1, col= "#666666", ... ) {

  abline( 0, 1, col= col, ... )
  abline( v= 0, col= col, ... )
  abline( h= 0, col= col, ... )

}
