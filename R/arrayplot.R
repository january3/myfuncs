#'@export
#'@rdname arrayplot
spatial.avg <- function( mat, window= 20 ) {

  window <- window - 1
  nr <- nrow( mat )
  nc <- ncol( mat )

  new.mat <- matrix( 0, nrow= nr - window, ncol= nc - window )

  for( r in 1:(nr-window) ) {
    for( c in 1:(nc-window ) ) {
      new.mat[r,c] <- mean( mat[r:(r+window),c:(c+window)] )
    }
  }

  class( new.mat ) <- c( class( new.mat ), "arrayplot" )
  return( invisible( new.mat ) )
}


#'@rdname arrayplot
#'@method plot arrayplot
#'@export
plot.arrayplot <- function( x, ... ) {

  pal <- attr( x, "pal" )
  if( is.null( pal ) ) pal <- colorRampPalette( c( "black", "white" ) )(255)
  
  par.old <- par( mar= c( 0, 0, 0, 0 ) )
  on.exit( par( par.old ) )
  image( x, col= pal )
}


#' Show an image based on row/column information
#'
#' Show an image based on row/column information
#'
#' The image matrix is created from three columns containing the value to
#' plot and its position (row and column) on the image.
#'
#' \code{spatial.avg} can be used to average the image.
#'
#' @param rows integer vector with row positions
#' @param cols integer vector with column positions
#' @param vals numeric vector with values to plot
#' @param byrow should the image be plotted row-wise (default:TRUE)
#' @param n.row number of rows (if NULL, maximum value of rows)
#' @param pal Character vector to use as color palette for the image
#' @param bg.col how the missing values should be extrapolated
#' @param mat an object of class arrayplot
#' @param window window size to average
#' @return \code{spatial.avg} and \code{arrayplot} both return an object of class arrayplot.
#' @export
arrayplot <- function( rows, cols, vals, byrow= TRUE, n.row= max( rows ), n.col= max( cols ), 
                       pal= NULL, bg.col= "min"  ) {

  if( is.null( n.row ) ) n.row <- max( rows )
  if( is.null( n.col ) ) n.col <- max( cols )

  if( ( length( rows ) != length( cols ) ) || 
      ( length( cols ) != length( rows ) ) ) 
    stop( "All three vectors must be of the same length" )

  val.n <- paste( rows, cols, sep= "." )

  bg.col <- match.arg( bg.col, c( "min", "mean", "median", "max" ) )
  bg.col <- switch( bg.col,
      min=    min( vals ),
      mean=   mean( vals ),
      median= median( vals ),
      max=    max( vals ) )

  x <- rep( bg.col, n.row * n.col )
  
  if( byrow ) {
    x.n <- paste( rep( 1:n.row, each= n.col ), rep( 1:n.col, n.row ), sep= "." )
  } else {
    x.n <- paste( rep( 1:n.row, n.col ), rep( 1:n.col, each= n.row ), sep= "." )
  }

  x[ match( val.n, x.n ) ] <- vals

  if( is.null( pal ) ) pal <- colorRampPalette( c( "black", "white" ) )(255)

  mat <- t( matrix( x, nrow= n.row, ncol= n.col, byrow= byrow )[n.row:1,] )
  class( mat ) <- c( class( mat ), "arrayplot" )
  attr( mat, "pal" ) <- pal

  plot( mat )

  return( invisible( mat ) )
}
