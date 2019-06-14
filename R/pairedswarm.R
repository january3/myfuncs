#' Beeswarm with lines linking pairs
#'
#' Beeswarm with lines linking pairs
#'
#' Beeswarm with lines linking pairs.
#' 
#'
#' @param y values to be plotted
#' @param g group assignments
#' @param pairs vector of length equal to number of data points plotted,
#' indicating the pair ID for each data point
#' @param color of the lines to draw
#' @param ... Any further parameters to pass to \code{beeswarm}
#' @export
pairedswarm <- function( y, g, pairs= NULL, pwcol= NULL, lcol="grey", ... ) {
  require( beeswarm )

  .pairedswarm( y, g, pairs, pwcol, paircol, ... )
}

#' Beeswarm with lines linking pairs
#'
#' Beeswarm with lines linking pairs
#'
#' Beeswarm with lines linking pairs
#' @param x x positions of the points
#' @param y y positions of the points
#' @param pairs factor defining the pairs
#' @export
linepairs <- function( x, y, pairs, ... ) {

  if( ! class( pairs ) == "factor" ) stop( "pairs must be a factor" )
  pids <- unique( pairs )
  x <- as.numeric( x )
  notnas <- ! (is.na( x ) | is.na( y ))

  for( p in pids ) {

    sel <- pairs == p & notnas
    if( sum( sel  ) != 2 ) {
      printf( "skipping %s", p )
      next ;
    }
    lines( x[sel], y[sel] )

  }


}

.pairedswarm <- function( y, g, pairs= NULL, pwcol= NULL, paircol= NULL, pch= 19, lcol=NULL,
  xlab= "", ylab= "", xlim= c( 0.75, 2.25 ), ... ) {
  #print( dupa )
  if( class( g ) != "factor" ) g <- factor( g )
  if( class( pairs ) != "factor" ) pairs <- factor( pairs )
  .default.color <- "#33333366"
  #if( length( unique( g ) ) > 2 ) stop( "Incorrect formula" )

  if( is.null( pwcol ) ) .pwcol <- .default.color
  else                   .pwcol <- pwcol

  beeswarm( y ~ g, pwcol=pwcol, xlab=xlab, ylab=ylab, pch=pch, ... )

  if( !is.null( pairs ) ) {
    linepairs( x=g, y=y, pairs=pairs, col=lcol )
  }

# plot( as.numeric( g ), x, 
#   pch= 19, xaxt= "n", bty= "n", col= .pwcol, 
#   xlim= xlim,
#   xlab= "", ylab= "", ... )
#
# axis( 1, at= c( 1, 2 ), labels= levels( g ) )
#
# if( ! is.null( pairs ) ) {
#   print( "showing pairs" )
#
#   for( p in unique( pairs ) ) {
#
#     sel <- which( pairs == p )
#     if( length( sel ) != 2 ) next 
#
#     if( ! is.null( paircol ) & ! is.null( paircol[ p ] ) ) .col= paircol[p]
#     else .col= .default.color
#
#     lines( g[ sel ], x[ sel ], col= .col )
#
#   }
# }

}



fx  <- function( f, col= NULL ) beeswarm( f )
