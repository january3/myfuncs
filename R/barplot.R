#' Draw whiskers on a plot
#'
#' Draw whiskers on a plot
#'
#' Draw whiskers at specified positions with given heights and breadth on a plot
#'@param x, y: position of the whisker midpoint
#'@param w, h: half of the total height and width of the whisker
#'@export
#'@rdname sdbar
sdbar <- function( x, y, w, h ) { 
  segments( c( x - w, x - w, x ), c( y - h, y + h, y - h ), c( x + w, x + w, x ), c( y-h, y+h, y+h ) ) 
}

#' A variant box and whiskers plot
#'
#' A variant box and whiskers plot
#'
#' A variant box and whiskers plot
#'@export
#'@rdname mybarplot
mybarplot <- function( data, groups= NULL, col= NULL, whiskers= NULL, ylim= NULL, ... ) {

  n <- length( data )

  if( is.null( groups ) ) { groups <- rep( 1, n ) }
  ng     <- length( unique( groups ) ) 
  groups <- factor( groups, levels= unique( groups ) )
  if( length( groups ) != n ) stop( "Incorrect groups parameter" )
  print( groups )

  xpos <- c()
  x <- 1


  for( g in unique( groups ) ) {
    gs <- sum( groups == g )
    xpos <- c( xpos, x:(x+gs-1) )
    x <- x + gs + 1
  }


  xpos <- xpos[ match( 1:n, order( groups ) ) ]

  if( is.null( ylim ) ) {
    if( is.null( whiskers ) ) { ylim <- c( min( c( 0, data ) ), max( data ) ) }
    else { ylim <- c( min( c( 0, data - whiskers ) ), max( data + whiskers ) ) }
  }

  if( is.null( col ) )  col <- rep( "white", n )
  plot( NULL, xlim= c( 0.5, n + ng ),  ylim= ylim, 
    xaxt= "n", yaxt= "n", 
    bty= "n", 
    ... )

  #sapply( 1:n, function( i ) rect( xpos[i], ylim[1], xpos[i]+1, data[i], col= col[i] ) )
  rect( xpos, rep( ylim[1], n ), xpos+1, data, col= col )

  if( ! is.null( whiskers ) ) {
    sdbar( xpos + 0.5, data, 0.15, whiskers )
  }


  axis( side= 2 )
}
