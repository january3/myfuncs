#' Alternative to plotDensities from the limma package.
#'
#' Alternative to plotDensities from the limma package.
#'
#' Alternative to plotDensities from the limma package.
#' @param O object (e.g. limma) for plotting the densities
#' @param group Color the densities by group
#' @param col character vector specyfing the color(s) to use for plotting
#' @param labels Labels to add at the density maxima
#' @param log Logarithmize the values first
#' @param xlim,ylim ranges for the plot
#' @param lwd line width to plot with
#' @param ... additional parameters passed to the \code{plot} command
#' @export 
myPlotDensities <- function( O, group= NULL, log= F, labels= NULL, col= NULL, xlim= NULL, ylim= NULL, lwd=1, ... ) {



  densXY <- function(Z) {
    zd <- density(Z, na.rm = TRUE)
    x <- zd$x
    y <- zd$y
    cbind(x, y)
  }

  if( class( O ) %in% c( "EList", "EListRaw" ) ) {
    O <- O$E
  }


  n <- ncol( O )
  x <- NULL
  if( ! missing( labels ) ) {
    labnames <- labels
    labels   <- T
  } else {
    labels <- F
  }

  if( log ) O <- log( O )

  d <- apply( O, 2, densXY )
  d.x <- d[ 1:(nrow( d )/2), ]
  d.y <- d[ (nrow( d )/2 + 1):(nrow( d ) ), ]

  if( is.null( xlim ) ) xlim <- range( d.x )

  if( is.null( ylim ) ) ylim <- range( d.y )

  plot( NULL, NULL, type= "n", xlim= xlim, ylim= ylim, xlab="", ylab= "Density", ... )

  if( ! is.null( col ) & length( col ) == 1 ) col <- rep( col, n )

  if( ! is.null( group ) ) {
    if( ! is.factor( group ) ) group <- factor( group )
    if( is.null( col ) ) col <- group 
  } else {
    if( is.null( col ) ) {
      col <- rep( "black", n ) 
    }
  }

  for( i in 1:n ) {
    if( labels ) {
      if( ! is.na( labnames[i] ) & ! labnames[i] == "" ) {
        text( d.x[ which.max( d.y[,i] ), i ], max( d.y[,i] ), labnames[i], pos= 4, col= "#00000033" )
        points( d.x[ which.max( d.y[,i] ), i ], max( d.y[,i] ), pch= 19, col= col[i] )
      }
    }
    lines( d.x[,i], d.y[,i], col= col[i], lwd= lwd )
  }

  return( invisible( list( d.x= d.x, d.y= d.y ) ) )
}
