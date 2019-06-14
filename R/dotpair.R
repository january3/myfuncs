#' A variant of the pairs plot
#'
#' A variant of the pairs plot
#'
#' A wrapper for the pairs plot which shows a plot in the upper panel and
#' correlation coefficient with p-value in the lower panel
#'@param x data frame or matrix
#'@param xlim,ylim ranges for the plot
#'@param main Title of the plot
#'@param ... additional parameters passed to the \code{plot} function
#'@export
dotpair <- function( x, xlim= NULL, ylim= NULL, main= NULL, cor.method= NULL, ... ) {

  lower.panel <- function( x, y ) {
    par( new= T ) 
    plot.new()
    usr <- par( "usr" ) 
    on.exit( par( usr ) ) 
    par( usr= c( 0, 1, 0, 1 ) ) 
    cc <- cor.test( x, y, method= cor.method )
    text( 0.5, 0.5, sprintf( "%.2f", cc$estimate ), cex= 1 + abs( cc$estimate ) ) 
    text( 0.5, 0.2, pval2str( cc$p.value ), cex= 0.5 + abs( cc$estimate ) / 2 )
  }


  xlim0 <- xlim
  ylim0 <- ylim

  upper.panel <- function( x, y, xlim= xlim0, ylim= ylim0 ) {
    par( new= T ) 
    if( is.null( xlim ) ) xlim= range( x )
    if( is.null( ylim ) ) ylim= range( y )
    ll <- loess( y ~ x )
    o <- order( x )
    plot( x, y, xlim= xlim, ylim= ylim, ... ) 
    lines( x[o], predict( ll )[o], col= "#cccccc" ) 
  }




  pairs( x, lower.panel= lower.panel, upper.panel= upper.panel, main= main  )


}
