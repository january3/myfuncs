#' Simple function to plot gene expression
#'
#' Simple function to plot individual gene expression for a number of genes
#'
#' @param x A matrix containing gene expression data (row-wise: genes; col-wise: samples)
#' @param center Whether gene expressions should be centered to mean= 0
#' @param scale  Whether gene expressions should be scaled to sd= 1 
#' @param labels Labels for the x-axis
#' @param xval Values for the x-axis
#' @param sel Selection of rows from the matrix to display
#' @param aveby If there are replicates that should be averaged before
#' plotting, this vector should contain IDs unique for a batch, but identical
#' between replicates in a batch, of length equal to number of columns in
#' \var{x}
#' @param xlim,ylim limits for x- and y- axes
#' @param col default color to use in plotting
#' @param lwd default line width to use in plotting
#' @param sdcol color used to plot the standard deviations if \var{aveby}
#' is not NULL
#' @param add Whether the existing plot should be updated rather than
#' replaced
#' @param las use las=2 to make sure x-axis labels are vertical
#' @param genelabels labels shown on the left for each gene plotted. Must
#' have a length equal to number of rows in \var{x}.
#' @param debug Switch on debugging
#' @param xlab Label for the x axis
#' @param ylab Label for the y axis
#' @param hline Whether a grey line should be drawn at h=0
#' @param vlines Whether grey vertical lines should be drawn at each sample
#' @param ... Additional parameters will be passed to \code{plot}
#' @export
plotGenes <- function( x, center= TRUE, scale= TRUE, 
  labels= NULL, xval= NULL, sel= NULL,
  aveby= NULL, xlim= NULL, ylim= NULL,
  col= "#33333366", sdcol= NULL, lwd= 1,
  add= FALSE, las= 2, 
  genelabels= rownames( x ),
  debug= FALSE,
  xlab= "", ylab= "",
  hline= 0,
  vlines=T,
  ... ) {


  if( is.null( labels ) ) labels <- colnames( x )


  if( scale ) {
    if( center ) {
      x <- t( scale( t( x ), center= TRUE, scale= TRUE ) )
    } else {
      x <- t( scale( t( x ), center= FALSE, scale= TRUE ) )
    }
  } else {
    if( center ) {
      x <- t( scale( t( x ), center= TRUE, scale= FALSE ) )
    }
  }

  sds <- NULL
  if( ! is.null( aveby ) ) {

    sds <- repsapply( x, aveby, colwise= FALSE, func= function( x ) sd( x, na.rm= T ) )
    x   <- repsapply( x, aveby, colwise= FALSE, func= function( x ) mean( x, na.rm= T ) )
    labels <- unique( aveby )

  }


  if( is.null( ylim ) ) {
    if( ! is.null( aveby ) ) {
      ylim <- range( rbind( x - sds, x + sds ), na.rm= T )
    } else {
      ylim <- range( x, na.rm= T )
    }
  }
  n <- ncol( x )
  if( is.null( xval ) ) xval <- 1:n
  if( is.null( xlim ) ) {
    xlim <- range( xval )
  }

  if( is.null( sdcol ) ) sdcol <- "#cccccc11"
  if( is.null( col ) )   col   <- "#33333366"

  on.exit( dev.flush() )
  dev.hold()
  if( ! add ) {
    plot( NULL, type= "n", ylim= ylim, xlim= xlim, xaxt= "n", las= las, xlab= xlab, ylab= ylab, ... )
    axis( 1, at= 1:n, labels= labels, las= las )
    if( !is.null( hline ) ) abline( h= 0, col= "#33333333" )
    if( vlines ) sapply( 1:ncol( x ), function( i ) abline( v=i, col= "#33333333" ) )
  }

  if( ! is.null( sel ) ) {
    x <- x[ sel,, drop= F ]
    sds <- sds[ sel,, drop= F ]
  }

  if( length( col ) < nrow( x ) ) col <- rep( col[1], nrow( x ) ) # length( xval ) )

  catf( "Plotting %d genes\n", nrow( x ) )

  if( is.null( aveby ) ) {
    for( g in 1:nrow( x ) ) {
      lines( xval, x[ g, ], col= col[ g ], lwd= lwd )
    }
    #apply( x, 1, function( g ) lines( xval, g, col= col ) )
  } else {
    for( g in 1:nrow( x ) ) {
      polygon( c( xval, rev( xval ) ), 
               c( x[g,] + sds[g,], rev( x[g,] - sds[g,] ) ),
               col= sdcol, border= NA )

    }
    apply( x, 1, function( g ) lines( xval, g, col= col, lwd= lwd ) )

  }

  if( ! is.null( genelabels ) ) {
    text( 0, x[,1], genelabels, pos= 4, cex= 0.7, col= col )
  }

  invisible( NULL )
}
