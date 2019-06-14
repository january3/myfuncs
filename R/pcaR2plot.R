#' Calculate R-squared for a matrix or principal components
#'
#' Calculate the fraction of variance explained by a predictor (numerical
#' or factor) for each of the columns in a matrix or for each principal
#' component in a prcomp object.
#'
#' Calculate the fraction of variance explained by a predictor (numerical
#' or factor) for each of the columns in a matrix or for each principal
#' component in a prcomp object.
#'
#' @param x a numeric matrix or object of class prcomp
#' @param predictor a factor or a numeric vector
#' @param randomize how many randomizations should be run
#' @param comps for which PCA components (or for which columns of the numeric matrix) the correlations should be calculated
#' @param scaled whether the data should be scaled
#' @export
pcaR2 <- function( x, predictor, randomize= 0, comps= NULL, scaled= FALSE ) {

  require( stats )



  r2 <- NULL
  if( class( x ) == "prcomp" ) {
    r2 <- x$sdev^2 / sum( x$sdev^2 )
    x <- x$x
  }

  if( is.null( r2 ) ) scaled <- FALSE

  if( !is.null( comps ) ) {
    if( !is.null( r2 ) ) r2 <- r2[ comps ]
    x <- x[ , comps ]
  }

  if( class( predictor ) != "factor" && class( predictor ) != "numeric" && class( predictor ) != "integer" ) {
    warning( "Converting predictor to factor" )
    predictor <- factor( predictor )
  }

  if( nrow( x ) != length( predictor ) )
    stop( "Incorrect parameter x" )


  getR2 <- function( x=NULL, predictor=NULL ) {
    lm.m <- lm( x ~ predictor )
    lm.s <- summary( lm.m )
    rs <- sapply( lm.s, function( x ) x$r.squared )
    return( rs )
  }

  rs <- list( r2= getR2( x, predictor ), nrandom= randomize )
  if( scaled ) rs$r2 <- rs$r2 * r2

  if( randomize > 0 ) {

    rand <- NULL

    for( i in 1:randomize ) {

      pred.rand <- sample( predictor )
      r2.rand   <- getR2( x, pred.rand )
      if( scaled ) r2.rand <- r2.rand * r2
      rand <- rbind( rand, r2.rand )

    }

    rs[["rand"]] <- rand
  }

  class( rs ) <- c( class( rs ), "pcaR2" )
  return( rs )
}

#' @rdname pcaR2
#' @method plot pcaR2
#' @export
plot.pcaR2 <- function( x, ... ) {

  .plot.pcaR2( x, ... )

}

.plot.pcaR2 <- function( x, xlim= NULL, ylim= NULL, rand.col= "#33333333", col="black", lty=1, lwd=1, show.labs=TRUE, ... ) {


  r2 <- x$r2
  n <- length( r2 )

  if( is.null( xlim ) ) xlim= c( 1, n )
  if( is.null( ylim ) ) {
    ylim <- range( r2 )
    if( x$nrandom > 0 ) ylim <- range( rbind( r2, x$rand ) )
  }
   

  plot( NULL, type= "l", xlim= xlim, ylim= ylim, ... )
  if( x$nrandom > 0 ) {

    apply( x$rand, 1, function( y ) lines( 1:n, y, col= rand.col ) )

  }

  lines( 1:n, r2, col=col, lty=lty, lwd=lwd )

  if(show.labs) text( 1:n, r2, 1:n, pos= 3 )


}

.pcaR2plot_plot <- function( comps, pca.var, ylim= c( 0, 1 ), 
                             xlab= "Components",
                             ylab= "Fraction of variance explained",
                             pch= 19,
                             col= "black", type= "b", ... ) {


  plot( comps, pca.var, ylim= ylim, xlab= xlab, ylab= ylab, col= col, type= type, pch= 19, ... )

}

.pcaR2plot_lines <- function( x, y, pch= 19, type= "b", col= "grey", lty=1, ... ) {

  lines( x, y, pch= pch, col= col, lty= lty, type= type )

}

#' Automatic R2 plot
#'
#' Plots fraction of explained variance for PCA components and 
#' some additonal explanatory variables
#'
#' Plots fraction of explained variance for PCA components and 
#' some additonal explanatory variables
#'
#' @param pca prcomp object
#' @param df data frame containing predictors
#' @param selvars names of the columns from data frame to choose as predictors
#' @param comps for which PCA components (or for which columns of the numeric matrix) the correlations should be plotted
#' @param scaled whether the data should be scaled
#' @param col colors to use for the different predictors
#' @param ... additional parameters to be passed to plotting functions such as pch or lty

#' @export
pcaR2plot <- function( pca, df, selvars= colnames( df ), comps=NULL, scaled= FALSE, col= NULL, ... ) {

  # if( is.null( selvars ) ) selvars <- colnames( df )
  # if( is.null( selvars ) ) selvars <- 1:ncol( df )
  if( is.null( col ) )    col <- c( "black", mypalette( length( selvars ) ))
  if( is.null( comps ) )   comps <- 1:ncol( pca$x ) 

  pca.var <- pca2var( pca )[comps]


  sv <- list()
  if( ! is.null( selvars ) ) {
    for( i in selvars ) {
      sv[[i]] <- pcaR2( pca, predictor= df[,i], comps= comps, scaled= scaled )
    }
  }

  .pcaR2plot_plot( comps, pca.var, col= col[1], ... )

  if( !is.null( selvars ) ) {
    for( i in 1:length( selvars ) ) {
      s <- selvars[i]
      .pcaR2plot_lines( comps, sv[[s]]$r2, col= col[i+1], ... )
    }
  }


  return( invisible( list( vartot= pca.var, colors= setNames( col, c( "vartot", selvars ) ), pcaR2=sv ) ) )
}
