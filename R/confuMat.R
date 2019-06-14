#' Calculate and show ROC curve
#'
#' Calculate and show ROC curve
#'
#' Calculate and show ROC curve
#'@param reality a vector of responses (two possible values)
#'@param predictor a vector predicting the response (numeric)
#'@param decision a vector of predicted responses (same values as response)
#'@return \code{rocplot} invisibly returns the return value of the \code{roc} call
#'@export
rocplot <- function( reality, predictor, decision=NULL, title= "ROC", file= NULL, confmat= TRUE, ci= TRUE ) {
  require( pROC )

  rr <- roc( response= reality, predictor= predictor, plot= T, ci= T, main= title )

  if( ci )
    legend( "topleft", sprintf( "AUC= %.2f\nCI= %.2f - %.2f", rr$auc, rr$ci[1], rr$ci[3] ), bty= "n" )

  if( confmat  ) {
    if( is.null( decision ) ) stop( "parameter 'decision' must be provided with option confmat" )
    pp <- par( "family" )
    par( family= "mono" )
    legend( "bottomright", confuMat( reality, decision, as.text= T ), bty= "n", inset= 0.05 )
    par( family= pp )
  }

  if( ! is.null( file ) ) copyan( file= file )

  invisible( rr )
}







#' Calculate a confusion matrix
#'
#' Calculate a confusion matrix
#'
#' Calculate a confusion matrix
#'@param reality vector with actual assignments of groups to items
#'@param predictions vector with predicted assignments of groups to items
#'@param as.text if TRUE, return a single character value; if FALSE, return a matrix
#'@return \code{confuMat} invisibly returns the confusion matrix
#'@export
confuMat <- function( reality, predictions, as.text= FALSE ) {

  tt <- as.matrix( table( reality, predictions ) )
  ml <- max( nchar( colnames( tt ) ) ) + 2
  fmt <- sprintf( "%% %d", ml )

  ret.txt <- ""

  # cat( sprintf( paste( fmt, 's', sep= '' ), "" ) )

  ret.txt <- sprintf( paste0( fmt, 's' ), "" )
  for( c in 1:ncol( tt ) ) 
    ret.txt <- paste0( ret.txt, sprintf( paste0( fmt, 's' ), colnames( tt)[c] ) )
    # cat( sprintf( paste( fmt, 's', sep= '' ), colnames( tt )[c] ) )
  ret.txt <- paste0( ret.txt, "\n" )
  # cat( "\n" ) 
  tot.n <- 0
  tot.e <- 0

  ret.mat <- NULL


  for( i in 1:nrow( tt ) ) {

    cl <- rownames( tt )[i]
    # cat( sprintf( paste( fmt, 's', sep= '' ), cl ) )
    ret.txt <- paste0( ret.txt, sprintf( paste( fmt, 's', sep= '' ), cl ) )
    for( c in 1:ncol( tt ) ) {
      # cat( sprintf( paste( fmt, 'd', sep= '' ), tt[i,c] ) )
      ret.txt <- paste0( ret.txt, sprintf( paste( fmt, 'd', sep= '' ), tt[i,c] ) )
    }

    n.all <- sum( tt[i,] )
    n.err <- n.all - tt[i,cl] 
    tot.n <- tot.n + n.all
    tot.e <- tot.e + n.err
    # cat( sprintf( "   %.2f %% (%d/%d)", n.err / n.all * 100, n.err, n.all ) )
    ret.txt <- paste0( ret.txt, sprintf( "   %.2f %% (%d/%d)", n.err / n.all * 100, n.err, n.all ) )
    ret.mat <- rbind( ret.mat, c( tt[i,], n.err / n.all * 100, n.err, n.all ) )
      
    # cat( "\n" )
    ret.txt <- paste0( ret.txt, "\n" )
  }

  # cat( sprintf( paste( fmt, 's', sep= '' ), "" ) )
  ret.txt <- paste0( ret.txt, sprintf( paste( fmt, 's', sep= '' ), "" ) )
  for( c in 1:ncol( tt ) ) 
    ret.txt <- paste0( ret.txt, sprintf( paste( fmt, 's', sep= '' ), "" ) )

  ret.txt <- paste0( ret.txt, sprintf( "   %.2f %% (%d/%d)\n", tot.e / tot.n * 100, tot.e, tot.n ) )

  ret.mat <- rbind( ret.mat, c( rep( 0, ncol( tt ) ), tot.e / tot.n * 100, tot.e, tot.n ) )
  colnames( ret.mat ) <- c( colnames( tt ), "errP", "nErr", "nTot" )


  cat( ret.txt )
  if( as.text ) ret.mat <- ret.txt
  invisible( ret.mat )
}
