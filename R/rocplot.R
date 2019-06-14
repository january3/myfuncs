#' Format AUC from roc() function
#'
#' Format AUC from roc() function
#'
#' Format AUC from roc() function
#' @param x object returned by roc()
#' @export
rocformat <- function( x ) {
  ret <- sprintf( "AUC=%.2f (95%% CI=%.2f-%.2f)", x$auc, x$ci[1], x$ci[3]) 
  return(ret)
}


#' Plot a series of ROCs
#'
#' Plot a series of ROCs
#'
#' Plot a series of receiver-operator curves (ROCs). This function is a wrapper around rocplot(),
#' allowing to plot multiple ROCs.
#' 
#' ROCs to be drawn are provided in the prs parameter, a list, for which
#' each element is a data frame containing at least the following columns:
#' reality, decision and prediction. The column reality describes the actual
#' (real) classification; "decision" is the class assigned by the ML
#' algorithm, and "prediction" is a numeric value describing the probability
#' of a given sample to belong to the case class.
#'
#' The graphic parameters col, lwd and lty can be either a single value,
#' which will be applied to all lines drawn, or a vector of the same length as
#' the number of ROCs to be drawn.
#' @seealso rocplot, rocformat, prconvert
#' @param prs a list returned by prconvert
#' @param col vector of colors of the length equal to length of prs
#' @param legend whether a legend should be plotted on the bottom right of the figure
#' @param legend.names a vector of names to plot of the legend
#' @param add.stats whether AUC and CI should be added to the legend
#' @param lwd line width
#' @param lty line type
#' @param ... any further arguments are passed to the rocplot function
#' @return A list of objects returned by the rocplot function.
#' @import methods
#' @export
plotROCseries <- function( prs, legend=TRUE, legend.names=NULL, add.stats=TRUE, col=NULL, lwd=3, lty=NULL, ... ) {

  if(!is(prs, "list") ) stop( "prs must be a list" )
  if(is.null(col)) col <- mypalette()

  N   <- length(prs)
  add <- FALSE

  if(is.null(lwd)) lwd <- 3
  if(length(lwd) == 1 ) lwd <- rep(lwd, N)

  if(is.null(lty)) lty <- 1
  if(length(lty) == 1 ) lty <- rep(lty, N)

  if(length(col) == 1 ) col <- rep(col, N)

  if(is.null(legend.names)) legend.names <- names(prs)
  if(is.null(legend.names)) legend.names <- 1:N

  rocs <- list()
  for( i in 1:N ) {
    rocs[[i]] <- rocplot( prs[[i]], col=col[i], add=add, lwd=lwd, lty=lty[i], confmat=FALSE, ci=FALSE, ... )
    if(!add) add <- TRUE
  }

  ciinf <- sapply(rocs, rocformat)

  if(add.stats) {
    legend.names <- paste(legend.names, ciinf, sep=", " )
  }

  if( legend ) {
    legend("bottomright", legend.names, lwd=lwd, col=col, bty="n", lty=lty )
  }

  return(invisible(rocs))
}




#' ROC figure using pROC package
#'
#' ROC figure using pROC package
#'
#' This is a wrapper around the \code{roc} function from the pROC package.
#' It plots the ROC curve, but additionally it also can add information on the
#' confidence interval and a confusion matrix.
#' @return whatever the roc() function returns
#' @export
rocplot <- function( ret, title= "ROC", file= NULL, confmat= TRUE, ci= TRUE, positive.name= NULL, add= FALSE, ... ) {
  require( pROC )

  if( ! "data.frame" %in% class( ret ) ) stop( "ret must be a data.frame object" )

  if( ! all( c( "reality", "prediction" ) %in% colnames( ret ) ) ) 
    stop( "ret must have the following column names: reality, prediction" )

  if(!"decision" %in% colnames(ret)) {
    if(confmat) warning("No decision column; setting confmat=FALSE")
    confmat <- FALSE
  }

  if(is.null(positive.name)) positive.name <- ret$reality[1]

  

  rr <- roc( response= ( ret$reality == positive.name ), predictor= ret$prediction, plot= T, ci= T, main= title, add= add, ... )

  if( ci && ! add )  {
    cat( sprintf( "# AUC= %.2f, CI= %.2f - %.2f\n", rr$auc, rr$ci[1], rr$ci[3] ) )
    legend( "topleft", sprintf( "AUC= %.2f\nCI= %.2f - %.2f", rr$auc, rr$ci[1], rr$ci[3] ), bty= "n" )
  }

  if( confmat && ! add ) {
    pp <- par( "family" )
    par( family= "mono" )
    legend( "bottomright", confuMat( ret$reality, ret$decision, as.text= T ), bty= "n", inset= 0.05 )
    par( family= pp )
  }

  if( ! is.null( file ) ) copyan( file= file )

  invisible( rr )
}

#' Convert RF votes to plottable object
#'
#' Convert RF votes to plottable object
#'
#' @return an object plottable with the rocplot() function
#' @export
prconvert <- function( pr, reality, caseclass= 1, classnames= unique( reality ) ) {

  ret <- data.frame( pr )


  if( caseclass != 1 ) {
    caseclass <- 2
    controlclass <- 1
  } else {
    controlclass <- 2
  }

  ret$prediction <- pr[,caseclass]
  ret$decision <- classnames[ ( pr[,caseclass] < pr[,controlclass] ) + 1 ]
  ret$reality <- reality

  return( ret )
}

#' Convert random forest to format suitable for rocplot()
#'
#' Convert random forest to format suitable for rocplot()
#'
#' Convert the object returned by randomForest() to a format suitable for
#' the function rocplot()
#' @param rf An object returned by randomForest()
#' @param reality A vector with actual class assignments
#' @export
rf2pr <- function(rf) {

  reality <- as.character(rf$y)
  ret <- data.frame(rf$votes)
  classes <- colnames(ret)
  if(length(classes) > 2) stop("More than two classes")
  if(!setequal(classes, reality)) stop("reality has different classes than rf")

  ret$decision <- classes[ (ret[,1] < ret[,2])+1 ]
  ret$reality  <- reality
  ret$prediction <- ret[,1]
  ret
}
