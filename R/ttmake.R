#' Quickly make a design
#'
#' Quickly make a model matrix from a factor
#'
#' This is a shortcut wrapper around \code{model.matrix(~0 + g)}
#' @param g factor to be plotted. If g is not a factor, it will be
#' converted to a factor.
#' @return a model matrix with column names corresponding to factor levels.
#' @export
mkdesign <- function( g ) {
  if( ! class( g ) == "factor" ) g <- factor( g )
  ret <- model.matrix( ~ 0 + g )
  colnames( ret ) <- levels( g )
  return( ret )
}


#' Create a topTable for limma with "msd" based on confidence interval
#'
#' Create a topTable for limma with "msd" based on confidence interval
#'
#' Create a topTable for limma with the additional column "msd" based on confidence interval.
#'
#' msd is defined as the lower boundary of the confidence interval for the
#' logFC, multiplied by the sum of the logFC. This allows to sort the list of
#' genes by both the confidence we have in the predicted logFC and its magnitude, rather than by
#' p-value alone (which can correspond to an small effect size) and logFC
#' (which can be very variable despite a large estimate).
#'
#' @param fit lmFit object
#' @param coef coefficient for which the output should be generated
#' @param sort whether the output should be sorted
#' @param ... any other parameter passed to \code{topTable}
#' @export
ttmake <- function( fit, coef= 1, sort= TRUE, ... ) { 
  require( limma )
  tt <- topTable( fit, sort.by= "n", coef= coef, number= Inf, confint= T, ... ) 

  if( any( grepl( "CI.025", colnames( tt ) ) ) )
    tt$msd <- apply( sign( tt$logFC ) * cbind( tt$CI.025, tt$CI.975 ), 1, min ) 
  else 
    tt$msd <- apply( sign( tt$logFC ) * cbind( tt$CI.L, tt$CI.R ), 1, min ) 



  # tt$msd <- apply( sign( tt$logFC ) * cbind( tt$CI.025, tt$CI.975 ), 1, min ) 

  if( sort ) 
    tt <- tt[ order( tt$msd, decreasing= T ), ] 

  tt

}
