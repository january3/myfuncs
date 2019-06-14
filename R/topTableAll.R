#' Convert a topTableAll results by gene ID
#'
#' Convert a topTableAll results by gene ID
#'
#' Convert a topTableAll results by gene ID by averaging log fold changes
#' and calculating combined p-values
#' @export

combineTopTableAll <- function( tt, ID, contrasts=NULL, method="FDR" ) {

  if(is.null(contrasts)) {
    contrasts <- gsub( "logFC\\.", "", colnames( tt )[ grep( "^logFC\\.", colnames(tt)) ] )
  }

  if(is.null(contrasts) || length(contrasts) == 0 ) stop( "no contrasts found" )

  tt.rest <- tt[ , !grepl( "^(logFC|msd|qval|P.Value)", colnames( tt ) ) ]

  ids <- unique( ID )

  ret <- tt.rest[ match( ids, ID ), ]

  for( c in contrasts ) {
    lfc  <- paste0( "logFC.", c )
    pval <- paste0( "P.Value.", c )

    if( ! all( c( lfc, pval ) %in% colnames( tt ) ) ) stop( "columns missing from top table" )

    tmp <- sapply( ids, function( x ) { 
       sel <- ID == x ; 
       nsel <- sum(sel) ;
       if( nsel == 1 ) { 
         return( c(P.Value=tt[sel, pval], logFC=tt[sel, lfc]) ) 
       } else {
        return( c(P.Value=pchisq(-2*sum(log(tt[sel, pval])), df=2*nsel, lower.tail=F ), logFC=mean(tt[sel,lfc]) ) ) 
       } 
      })

    tmp <- data.frame( t(tmp ) )[, c(2,1) ]
    colnames(tmp) <- c( lfc, pval )

    ret <- cbind( ret, tmp )
    #ret[ , paste0( "qval.", c ) ] <- ret[ , 
  }

  return(ret)

}


#' Calculate MSD based on logFC and CI
#'
#' Calculate MSD based on logFC and CI
#'
#' MSD ("minimal significant difference") is defined as minimum between the
#' confidence interval boundaries multiplied by the sign of the change. MSD is
#' the absolute distance between the confidence interval and 0. Or, MSD is the
#' smaller of absolute values of confidence intervals.
#' @param lfc vector of log fold changes or signs of log fold changes
#' @param ci.L Left boundary of confidence interval
#' @param ci.R Right boundary of confidence interval
#' @return a vector of MSD values
#' @export
MSD <- function( lfc, ci.L, ci.R ) {

  MSD <- apply( sign( lfc ) * cbind( ci.L, ci.R ), 1, min ) 

  MSD
}



#' Create a topTable for all coefficients in a limma fit object
#'
#' Create a topTable for all coefficients in a limma fit object
#'
#' For every contrast in \code{fit}, the log fold change and q-value
#' (p-value corrected for multiple testing) are calculated with the limma
#' function \code{topTable}. These columns are joined to common fields (such
#' as probe ID and gene meta data). topTableAll does not sort the data from
#' the fit object.
#' @return A data frame with gene meta data and gene fold changes /
#' q-values. For each contrast "N", columns "logFC.N" and "qval.N" are
#' created (and, optionally, columns "logFC.s.N" and "msd.N").
#' @export
topTableAll <- function( fit, contrasts=NULL, genelist= NULL, 
                         idcoll= NULL, sign.fc.col=FALSE, 
                         sig.threshold= 1e-5, msd=FALSE, ci=FALSE, rawp=FALSE ) {
  require( limma )

  if( is.null( contrasts ) ) {
    contrasts <- colnames( fit$coefficients )
  }

  common <- colnames( fit$genes )

  if( ! missing( genelist ) ) {
    common <- union( common, colnames( genelist ) )
  } else {
    genelist <- fit$genes 

  }

  ret <- NULL

  for( c in contrasts ) {

    tt <- topTable( fit, coef= c, genelist= genelist, sort.by= "none", number= Inf, confint= msd )
    if( msd | ci ) {
      if( any( grepl( "CI.025", colnames( tt ) ) ) ) {
        tt$CI.L <- tt$CI.025
        tt$CI.R <- tt$CI.975
      }

      tt$msd <- apply( sign( tt$logFC ) * cbind( tt$CI.L, tt$CI.R ), 1, min ) 

    }



    if( is.null( ret ) ) {
      print( colnames( tt ) )
      ret <- tt[ , common ]
    }

    ret[, paste( "logFC", c, sep= "." ) ] <- tt$logFC
    if( sign.fc.col ) {
      ret[, paste( "logFC.s", c, sep= "." ) ] <- tt$logFC
      ret[ tt$adj.P.Val > sig.threshold , paste( "logFC.s", c, sep= "." ) ] <- 0
    }
    if( msd ) {
      ret[, paste( "msd",  c, sep= "." ) ] <- tt$msd
    }
    if( ci ) {
      ret[, paste( "cil",  c, sep= "." ) ] <- tt$CI.L
      ret[, paste( "cir",  c, sep= "." ) ] <- tt$CI.R
    }

    if( rawp ) {
      ret[ , paste( "P.Value", c, sep="." ) ] <- tt$P.Value

    }


    ret[, paste( "qval",  c, sep= "." ) ] <- tt$adj.P.Val

  }

  if( !is.null( idcoll ) ) { rownames( ret ) <- ret[, idcoll ] }
  return( ret )
}
