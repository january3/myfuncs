#' leave one out cross-validation with additional functions
#'
#' leave one out cross-validation with additional functions
#'
#' leave one out cross-validation with additional functions
#' @param rg EList object from limma
#' @param grouping_var grouping variable 
#' @param secondary_model whether secondary model should be calculated
#' @param balancing whether sets should be balanced
#' @param mask additional mask
#' @param nvar number of variables for the secondary model
#' @param bestsample_sel choose only samples which were predicted 
#' correctly for prediction of the unknown sample
#' @param response name of the response variable
#' @export
myloo <- function( rg, grouping_var= NULL, 
                       secondary_model= FALSE, nvar= 20, balancing= TRUE, 
                       mask= NULL, bestsample_sel= FALSE,
                       response= "GROUP" ) {
  require( randomForest )
  require( limma )


  .importance <- secondary_model

  ret <- NULL
  nn  <- ncol( rg )

  for( i in 1:ncol( rg ) ) {
    cat( sprintf( "\r%d %.0f%%", i, 100*i/ncol(rg) ) )
    s <- rg[ , i ]

    # from the training set, omit all the samples with the same grouping
    # var (e.g. all samples from the same person)
    if( !is.null( grouping_var ) ) tmp.sel <- grouping_var != grouping_var[i]
    else                           tmp.sel <- rep( TRUE, nn )
    
    if( ! is.null( mask ) ) tmp.sel <- tmp.sel & mask


    tmp <- rg[ , tmp.sel ]

    if( balancing ) train <- balanceset( tmp$targets[ , response ] )
    else            train <- 1:ncol( tmp )

    tmp <- tmp[ , train ]

    rf.m <- randomForest( t( tmp$E ), factor( tmp$targets[ , response ] ), importance= .importance, mtry= nrow( tmp$E ) )

    if( secondary_model ) {

      if( nvar > nrow( rf.m$importance ) ) nvar <- nrow( rf.m$importance )
      sel_var <- order( rf.m$importance[,4], decreasing= TRUE )[1:nvar]

      if( bestsample_sel ) {
        s_sel <- rf.m$predicted == tmp$targets[ , response ] 
        tmp <- tmp[ , s_sel ]
        tmp <- tmp[ , balanceset( tmp$targets[ , response ] ) ]

      }


      rf.m <- randomForest( t( tmp$E[ sel_var, ] ), factor( tmp$targets[ , response ] ), mtry= nvar )
    }

    ret <- rbind( ret, predict( rf.m, newdata= t( s$E ), type= 'prob' ) )

  }

  cat( "\n" )

  ret <- data.frame( ret )
  colnames(ret)[1] <- "prediction"
  ret$decision <- colnames( ret )[ ( ret[,1] < ret[,2] ) + 1 ]
  ret$reality  <- rg$targets[ , response ]
  return( ret )
}


