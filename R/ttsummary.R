#' Print a summary of a top table.
#'
#' Print a summary of a top table.
#'
#' Print a summary of a top table.
#'@export
ttsummary <- function( tt, c= NULL, p.val= 0.01, lfc= 1 ) {

  if( is.null( c ) ) {
    c <- gsub( "logFC\\.", "", colnames( tt )[ grep( "logFC", colnames( tt) ) ] )
  }
  ret <- NULL

  for( cc in c ) {
    ltt <- paste0( "logFC.", cc )
    ptt <- paste0( "qval.", cc )
    nup <- sum( tt[,ltt] > lfc & tt[,ptt] < p.val )
    ndo <- sum( tt[,ltt] < -lfc & tt[,ptt] < p.val )
    cat( sprintf( "%s\t%d\t%d\t%d\n", cc, nup, ndo, nup+ndo ) )
    ret <- rbind( ret, c( Nup=nup, Ndown=ndo, Total=nup+ndo ) )
  }


  return( invisible( cbind( c, data.frame(ret) ) ) )
}
