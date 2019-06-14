#' beeswarm for a top table (result of topTableAll) plot 
#'
#' beeswarm for a top table (result of topTableAll) plot 
#'
#' beeswarm for a top table (result of topTableAll) plot 
#'
#' @param tt an object returned by topTableAll
#' @param qval q-value threshold for coloring data points
#' @export
ttplot <- function( tt, qval= 0.05, ... ) {
  require( beeswarm )
  
  n.lfc <- colnames( tt )[ grep( "^logFC\\.", colnames( tt ) ) ]
  n.qval <- colnames( tt )[ grep( "^qval\\.", colnames( tt ) ) ]
  n <- gsub( "logFC\\.", "", n.lfc )

  qv <- as.numeric( as.matrix( tt[ , n.qval ] ) )
  pwcol<- rep( "#33333333", length( qv ) )
  pwcol[ qv < qval ] <- "#ff111199"
  # print( pwcol )
  
  beeswarm( tt[ , n.lfc ], pch= 19, pwcol= pwcol, labels= n, ... )
  abline( h= 0, col= "#33333333" )

}

