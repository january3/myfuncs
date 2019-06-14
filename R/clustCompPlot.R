#' compare different clusters in an overview plot
#'
#' compare different clusters in an overview plot
#'
#' compare different clusters in an overview plot
#'@param c1,c2 alternate cluster memberships
#'@param col color of the plot
#'@param ... additional parameters passed to the \code{plot} function
#'@export
clustCompPlot <- function( c1, c2, col= "black", ... ) {

  c1n <- unique( c1 )
  c1siz <- sapply( c1n, function( x ) length( which( c1 == x ) ) )
  #c1n <- c1n[ order( c1siz, decreasing= T ) ]
  c1genes <- names( c1 )


  c2n <- unique( c2 )
  c2siz <- sapply( c2n, function( x ) length( which( c2 == x ) ) )
  #c2n <- c2n[ order( c2siz, decreasing= T ) ]
  c2genes <- names( c2 )

  n1 <- length( c1n )
  n2 <- length( c2n )

  plot( NULL, type= "n", xlim= c( 1, n1 ), ylim= c( 1, n2 ), xaxt= "n", yaxt= "n", las= 2, ... )
  axis( 1, 1:n1, labels= c1n, las= 2 )
  axis( 2, 1:n2, labels= c2n, las= 2 )


  for( i1 in 1:n1 ) abline( v= i1, col= "#cccccc33" )
  for( i2 in 1:n2 ) abline( h= i2, col= "#cccccc33" )

  comm <- matrix( 0, nrow= n1, ncol= n2 )

  for( i1 in 1:n1 ) {
    cl_1 <- c1n[ i1 ]
    for( i2 in 1:n2 ) {
      cl_2 <- c2n[ i2 ]

      comm[i1,i2] <- length( intersect( c1genes[ c1 == cl_1 ], c2genes[ c2 == cl_2 ] ) )
    }
  }

  max_comm <- max( comm )
  for( i1 in 1:n1 ) {
    cl_1 <- c1n[ i1 ]
    for( i2 in 1:n2 ) {
      cl_2 <- c2n[ i2 ]

      if( comm[i1,i2] == 0 ) next ;

      points( i1, i2, cex= 1 + 4*comm[i1,i2]/max_comm, pch= 19, col= col )
      text( i1, i2, as.character( comm[i1,i2] ), cex= 0.5 + 2*comm[i1,i2]/max_comm/3, col= "white" )
    }
  }


  return( invisible( comm ) )
}
