#' first attempt at pca/clustering plot (not working?)
#' @export
clustplot <- function( d, pca.comp= 1, line.width= 1e-4, col= "#33333333" ) {

  #line.width= 0.1

  nclust <- apply( d, 2, function( x ) length( unique( x ) ) )

  nmax <- max( nclust )
  nclusterings <- ncol( d )

  c.names <- colnames( d )
  if( is.null( c.names ) ) c.names <- 1:nclusterings 

  clust.pos <- list()

  if( length( pca.comp ) > 1 ) {
    pca <- pca.comp
  } else {
    pca <- prcomp( d, scale.= T )$x[,pca.comp]
  }

  clust.pos <- list()
  clust.sizes <- list()
  d2 <- NULL
  for( i in 1:nclusterings ) {
    clust.pos[[i]] <- sapply( 1:nclust[i], function( x ) mean( pca[ d[,i] == x ] ) )

    y <- d[,i]


    for( cl in 1:nclust[i] ) {
      wh <- d[,i] == cl
      n <- sum( wh ) # cluster size
      #clust.sizes[[i]] <- c( clust.sizes[[i]], n )
      y[ wh ] <- clust.pos[[i]][ cl ] + seq( -sum( wh ) * line.width / 2, by= line.width, length.out = sum( wh ) )
    }

    clust.sizes[[i]] <- sapply( 1:nclust[i], function( x ) diff( range( y[ d[,i] == x ] ) ) )

    d2 <- cbind( d2, y )

  }

  #print( d2 )
  

  plot( NULL, type= "l", xlim= c( 0, nclusterings ), ylim= range( d2 )  )


  dev.hold()
  apply( d2, 1, function( y ) lines( 1:nclusterings, y, col= col ) )
  #for( i in 1:nclusterings ) points( rep( i, nclust[i] ), clust.pos[[i]], pch= 19, cex= 1.5 )
  for( i in 1:nclusterings ) {
    for( cl in 1:nclust[i] ) {
      cs <- clust.sizes[[i]][cl] / 2
      cp <- clust.pos[[i]][cl]
      print( cl, cs, cp )
      rect( i - 0.1, cp - cs, i + 0.1, cp + cs, col= "black" )
    }

  }
  dev.flush()



}
