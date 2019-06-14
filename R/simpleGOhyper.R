#' simple alternative to hyperGTest
#'
#' simple alternative to hyperGTest
#'
#' @param categories Vector of category assignments for the data
#' @param sign Either a logical vector of the same length as categories
#' indicating whether the given entity is significant, or a vector of
#' indices indicating which entities are significant.
#'
#' @export

simpleGOhyper <- function( categories, sign ) {

  ret <- NULL
  if( class( sign ) != "logical" ) 
    sign <- 1:length( categories ) %in% sign

  N <- length( categories )
  n <- sum( sign )

  printf( "N= %d, n= %d", N, n )

  for( c in unique( categories ) ) {

    sel <- categories == c
    B <- sum( sel )
    b <- sum( sign & sel )
    if( b == 0 ) {
      ret <- rbind( ret, c( N=N, n=n, B=B, b=b, en=NA, p= 1 ) )
      next 
    }


    p <- 1 - phyper( b-1, B, N - B, n )


    en <- ( b/n ) / ( B/N )
    #en <- ( sum( sign & sel ) / sum( sign ) ) / ( sum( sel ) / length( categories ) ) 

    ret <- rbind( ret, c( N=N, n=n, B=B, b=b, en=en, p=p ) )

  }

  colnames( ret ) <- c( "N", "n", "B", "b", "Enrichment", "P.Value" )
  ret <- data.frame( Category=unique( categories ), ret )
  ret$adj.P.Val <- p.adjust( ret$P.Value, method= "fdr" )

  ret <- ret[ ret$b > 0, ]
  if( nrow( ret ) == 0 )
    cat( "No enrichment found\n" ) 

  return( ret[ order( ret$adj.P.Val ), ] )
}
