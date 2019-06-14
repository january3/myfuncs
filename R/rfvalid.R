rfvalid <- function( X, Y, kfold= 0, ... ) {
  
  require( randomForest )

  if( ! is.factor( Y ) ) {
    warning( "converting Y to factor" )
    Y <- factor( Y )
  }

  n <- nrow( X )

  ret <- c()

  nerr <- 0

  for( i in 1:n ) {
    catf( "\r%i/%i", i, n )
    rf <- randomForest( X[ -i, ], Y[ -i ] )
    pr <- as.character( predict( rf, newdata= X[ i, ] ) )
    if( pr != Y[ i ] ) {
      nerr <- nerr + 1
      catf( "\n%d pred=%s real=%s\n", i, pr, Y[ i ] )
    }
    ret <- c( ret, pr )
  }


  catf( "\nError rate: %.2f %%", nerr / n * 100 )

  return( ret )
}
