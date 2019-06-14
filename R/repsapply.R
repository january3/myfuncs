#' A more general version of avereps
#'
#' A more general version of avereps
#'
#' Just like avereps averages the expression value for each sample for
#' genes that have the same ID, \code{repsapply} applies any arbitrary
#' function. 
#'
#' @param x a numeric matrix 
#' @param ID function will be applied to replicates with the same ID
#' @param colwise If TRUE, then for each column, the function will be
#' applied to rows with identical IDs. If FALSE, then for each row, the
#' function will be applied to columns with identical IDs.
#' @param func the function to be applied
#' @export
repsapply <- function( x, ID, colwise= TRUE, func= mean ) {
  if( ! colwise ) x <- t( x )

  if( length( ID ) != nrow( x ) ) stop( "Length of ID does not match" )

  uID <- unique( ID )

  ret <- sapply( uID, function( i ) {
    apply( x[ ID == i, , drop= F ], 2, func )
  } )

  colnames( ret ) <- uID

  if( colwise ) ret <- t( ret )
  ret
}
