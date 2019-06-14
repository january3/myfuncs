#' Return the object with colnames set
#'
#' Return the object with colnames set
#'
#' Return the object with colnames set. This is equivalent to setNames, but
#' for objects that have columns like data frames and matrices.
#' @return the same object with colnames set to colnames
#' @export

setColnames <- function( x, colnames= NULL ) {

  if( !is.null(colnames) && length( colnames ) != ncol( x ) ) {
    stop( "colnames: Invalid argument" )
  }

  colnames( x ) <- colnames
  return( x )
}


#' @rdname setColnames
#' @export
setRownames <- function( x, rownames= NULL ) {

  if( any( duplicated(rownames))) stop( "duplicated row names" )

  if( !is.null(rownames) && length( rownames ) != nrow( x ) ) {
    stop( "rownames: Invalid argument" )
  }

  rownames( x ) <- rownames
  return( x )
}
