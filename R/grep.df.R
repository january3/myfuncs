#' Quick grep through all columns of a data frame
#'
#' Quick grep through all columns of a data frame
#'
#' Quick grep through all columns of a data frame
#' @param arg The regular expression to be searched for
#' @param df  Data frame to search
#' @param multiple Whether more than one result should be returned
#' @param return.rowname Whether rowname rather than the index should be returned
#' @param ... further arguments passed to \code{grep}
#' @export
grep.df <- function( arg, df, multiple= FALSE, return.rowname= TRUE, ... ) {

  getr <- function( r ) {
    if( return.rowname ) {
      r <- rownames( df )[r]
    }
    return( r )
  } 
    
    r <- grep( arg, rownames( df ), ... )
    if( length( r ) == 0 || multiple ) {

      for( c in 1:ncol( df ) ) {
        r <- unique( c( r, grep( arg, df[,c], ... ) ) )

        #if( length( r ) > 0 ) break ;
      }
    
    } 
      
    if( length( r ) == 0 ) {
      warning( "no match found" ) ;
      return( r )
    } 
    
    if( length( r ) == 1 || multiple ) {
      return( getr( r ) )
    }
    
    print( sprintf( "Found %d records", length( r ) ) )

    warning( "More than one record found, returning the first" )
    return( r[1] )
  
}

