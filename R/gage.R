#' @export
showpath <- function( gene.data, pathway.id, species= "hsa", ... ) {
  require("pathview")

  blah <- pathview( gene.data= gene.data, pathway.id= pathway.id, species= species, ... ) 

  multi <- ""
  if( class( gene.data ) %in% c( "matrix", "data.frame" ) && ncol( gene.data ) > 1 )
    multi <- ".multi"

  if( !is.null( blah ) ) {
    fname <- paste0( "xzgv ", pathway.id, ".pathview", multi, ".png" ) 
    printf( "showing %s", fname )
    system( paste0( "xzgv ", fname, "&" ) )
  }
  return( invisible( blah ) )
}
