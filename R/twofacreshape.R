twofacreshape <- function( m, newid, groupby ) {
  require( reshape2 )
  df <- data.frame( m, newid= newid, groupby= groupby )
  df <- melt( df, c( "newid", "groupby" ) )
  df <- dcast( df, newid ~ groupby + variable )
  rownames( df ) <- df[,1] 
  df <- df[,-1]
  return( df )
}
