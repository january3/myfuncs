#' @export
is.signif <- function( tt, coef= 1, qval= 0.05, lfc= 1, lfc.abs= TRUE ) {

  ctt <- colnames( tt )
  all.coefs <- gsub( "logFC\\.", "", ctt[ grep( "^logFC\\.", ctt ) ] )
  # print( all.coefs )

  if( ! coef %in% all.coefs ) {
    if( ! is.numeric( coef ) ) stop( "Incorrect coef parameter" )
    coef <- all.coefs[ coef ]
  }

  l <- paste0( "logFC.", coef )
  q <- paste0( "qval.",  coef )

  ret <- tt[ , q ] < qval

  if( lfc.abs ) {
    ret <- ret & abs( tt[ , l ] ) > lfc
  } else {
    ret <- ret & tt[ , l ] > lfc
  }

  return( ret )
}

#' Inteligently find significant coefficients for a gene
#' @export
whatabout <- function( name, tt, qval= 0.05, lfc= 1, ... ) {

  ctt <- colnames( tt )
  all.coefs <- gsub( "^logFC\\.", "", ctt[ grep( "^logFC\\.", ctt ) ] )
  if( length( grep( "^msd\\.", ctt ) ) == length( all.coefs ) ) 
    MSD= TRUE
  else
    MSD= FALSE
    
  qcols <- ctt[ grep( "^qval\\.", ctt ) ]


  for( g in name ) {
    sel <- grep.df( g, tt, multiple= TRUE, ... )
    if( length( sel ) == 0 ) {
      catf( "Search string not found\n" )
      next ;
    }
    catf( "Search string: %s", g ) 
    if( length( sel ) > 1 ) {
      catf( ", %d hits", length( sel ) )
    }
    cat( "\n" )


    
    n <- 1
    for( s in sel ) {
      report <- ''
      found <- FALSE

      if( length( sel ) > 1 ) catf( "Hit: %d, %s\n", n, s  )
      n <- n + 1

      # look through all coefficients
      for( c in all.coefs ) {
        if( tt[ s, paste0( "qval.", c ) ] < qval 
          && abs( tt[ s, paste0( "logFC.", c ) ] ) > lfc  ) {

          report <- paste0( report, sprintf( "%s q=%.3e logFC= %.2f\n", c, tt[ s, paste0( "qval.", c ) ], tt[ s, paste0( "logFC.", c ) ] ) )
          found <- TRUE
        }
      }
        
      if( found ) {
        print( tt[ s, ! grepl( "^(logFC|qval|msd)\\.", ctt ) ] )
        cat( report )
      } else {
        cat( "not significant\n" )
      }

    }
  }

}


#' Search through a data frame
#'
#' Wrapper for grep.df
#'
#' @export
dfsearch <- function( pattern, tt, multiple= TRUE, ignore.case= TRUE, return.rowname= FALSE, ... ) {

  x <- grep.df( pattern, tt, multiple= multiple, ignore.case= ignore.case, return.rowname= return.rowname, ... )
  print( tt[ x, ] )
  return( invisible( x ) )
}
