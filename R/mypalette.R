#' A nice, pastel color palette
#'
#' A nice, pastel color palette
#'
#' A nice, pastel color palette. The \code{f2pal} and \code{f2leg} convert
#' factors to colors. \code{f2pal} converts a factors to colors directly,
#' while \code{f2pal} returns a data frame with one column corresponding to
#' factor levels, and another to the respective colors. This way it is
#' possible to  use it in legends. If the parameter \code{f} is not a
#' factor, it will be converted to a factor.
#' @param n length of palette to return. If longer than the number of
#' colors in the return value of \code{mypalette} without any parameters, colors will be reused.
#' @param f a vector
#' @param transparent a character value to be pasted to each of the RGB
#' color specifications in the palette indicating transparency ("00" for full
#' transparency, "ff" for full opacity)
#' @param alpha same as "transparent", but an alphanumeric value from 0..1.
#'        Takes precedence over "transparent"
#' @return \code{mypalette} returns character vector with RGB color
#' specifications. \code{f2pal} returns a character vector of RGB colors.
#' \code{f2leg} returns a data frame with columns "text" and "col"
#' @examples
#' let <- sample(LETTERS[1:4], 50, replace=TRUE)
#' col <- f2pal(let)
#' plot(runif(50), runif(50), pch=19, col=col, cex=2)
#' leg <- f2leg(let)
#' legend("topleft", leg$text, pch=19, col=leg$col)
#' @export
mypalette <- function( n= NULL, transparent= "99", alpha=NULL ) {

  pal <- "E69F00 56B4E9 009E73 F0E442 0072B2 D55E00 CC79A7 999999 E69F00 56B4E9 009E73 F0E442 0072B2 D55E00 CC79A7"

  pal <- unlist( strsplit( pal, ' ' ) )
  if(!is.null(alpha)) {
    if(alpha > 1) alpha <- 1
    if(alpha < 0) alpha <- 0
    transparent <- sprintf("%02X", as.integer(alpha * 255 ))
  }
  pal <- paste( "#", pal, transparent, sep= "" )

  if( ! is.null( n ) ) {
    if( n > length( pal ) ) {
      pal <- rep( pal, ceiling( n / length( pal ) ) )
    } else {
      pal <- pal[ 1:n ]
    }
  }


  return( pal )
}



#' @rdname mypalette
#' @export
f2pal <- function( f, palfunc= mypalette, ... ) {

 if( ! is.factor( f ) ) f <- factor( f )
 
 return( palfunc(...)[ f ] )

}


#' @rdname mypalette
#' @export
f2leg <- function( f, palfunc= mypalette, ... ) {

  
 if( ! is.factor( f ) ) f <- factor( f )

 ltext <- levels( f )
 lcol  <- mypalette(...)[1:length( ltext ) ]

 return( data.frame( text= ltext, col= lcol ) )

}
