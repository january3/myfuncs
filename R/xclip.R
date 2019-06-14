#' Copy a few history lines to the X primary clipboard
#'
#' Copy a few history lines to the X primary clipboard
#'
#' Copy a few history lines to the X primary clipboard. This will probably
#' work only on X-window systems like Ubuntu, and requires the xclip
#' program to be installed.
#'
#' @param n number of lines to copy, not counting the current xclip command
#' @export
xclip <- function( n= 5 ) {

 t <- tempfile()
 savehistory( file= t )

 system( paste0( "tail -n ", n + 1, " ", t, "|head -n -1|xclip" ) )
}



#' @export
hh <- function(x, n=10) head(x[,1:n], n=10)
