#' Shortcuts for common functions
#'
#' Shortcuts for common functions
#'
#' \code{p}, \code{h} and \code{l} are simple shortcuts for \code{paste},
#' \code{head} and \code{length} functions, respectively.
#'
#' \code{msd} prints the mean and standard deviation of a numeric vector
#' and returns them invisibly in a named numeric vector.
#' 
#' \code{ss} returns a sum of squares of a numeric vector
#'
#' \code{write.col} wrapper for \code{write.table} for saving a single
#' column without column or rownames and quotes.
#' @aliases p h l msd ss
#' @param x a numeric vector
#' @param ... further parameters to be passed to other functions.
#' @rdname shortcuts
#' @export
l <- function( ... ) length( ... )

#' @rdname shortcuts
#' @export
p <- function( ... ) paste( ... )

#' @rdname shortcuts
#' @export
h <- function( ... ) head( ... )

#' @rdname shortcuts
#' @export
msd <- function( x ) {

   m <- mean( x )
   sd <- sd( x )
   printf( "%.2f +- %.2f", m, sd )

   return( invisible( c( mean=m, sd=sd ) ) ) 
}

#' @rdname shortcuts
#' @export
# just a sum of squares
ss <- function(x) sum(scale(x, scale = FALSE)^2)

# blah! It is just a simple alternate form of write.table, for one column only with no
# rownames, colnames, quotes
#' @rdname shortcuts
#' @export
write.col <- function( x, col.names= F, row.names= F, quote= F, ... ) {
  write.table( x, col.names= col.names, row.names= row.names, quote= quote, ... )
}
