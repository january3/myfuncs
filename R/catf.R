#' Shortcut with sprintf
#'
#' Shortcut with sprintf
#'
#' These functions are wrappers around \code{function( sprintf( ... ) )}
#'@param ... arguments passed to sprintf before calling the actual function
#'@examples
#'catf("pi=%.5f", pi)
#'printf("pi=%.5f", pi)
#'@export
catf   <- function( ... ) { cat( sprintf( ... ) ) }

#'@export
#'@rdname catf
printf <- function( ... ) { print( sprintf( ... ) ) }


