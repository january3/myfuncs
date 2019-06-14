#' Shortcuts for sorting data frames
#'
#' Shortcuts for sorting data frames
#'
#' \code{sortby} sorts a data frame by column \code{key}. If
#' \code{absolute} is TRUE, then the numeric values in column \code{key} will
#' be first turned to absolute values with \code{abs()}. 
#' 
#' \code{rsortby} is the same as \code{sortby(..., decreasing=T)}.
#' 
#' \code{sortabs} and \code{rsortabs} are the same as \code{sortby} and
#' \code{rsortby} with parameter \code{abs=TRUE}.
#' @param df a data frame
#' @param key column for sorting the data frame
#' @param abs whether sort should be by absolute numeric values
#' @return All functions return a sorted data frame.
#' @export
sortby <- function( df, key, decreasing= F, absolute= F ) {

  key.d <- df[,key]
  if( absolute ) key.d <- abs( key.d )

  df[ order( key.d, decreasing= decreasing ), ]
}

#' @rdname sortby
#' @export
rsortby <- function( df, key, absolute= F ) {

  sortby( df, key, decreasing= T, absolute= absolute )

}

#' @rdname sortby
#' @export
rsortabs <- function( df, key ) {

  sortby( df, key, decreasing= T, absolute= T )

}

#' @rdname sortby
#' @export
sortabs <- function( df, key, decreasing= F ) {

  sortby( df, key, decreasing= decreasing, absolute= T )

}
