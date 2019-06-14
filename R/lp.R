#' Send current graph to printer.
#'
#' Send current graph to printer.
#'
#' Send current graph to printer.
#' @export

lp <- function(args="") {
  tmp <- tempfile()
  copyan( file=tmp )
  system( paste0( "lp ", args, " ", tmp ) )
}
