#' Annotated PDF copy of the graph produced
#'
#' Annotated PDF copy of the graph produced
#'
#' Produces a PDF copy which includes the information about date and
#' working directory. For documentation purposes.
#'@param file File to write the copy to (default="tmp.pdf")
#'@param cex Scaling used to annotate the figure
#'@param out.type Output type passed to \code{\link{dev.copy2pdf}}
#'@param ... additional parameters passed to \code{\link{dev.copy2pdf}}
#'@export
copyan <- function( file= "tmp.pdf", cex= 0.8, out.type= "cairo", ... ) {

  oldpar <- par( mar= c( 0, 0, 0, 0 ), usr= c( 0, 1, 0, 1 ) )
  on.exit( par( oldpar ) )

  par( new= TRUE )
  plot.new()

  # ann is the annotation stamp:
  # current working directory,
  # file name, date and time.
  ann <- paste( getwd(), file, Sys.time(), sep= ", " )
  strh <- strheight( ann, cex= cex )
  strw <- strwidth(  ann, cex= cex )

  # put text in the lower bottom corner,
  # just at the very margin of the plot
  usr1 <- par( "usr" )
  fig_label(ann, pos="bottomleft", region="device", cex=cex)

  dev.copy2pdf( file= file, out.type= out.type, ... )
}

#' @export
dcpy <- function(fn, out.type="cairo", ...) {

  dev.copy2pdf(file=fn, out.type=out.type, ...)

}
