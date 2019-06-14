#' Create a label at the top left of the graphics device
#'
#' Puts a text anywhere on the device, with a logical position (such as
#' "topleft" or "center") specified relative to one of three plotting
#' regions (\code{figure} (default), \code{device} or \code{plot})
#'
#' The main purpose of this function is to add labels (such as "A", "B",
#' ...) to figures 
#' @section Warnings:
#' \code{fig_label} is not compatible with \code{ggplot2}. However,
#' par(mfrow=...) and layout() work just fine.
#' @seealso \code{\link{layout}}, \code{\link{par}}
#' @examples 
#' ## Basic use:
#'
#' par(mfrow=c(2,2))
#' for(x in LETTERS[1:4]) { 
#'    plot(rnorm(100))
#'    fig_label(x, cex=2) 
#' }
#'
#' ## Plotting at different positions and in different regions:
#'
#' plot(rnorm(100))
#' for(i in c("topleft", "topright", "top", 
#'       "left", "center", "right", 
#'       "bottomleft", "bottom", "bottomright")) {
#'       fig_label(i, pos=i, cex=2, col="blue")
#'       fig_label(i, pos=i, cex=1.5, col="red", region="plot")
#' }
#'
#' ## All the different regions:
#'
#' par(mfrow=c(2,2))
#' for(x in LETTERS[1:4]) { 
#'    plot(rnorm(100))
#'    fig_label("figure region", cex=2, col="red") 
#'    fig_label("plot region", region="plot", cex=2, col="blue")
#' }
#' fig_label("device region", cex=2, pos="bottomright", 
#'   col="darkgreen", region="device")
#' @param text a character string to be plotted
#' @param region in which region of the device the label should be placed
#' @param pos position relative to the region ("topleft" by default)
#' @param cex magnification of the characters. See \code{\link{par}}
#' @param ... any further parameters (e.g. \code{col}) will be passed to
#'        the \code{\link{text}} function.
#' @import graphics
#' @importFrom grDevices dev.size
#' @export
fig_label <- function(text, region="figure", pos="topleft", cex=NULL, ...) {

  region <- match.arg(region, c("figure", "plot", "device"))
  pos <- match.arg(pos, c("topleft", "top", "topright", 
                          "left", "center", "right", 
                          "bottomleft", "bottom", "bottomright"))

  if(region %in% c("figure", "device")) {
    ds <- dev.size("in")
    # xy coordinates of device corners in user coordinates
    x <- grconvertX(c(0, ds[1]), from="in", to="user")
    y <- grconvertY(c(0, ds[2]), from="in", to="user")

    # fragment of the device we use to plot
    if(region == "figure") {
      # account for the fragment of the device that 
      # the figure is using
      fig <- par("fig")
      dx <- (x[2] - x[1])
      dy <- (y[2] - y[1])
      x <- x[1] + dx * fig[1:2]
      y <- y[1] + dy * fig[3:4]
    } 
  }

  # much simpler if in plotting region
  if(region == "plot") {
    u <- par("usr")
    x <- u[1:2]
    y <- u[3:4]
  }

  sw <- strwidth(text, cex=cex) * 60/100
  sh <- strheight(text, cex=cex) * 60/100

  x1 <- switch(pos,
    topleft     =x[1] + sw, 
    left        =x[1] + sw,
    bottomleft  =x[1] + sw,
    top         =(x[1] + x[2])/2,
    center      =(x[1] + x[2])/2,
    bottom      =(x[1] + x[2])/2,
    topright    =x[2] - sw,
    right       =x[2] - sw,
    bottomright =x[2] - sw)

  y1 <- switch(pos,
    topleft     =y[2] - sh,
    top         =y[2] - sh,
    topright    =y[2] - sh,
    left        =(y[1] + y[2])/2,
    center      =(y[1] + y[2])/2,
    right       =(y[1] + y[2])/2,
    bottomleft  =y[1] + sh,
    bottom      =y[1] + sh,
    bottomright =y[1] + sh)

  old.par <- par(xpd=NA)
  on.exit(par(old.par))

  text(x1, y1, text, cex=cex, ...)
  return(invisible(c(x,y)))
}
