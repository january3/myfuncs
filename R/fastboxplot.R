#' A boxplot consisting of lines only 
#'
#' A boxplot consisting of lines only for fast drawing
#'
#' Quick to draw and hundreds of boxes can be fit. I use it to visualize
#' microarray distributions.
#' @param x a matrix or vector; if it is a matrix, each box plot will correspond to a column of this matrix
#' @param g grouping of values (by default, 'col(x)')
#' @param columns number of columns on the plot
#' @param log whether the scale should be logarithmic
#' @param thinline thickness of the thin line (range)
#' @param thickline thickness of the thick line (.25 - .75 quantile)
#' @param xlab,ylab labels for the x and y axes
#' @export
fastboxplot <- function(x, 
  g=NULL, columns=1, log=FALSE, 
  thinline=0.1,
  thickline=1,
  ylab="",
  xlab="",
  ...) {

  iqrs <- function(.x) c(min(.x), quantile(.x, c(0.25, 0.75)), max(.x), median(.x))

  if(is.null(g)) g <- col(x)
  if(is.vector(x)) {
    #x <- as.vector(x)
    stats <- tapply(x, g, iqrs)
    stats <- t(simplify2array(stats))
  } else {
    stats <- t(apply(x, 2, iqrs))
  }


  n <- length(unique(g))

  npc <- ceiling(n/columns)
  rx <- range(x)

  dx <- rx[2] - rx[1] + 0.1
  plot(NULL, 
    ylim=c(1, npc), 
    xlim=c(rx[1], rx[1] + columns * dx), 
    bty="n", 
    xaxt="n", 
    ylab=ylab, 
    xlab=xlab,
    ...)

  ticks <- axisTicks(range(x), log=log)

  for(i in 1:columns) {
    n0 <- (i-1)*npc + 1
    n1 <- min(i*npc, n)
    sel <- n0:n1
    y <- 1:(n1 - n0 + 1)
    print(sprintf("%d - %d", n0, n1))
    ddx <- (i-1)*dx
    axis(1, at=ticks + ddx, labels=ticks)
    segments(ddx + stats[sel,1], y, ddx + stats[sel,4], y, 
      lwd=thinline)
    segments(ddx + stats[sel,2], y, ddx + stats[sel,3], y, 
      lwd=thickline)
    points(ddx + stats[sel,5], y, pch=19, cex=0.5)
    mtext(sprintf("%d - %d", n0, n1), side=1, at=ddx + rx[1] + dx/2, line=2)
  }
}
