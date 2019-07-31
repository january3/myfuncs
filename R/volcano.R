
#' @export
volcano <- function(p, lfc, lfc.thr=1, p.thr=0.01, ylim=NULL) {

  if(is.null(ylim)) ylim <- rev(range(p))
  plot(lfc, p, ylim=ylim, log="y", bty="n", pch=19, col="#33333333",
    xlab="log(Fold)", ylab="FDR")

  sel <- p < p.thr & abs(lfc) > lfc.thr
  points(lfc[sel], p[sel], pch=19, col="#cc000033")
}


