#' Local Polynomial Regression Fitting with Variability bands
#'
#' Local Polynomial Regression Fitting with Variability bands
#'
#' Slightly modified version from the msir package. See the manual for
#' loess.sd in the msir package for help.
#'
#' @export
myloess.sd <- function (x, y=NULL, level=0.95,  ...) 
{


    level <- 1 - (1 - level)/2
    xy <- xy.coords(x, y)
    x <- xy$x
    x0 <- unique(sort(x))
    y <- xy$y

    sel <- !is.na(x) & !is.na(y)
    if(sum(!sel) > 0) {
      warning(sprintf("Removing %d pairs containing NA", sum(!sel)))
      x <- x[sel]
      y <- y[sel]
      x0 <- x0[sel]
    }


    mod <- loess(y ~ x, ...)
    yfit <- predict(mod, data.frame(x = x0))
    r <- residuals(mod)
    modr <- loess(I(r^2) ~ x, ...)
    sd <- sqrt(pmax(0, predict(modr, data.frame(x = x0))))

    lo.conf <- predict(mod, newdata=x0, se=T)
    lo.conf$upper <- lo.conf$fit + qt(level, lo.conf$df)*lo.conf$se.fit
    lo.conf$lower <- lo.conf$fit - qt(level, lo.conf$df)*lo.conf$se.fit

    list(model = mod, 
        x = x0, y = yfit, sd = sd, 
        pr.upper = yfit + qnorm(level) * sd, 
        pr.lower = yfit - qnorm(level) * sd,
        ci.upper = lo.conf$upper,
        ci.lower = lo.conf$lower)
}

#' Plot an object from myloess.sd
#'
#' Plot an object from myloess.sd
#'

#' @export
plotMyloess <- function(lo, add=T, col="#990066", pred.int=TRUE, conf.int=TRUE, ...) {

  if(!add) {
    plot(lo$x, lo$y, ...)
  }

  x <- c(lo$x, rev(lo$x))

  if(conf.int)
    polygon(x, c(lo$ci.lower, rev(lo$ci.upper)), border=F, col=paste0(col, "66"))

  if(pred.int)
    polygon(x, c(lo$pr.lower, rev(lo$pr.upper)), border=F, col=paste0(col, "33"))

  lines(lo$x, lo$y, lwd=2, col=col)

}
