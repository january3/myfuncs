#' @importFrom ellipse ellipse
#' @export
corellipse <- function(x, y, level=0.95, ...) {
  x0 <- mean(x)
  y0 <- mean(y)

  co <- cov(cbind(x,y))
  lines(ellipse(co, centre=c(x0,y0), level=level), ...)



}
