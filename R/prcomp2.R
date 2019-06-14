#' A wrapper around prcomp
#'
#' A wrapper around prcomp
#'
#' This function first transposes the matrix and then eliminates zero variance rows.
#' @export
prcomp2 <- function(x, removeZeroVariance=TRUE, transpose=TRUE, scale=TRUE, ...) {

  if(transpose) {
    x <- t(x)
  }

  if(removeZeroVariance) {
    vars <- apply(x, 1, var)
    x <- x[ vars > 0, ]
  }

  prcomp(x, .scale=scale, ...)

}
