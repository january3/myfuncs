#' Convert a p-value to a character corresponding to p-value levels
#'
#' Convert a p-value to a character corresponding to p-value levels
#'
#' Convert a p-value to a character corresponding to p-value levels.
#' \code{pval2str} returns a formatted number with, optionally, stars denoting
#' the significance of the p-value. \code{pval2star} returns just the stars.
#' @param pv a p-value
#' @param numonly do not append stars to the output of \code{pval2str}
#' @param ns string to be used for values below the lowest threshold
#' @param thresholds numeric vector defining three thresholds for one, two
#' or three stars
#' @return Both functions return character vector with p-values converted to respective strings.
#' @examples 
#' # will return c( "**" )
#' pval2star( 0.0005 )
#' @export
pval2str <- function( pv, numonly= FALSE, ... ) {

  thresholds <- c(0.01, 0.001, 0.0001, 0)
  strings    <- sprintf("(%s)", c("NS", "*", "**", "***"))
  formats    <- c("%.2f", "%.3f", "%.4f", "%.3e")
  ret <- c()
  for(i in length(thresholds):1) {
    sel <- pv > thresholds[i]
    ret[sel] <- sprintf(formats[i], pv[sel])
  }

  if(!numonly)
    ret <- paste(ret, sprintf("(%s)", pval2star(pv, ns="NS", ...)))

  ret
}

#' @export
#' @rdname pval2str
pval2star <- function( pv, ns= "", thresholds= c( 0.05, 1e-3, 1e-4 ) ) {

  ret <- as.character(pv)

  thresholds <- c(thresholds, 0)
  strings    <- c(ns, "*", "**", "***")

  for(i in length(thresholds):1)
    ret[pv > thresholds[i]] <- strings[i]

  ret
}


