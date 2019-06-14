#' Smart counters for rmarkdown
#'
#' Smart counters for rmarkdown
#' @param label character string consisting of a prefix, `_` (underscore) and actual label, e.g. `fig_label1`
#' @export
cc <- function(label) {
  label <- as.character(substitute(label))
  prefix <- unlist(strsplit(label, "_"))[1]
  message(prefix)
  if(!exists("counters")) counters <<- list()

  if(is.null(counters[[prefix]]))
    counters[[prefix]] <<- c()

  if(is.na(match(label, counters[[prefix]]))) {
    counters[[prefix]] <<- c(counters[[prefix]], label)
  }
  return(match(label, counters[[prefix]]))
}

