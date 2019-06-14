
#' Shows a rectangle colored by that color
#'
#' Shows a rectangle colored by that color
#' @param c a valid color
#' @export
show.color <- function(c) {
  plot.new()
  rect(0,0,1,1,col=c)
}
