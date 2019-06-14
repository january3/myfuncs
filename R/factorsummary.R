#' Convert columns of a data.frame to factors and show summary
#'
#' Convert columns of a data.frame to factors and show summary
#'
#' Shortcut to convert the columns of a data.frame to factors and then run
#' the summary function on the resulting data frame. By default, columns
#' which either have as many unique values as there are rows, or only one
#' unique value are filtered.
#' @param data a data frame
#' @param filter Whether boring columns should be omitted.
#' @export
factorsummary <- function( data, filter=TRUE ) {

  nr <- nrow(data)

  if( filter ) {

    boring <- apply(data, 2, function(x) {
      nu <- length(unique(x))
      nu == 1 || nu == nr
    })

    data <- data[ , ! boring ]
  }
  
  if( !is( data, "data.frame" ) ) stop( "data must be a data frame" )
  summary(data.frame(apply(data, 2, function(x) factor(x))))
}



#' @export
removeConstant <- function(df) {

  sel <- apply(df, 2, function(x) all(x == x[1]))
  df[,!sel,drop=F]

}
