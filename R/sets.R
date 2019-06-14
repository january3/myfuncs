#' Set difference operator
#'
#' Set difference operator
#' @param x,y a vector
#' @rdname setdiff
#' @export
`%diff%` <- function(x, y) setdiff(x, y)

#' All contained in operator
#'
#' All contained in operator
#' @rdname setdiff
#' @export
`%allin%` <- function(x, y) all(x %in% y)


#' Set sum operator
#'
#' Set sum operator
#' @rdname setdiff
#' @export
`%sum%` <- function(x, y) unique(c(x, y))

#' Set equal operator
#'
#' Set equal operator
#' @rdname setdiff
#' @export
`%eq%` <- function(x, y) setequal(x, y)

#' Set "not in" operator
#'
#' Set "not in" operator
#' @rdname setdiff
#' @export
`%notin%` <- function(x, y) ! (x %in% y)

#' Set intersection operator
#'
#' Set intersection operator
#' @rdname setdiff
#' @export
`%and%` <- function(x, y) intersect(x, y)
