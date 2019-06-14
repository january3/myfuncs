#' Last observation moved forward
#'
#' Replaces all NA values with last non-NA value
#'
#' From http://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value
#' @export
na.lomf <- function(x) {

    na.lomf.0 <- function(x) {
        non.na.idx <- which(!is.na(x))
        if (is.na(x[1L])) {
            non.na.idx <- c(1L, non.na.idx)
        }
        rep.int(x[non.na.idx], diff(c(non.na.idx, length(x) + 1L)))
    }

    dim.len <- length(dim(x))

    if (dim.len == 0L) {
        na.lomf.0(x)
    } else {
        apply(x, dim.len, na.lomf.0)
    }
}
