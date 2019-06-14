#' Standarization using a subset of the data
#'
#' Standarization using a subset of the data
#'
#' Standarization using a subset of the data. 
#' @param data either a matrix, data frame or an EList object
#' @param sel a logical vector specifying which samples to use for standarization
#' @param min.var if not NULL, all variables with variance / IQR below this threshold will be removed
#' @param param if TRUE, mean and sd will be used; if FALSE (default), median and IQR will be used for standarization
#' @export
normtosel <- function( data, sel, min.var=1e-16, param=FALSE ) {

  rg <- NULL
  if( is( data, "EList" ) ) {
    rg   <- data
    data <- rg$E
  }

  # calculate the standarization parameters
  if( param ) {
    catf( "Using parametric standarization\n" )
    iqrs <- apply( data[,sel], 1, sd )
    meds <- apply( data[,sel], 1, mean )
  } else {
    catf( "Using non-parametric standarization\n" )
    iqrs <- apply( data[,sel], 1, IQR )
    meds <- apply( data[,sel], 1, median )
  }

  data <- (data - meds) / iqrs

  # get rid of low var variables
  nz   <- iqrs > min.var
  data <- data[ nz, ]

  if(!is.null( rg ) ) {
    rg <- rg[ nz, ]
    rg$E <- data
    return(rg)
  }
  return( data )

}






#' Sample a number of samples by factor levels
#'
#' Sample a number of samples by factor levels
#'
#' Sample a number of samples by factor levels. 
#' @param f a factor
#' @param n sample size. If n < 1, then n will be treated as fraction rather than number
#' @param sel which factor levels to choose from. Default: NULL, meaning every factor level
#' @export
getsubsets <- function( f, n= 100, sel= NULL, verbose=FALSE ) {

  if( ! is(f, "factor") ) f <- factor( f )

  if( is.null( sel ) ) sel <- levels( f )
  nn <- n

  ret <- c()
  for( s in sel ) {

    ff <- which( f == s )
    lf <- length( ff )
    if( n < 1 ) nn <- round( lf * n )
    if( lf < nn ) {
      cat( sprintf( "group %s: not enough samples (%d/%d)\n", s, lf, nn ) )
      next ;
    }
    if(verbose) cat( sprintf( "group %s selecting N=%d out of %d\n", s, nn, lf ) )
    ret <- c( ret, sample( ff, nn ) )
  }

  ret <- 1:length(f) %in% ret

  return( ret )
}

