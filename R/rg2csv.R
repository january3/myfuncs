#' Save an RG object as CSV files
#'
#' Save an RG object as CSV files
#'
#' Save an RG object as CSV files. Each of the components E, genes and
#' targets will be save as a separate CSV file starting with \code{stem}.
#' File names will be the stem + underscore + qualifier (data for E, features
#' for genes and samples for targets) + .csv.
#' @param rg RG object from limma
#' @param stem File stem to save to
#' @return NULL
#' @export
rg2csv <- function( rg, stem= "export" ) {

  write.csv( rg$E, file=paste0( stem, "_data.csv" ) )
  write.csv( rg$genes, file=paste0( stem, "_features.csv" ) )
  write.csv( rg$targets, file=paste0( stem, "_samples.csv" ) )


}


