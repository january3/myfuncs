#' Write expression data in the braindead GCT format
#'
#' Write expression data in the braindead GCT format
#'
#' Write expression data in the braindead GCT format. See http://www.broadinstitute.org/cancer/software/gsea/wiki/index.php/Data_formats
#' @param E a matrix with the data
#' @param Name  A vector with probe or gene names
#' @param Description Optional description vector
#' @param FileName name of the file to write to
#' @param sampleNames unique names of the samples
#' @export
saveAsGCT <- function(E, Name, FileName, Description=NULL, sampleNames=NULL) {

  sink(FileName)
  on.exit(sink())

  cat( "#1.2\n" )

  i <- nrow(E)
  j <- ncol(E)

  catf( "%d\t%d\n", i, j )

  if(is.null(sampleNames))
    sampleNames <- colnames(E)

  if(is.null(sampleNames) || length(sampleNames) != j) 
    sampleNames <- rep( "ID", j )

  if(any(duplicated(sampleNames))) 
    sampleNames <- paste0(sampleNames, 1:j, sep="_")


  if(is.null(Description)) Description <- rep("", i)

  df <- data.frame(NAME=Name, Description=Description, E )
  write.table(df, sep="\t", row.names=FALSE, quote=FALSE)

}


#' Write pheno data as braindead CLS format
#'
#' Write pheno data as braindead CLS format
#'
#' Write pheno data as braindead CLS format.
#' See http://www.broadinstitute.org/cancer/software/gsea/wiki/index.php/Data_formats
#' @param groups A factor
#' @param FileName name of the file to write to
#' @export 
saveAsCLS <- function(groups, FileName) {

  sink(FileName)
  on.exit(sink())

  n <- length(groups)
  g <- length(unique(groups))

  catf( "%d %d 1\n", n, g )
  catf( "# %s\n", paste( unique(groups), collapse=" ") )
  cat( paste(groups, collapse=" ") )
  cat("\n" )

}
