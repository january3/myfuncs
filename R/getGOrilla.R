'[.GORilla' <- function( g, i ) {

  g$result.table <- g$result.table[i,]

  g
}


#' @export
#' @method print GORilla
#' @S3method print GORilla
#' @rdname getGOrilla
print.GORilla <- function( g ) {

  catf( "GORilla ID: %s\n", g$id )
  catf( "Result URL: %s\n", g$result.url )
  print( g$result.table[ ,c(1:9, 11)] ) 


}


#' Wrapper to run the online GO service GOrilla
#'
#' Wrapper to run the online GO service GOrilla
#'
#' Wrapper to run the online GO service GOrilla
#' @param x character vector with sorted gene identifiers or (in two-list mode)
#' the identifiers of differentially expressed genes
#' @param y if in two list mode: character vector with gene identifiers of the background gene set
#' @param ontology Ontology to use (proc, func or comp. Default: proc)
#' @param species Can be HOMO_SAPIENS, 
#'     ARABIDOPSIS_THALIANA,
#'     SACCHAROMYCES_CEREVISIAE,
#'     CAENORHABDITIS_ELEGANS,
#'     DROSOPHILA_MELANOGASTER,
#'     DANIO_RERIO,
#'     HOMO_SAPIENS,
#'     MUS_MUSCULUS or
#'     RATTUS_NORVEGICUS.
#' @param pval.thresh The p-value threshold parameter. Allowed values: 1e-3, 1e-4, ..., 1e-11
#' @param download Should \code{getGOrilla} attempt to download the data tables.
#' @param ... arguments to be passed to or from other methods.
#' @return An object of the class GORilla
#' @export
getGOrilla <- function( x, y= NULL, 
  ontology= "proc", 
  species= "HOMO_SAPIENS",
  pval.thresh= 0.001, 
  download= TRUE ) {

  require( RCurl )

  form.uri     <- "http://cbl-gorilla.cs.technion.ac.il/servlet/GOrilla"
  result.url.f <- "http://cbl-gorilla.cs.technion.ac.il/GOrilla/%s/GO%s"

  pval.thresh <- gsub( "0*$", "", sprintf( "%f", pval.thresh ) )

  allowed.pvals <- c( "0.001", "0.0001", "0.00001", "0.000001",
      "0.0000001", "0.00000001", "0.000000001",
          "0.0000000001", "0.00000000001" )

  if( ! pval.thresh %in% allowed.pvals ) {
    print( "pval should be one of the following:" )
    print( allowed.pvals )
    stop( "Incorrect pval.thresh" )
  }

  catf( "Querying server %s...\n", form.uri )

  species <- match.arg( species, c(
      "ARABIDOPSIS_THALIANA",
      "SACCHAROMYCES_CEREVISIAE",
      "CAENORHABDITIS_ELEGANS",
      "DROSOPHILA_MELANOGASTER",
      "DANIO_RERIO",
      "HOMO_SAPIENS",
      "MUS_MUSCULUS",
      "RATTUS_NORVEGICUS"
  ) )

  # "all" should be included, but the results will be more complex
  ontology <- match.arg( ontology, c( "proc", "func", "comp" ) ) # , "all" ) )

  t_x <- paste( x, collapse= "\n" )

  # default: single list mode (mhg)
  params <- list( 
    target_set=      t_x, 
    application=     "gorilla",
    run_gogo_button= "Search Enriched GO terms", 
    pvalue_thresh=   pval.thresh,
    run_mode=        "mhg", 
    species=         species,
    db=              ontology,
    fast_mode=       1, 
    output_excel=    1 )

  # two lists mode (hg)
  if( ! is.null( y ) ) {
    t_y <- paste( y, collapse= "\n" )
    params$background_set <- t_y
    params$run_mode <- "hg"
  }


  h        <- getCurlHandle( .opts=list( header= TRUE, followlocation= TRUE ) )
  response <- postForm( form.uri, curl= h, .params= params, .opts= list( header= FALSE, followlocation= TRUE )  )

  info <- getCurlInfo( h )
  id <- gsub( ".*=", "", info$effective.url )
  catf( "Got response, Query ID=%s\nwaiting for the effective URL from %s\n", id, info$effective.url )

  max.tries <- 9
  i <- 1
  while( getURL( info$effective.url ) != "" ) {
    catf( "\rwaiting... %d", i )
    if( i > max.tries ) stop( "Waiting takes too long" )
    Sys.sleep( 1 )
  }

  # return
  ret <- list( id= id, 
    result.url= sprintf( result.url.f, id, "Results.html" ),
    table.url=  sprintf( result.url.f, id, ".xls" ),
    img.url=    sprintf( result.url.f, id, ".png" )
     )

  if( download ) {
    catf( "Downloading data\n" )

    if( url.exists( ret$result.url ) ) 
      ret$result.text <- getURL( ret$result.url )
    else 
      warning( "Result URL does not exist" )

    if( url.exists( ret$table.url ) ) {
      ret$result.table <- try( read.table( ret$table.url, sep= "\t", quote= "", comment.char= "", header= TRUE, stringsAsFactors= FALSE ) )
      if( class( ret$result.table ) == "try-error" ) ret$result.table <- NULL

      if( ! is.null( ret$result.table ) ) {
        sel <- 3:9
        ret$result.table[ sel ] <- apply( ret$result.table[ sel ], 2, function( x ) as.numeric( gsub( ",", "", x ) ) )
        ret$result.table$score <- -log10( ret$result.table$P.value ) * ret$result.table$Enrichment
        ret$result.table <- ret$result.table[ order( ret$result.table$score, decreasing= T ), ]
      }

    }
    else
      warning( "Result table URL does not exist" )
  }

  catf( "done.\n" )
  class( ret ) <- c( class( ret ), "GORilla" )
  invisible( ret )
}


