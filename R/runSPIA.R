#' wrapper for the spia function. 
#'
#' wrapper for the spia function. 
#'
#' wrapper for the spia function. 
#' @export
runSPIA <- function( fit, coef= 1, organism= "mmu", p.value= 0.05, lfc= 1, eid.field= "EID" ) {



  require( SPIA )

  tt <- topTable( fit, number= Inf, sort= "n", coef= coef )
  tt <- tt[ !is.na( tt[ , eid.field ] ), ]

  tt_de <- topTable( fit, number= Inf, coef= coef, p.value= p.value, lfc= lfc )
  tt_de <- tt_de[ !duplicated( tt_de$EID ), ]

  spia_de <- tt_de$logFC
  names( spia_de ) <- tt_de[ , eid.field ]

  res_spia <- spia( de= spia_de, all= as.character( tt[ , eid.field ] ), organism= organism, plots= F )

  return( res_spia )
}

