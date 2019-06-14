#' Create a "difference" gene expression set 
#'
#'  This function takes a \code{limma} \code{EList} object and calculates the
#'    pairwise differences between two groups of samples.
#' 
#' First, the function construct a list of pair IDs which are present in
#' both groups. Next, for each sample in the first group, the corresponding
#' sample from the second group is subtracted from it. This is of course
#' done for every variable (gene or otherwise) in \code{E}.
#'@return   \code{EList} object with as many samples as there are pairs
#' present in both groups. In the \code{target} slot of the object, a new
#' column called \code{difID} is created (if absent) or modified, and includes
#' information about the subtraction.
#'@param E A limma \code{EList} object.
#'@param pairID A factor used to match the samples. Incomplete pairs are allowed, but they will be ignored.
#'@param groupID A factor categorizing the samples in one of two groups.  This factor must have exactly two levels.
#'@export
calcDif <- function( E, pairID, groupID ) {
  require( limma )

  if( ! is.factor( groupID ) ) groupID = factor( groupID )
  groupID_u <- levels( groupID )

  if( length( groupID_u ) != 2 ) 
    stop( "groupID must have exactly two unique values" )

  if( ncol( E ) != length( pairID ) ||
      ncol( E ) != length( groupID ) ) 
    stop( "ID, pairID, groupID must be of length equal to column number of E" )

  E1 <- E[ , groupID == groupID_u[1] ]
  E2 <- E[ , groupID == groupID_u[2] ]

  pairID1 <- pairID[ groupID == groupID_u[1] ]
  pairID2 <- pairID[ groupID == groupID_u[2] ]

  common <- intersect( pairID1, pairID2 )
  E1 <- E1[ , match( common, pairID1 ) ]
  E2 <- E2[ , match( common, pairID2 ) ]

  E.dif <- E1
  E.dif$E <- E1$E - E2$E
  colnames( E.dif ) <- common

  E.dif$targets$difID <- sprintf( "%s-%s", groupID_u[1], groupID_u[2] )

  E.dif
}


