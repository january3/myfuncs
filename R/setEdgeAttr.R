#' set Edge attributes in an Rgraphviz graph
#'
#' set Edge attributes in an Rgraphviz graph
#'
#' set Edge attributes in an Rgraphviz graph

#' @param graph object of class Ragraph
#' @param attribute name of the attribute to set
#' @param value vector of values for the attribute
#' @param ID1, ID2 vectors with node names defining edges
#' @return the modified graph object with attributes set
#' @export
setEdgeAttr <- function( graph, attribute, value, ID1, ID2 ) {
  
  if( length(value) < 1 || length(value) != length(ID1) || length(value) != length(ID2) ) {
    stop( "value, ID1 and ID2 must be of equal length and their length must be at least 1" )
  }

  idfunc <- function(x) paste0( sort(x), collapse="~" )
  all.ids <- sapply( AgEdge(graph), 
    function(e) idfunc( c( attr(e, "head"), attr( e, "tail" ))))
  
  sel.ids <- apply(cbind( ID1, ID2 ), 1, idfunc )
  
  if(!all(sel.ids %in% all.ids)) {
    warning( sprintf( 
      "Following edges not found:\n%s\n",
      paste( sel.ids[ ! sel.ids %in% all.ids ], collapse=", " )
      ))
    stop( "only existing edges, please" )
  }

  sel <- match( sel.ids, all.ids )

  for(i in 1:length(sel)) {
    attr( attr( graph, "AgEdge" )[[ sel[i] ]], attribute ) <- value[i]
  }
  
  return(graph)
}

