#' @rdname dmtx2graph
#' @export
clean.graph <- function(x, cutoff=NULL) {

  if(!is.null(cutoff)) {
    sel <- apply(x, 1, function(xx) sum(xx < cutoff)) >= 1
    x <- x[ sel, sel, drop=F ]
  }
  x
}


#' Convert a distance matrix to an undirected graph
#'
#' Convert a distance matrix to an undirected graph
#'
#' @param x distance matrix
#' @export

dmtx2graph <- function(x, 
  nodenames=NULL, 
  data=NULL,
  edgemode="undirected", 
  cutoff=Inf, init.RCytoscape=TRUE,
  clean=TRUE) {

  require(graph)
  if(init.RCytoscape) require(RCytoscape)

  diag(x) <- Inf
  sel <- NULL
  if(clean) {
    sel <- apply(x, 1, function(xx) sum(xx < cutoff)) >= 1
    x <- x[ sel, sel, drop=F ]
    nodenames <- nodenames[sel,]
  }


  N <- nrow(x)
  if(is.null(nodenames)) nodenames <- rownames(x)
  if(is.null(nodenames)) nodenames <- 1:N
  
  ret <- new( "graphNEL", nodes= nodenames, edgemode=edgemode )

  ef <- c()
  et <- c()
  ew <- c()

  for(i in 1:(N-1)) {
    for(j in (i+1):N) {
      if(x[i,j] < cutoff) {
        ef <- c(ef, nodenames[i])
        et <- c(et, nodenames[j])
        ew <- c(ew, x[i,j])
      }
    }
  }

  ret <- addEdge( ef, et, ret, ew )
  if(init.RCytoscape) {
    ret <- initEdgeAttribute(ret, attribute.name="weight", attribute.type="numeric", default.value=1)
  }

  ## add additional data
  if(!is.null(data)) {
    for(d in names(data)) {

      if(is.factor(data[[d]])) data[[d]] <- as.character(data[[d]])
      c <- class(data[[d]])
      if(c == "character") c <- "char"
      default <- switch(c, char="", numeric=1, integer=1)

      printf("Initializing %s", d)
      if(init.RCytoscape) {
        ret <- initNodeAttribute(ret, attribute.name=d, attribute.type=c, default.value=default)
      } else {
        nodeDataDefaults(ret, d) <- ""
      }
      if(!is.null(sel)) data[[d]] <- data[[d]][sel]
      nodeData(ret, n=nodenames, attr=d) <- data[[d]]
    }
  }


  ret
}

#' Change q values to logarithmized signed q values
#'
#' Change q values to logarithmized signed q values
#'
#' @param qval a vector of q values
#' @param lfc a vector of directions of change (e.g. log fold changes)
#' @param log whether the q values should be logarithmized
#' @export
qval2signedqval <- function(qval, lfc, log=TRUE) {

  if(log) {
    qval <- -log10(qval)
  }

  qval[ lfc < 0 ] <- -qval[ lfc < 0 ]
  qval
}


#' Call RCytoscape functions to display a graph
#'
#' Call RCytoscape functions to display a graph
#'
#' Call RCytoscape functions to display a graph
#' @param graph a graphNEL object
#' @param name name for the window
#' @param overwriteWindow whether to overwrite the window
#' @param display whether to display graph immediately
#' @param layout layout to use
#' @export
showGraph <- function(graph, name='foo2', overwriteWindow=TRUE, display=TRUE, layout.name="force-directed") {
  require(graph)
  require(RCytoscape)

  cw <- new.CytoscapeWindow(name, graph=graph, overwriteWindow=overwriteWindow)
  if(display) {
    displayGraph(cw)
    if(!is.null(layout)) layoutNetwork(cw, layout.name=layout.name) ; redraw(cw)
  }

  cw
}

#' Calculate the variation of information distances
#'
#' Calculate the variation of information distances, based on mutal
#' information
#' @param x matrix or data frame. Variables should be in rows
#' @export
distVI <- function(x, discretize=TRUE) {
  require(infotheo)

  n <- dimnames(x)
  if(discretize) {
    x <- t(apply(x, 1, function(x) discretize(x)[,1]))
    dimnames(x) <- n
  }

  x.entrop <- apply(x, 1, entropy)
  mi <- mutinformation(as.data.frame(t(x)))

  N <- nrow(mi)
  vd <- sapply(1:N, function(i) sapply(1:N, function(j) x.entrop[i] + x.entrop[j] - 2 * mi[i,j]))
  dimnames(vd) <- dimnames(mi)
  vd

}
