#' A combined beeswarm / boxplot
#'
#' A combined beeswarm / boxplot
#'
#' A combined beeswarm / boxplot
#'
#' @export
showgene <- function( data, group, main= "", pch= 19, ylim=NULL,
                         xlab= "", ylab= "log2 expression", las= 2, pwcol= NULL, 
                         col=NULL, bty="n",
                         signboxes=NULL,
                         start.at.zero=FALSE,
                         comparisons=NULL, 
                         test.func=wilcox.test, ... ) {


  require( beeswarm )

  nas   <- is.na(data) | is.na(group)
  data  <- data[!nas]
  group <- group[!nas]

  if(!is.factor(group)) { group  <- factor( group ) }

  if(is.null(ylim)) {
    ylim <- range(data) + c(0, 0.1 * (max(data) - min(data)))
  }
  if(start.at.zero) ylim[1] <- 0

  pal    <- mypalette( n= length( unique( group ) ) )
  if( ! is.null( pwcol ) & length( pwcol ) == 1 ) pwcol <- rep( pwcol, length( group ) )
  if( is.null(pwcol) & is.null(col)) pwcol= pal[ group ]

  #print(group)

  if(!is.null(signboxes)) {
    #plot.new()
    #signboxheight <- strheight("Qq") * 1.5 * (ylim[2] - ylim[1])

    ylim[2] <- ylim[2] + (ylim[2] - ylim[1]) * 0.25
  }


  
  beeswarm( data ~ group, 
    pch= pch, xlab= xlab, ylab= ylab, main= main, las= las, 
    pwcol= pwcol, col=col, bty=bty, ylim=ylim,
    ... )

  boxplot( data ~ group, col= "#ffffff00", add= T, yaxt= "n", xaxt= "n", main= "", outline= F, frame=F )
  signboxheight <- strheight("Qq") * 1.5 

  if(!is.null(comparisons)) {
    ll <- levels(group)
    h  <- ylim[2]
    comparisons <- strsplit(comparisons, "-")
    i <- 1
    for(c in comparisons) {
      h  <- ylim[2] - (i-1) * signboxheight * 1.2
      i <- i + 1
      i1 <- which(ll == c[1])
      i2 <- which(ll == c[2])
      p <- test.func(data[group == c[1]], data[group == c[2]])$p.value
      if(p < 0.05) {
        signbox(h, i1, i2, p)
      }
    }
  }

  if(!is.null(signboxes)) {
    ll <- levels(group)
    comparisons <- strsplit(names(signboxes), "-")
    for(i in 1:length(signboxes)) {
      #printf("drawink signbox %d", i)
      h  <- ylim[2] - (i-1) * signboxheight * 1.2
      #printf("h=%.2f", h)
      c <- comparisons[[i]]
      i1 <- which(ll == c[1])
      i2 <- which(ll == c[2])
      p <- signboxes[i]

      if(!is.na(p) && p < 0.05) {
        signbox(h, i1, i2, p, height=signboxheight)
      }
    }
  }


  return( invisible( list( groups= levels( group ), col= pal ) ) )
}



signbox <- function( h1, x1, x2, p1, width= 0.75, height= NULL ) {
  if( is.null( height) ) height <- strheight( "Qp" ) * 1.5
  x <- sort(c(x1,x2))
  h1 <- h1 - height 
  h1 <- rep( h1, 2 )
  mid <- ( x[1] + x[2] ) / 2
  segments( c( x[1], mid + width / 2 ), h1, c( mid - width / 2, x[2] ), h1 )
  segments( c( x[1], x[2] ), h1 - height / 2, c( x[1], x[2] ), h1 )
  rect( mid - width / 2, h1[2] - height / 2, mid + width / 2, h1[2] + height / 2 )
  #text( mid, h1[2], pval2str( p1, thresholds= c( 0.05, 0.01, 0.001 ) ), cex= 1.5 )
  text( mid, h1[2], pval2str( p1, numonly=TRUE), cex= 1.2 )
}
