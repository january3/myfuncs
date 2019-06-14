#' Pairs plot using smoothScatter
#'
#' Pairs plot using smoothScatter
#'
#' Pairs plot using smoothScatter
#'@export
genePairs <- function( x, y= NULL, controls= NULL, xlim= NULL, ylim= NULL, cor.method="pearson", ... ) {

  if( is.null( y ) ) {
    normal.plot= T
  } else {
    normal.plot= F
  }

  lower.panel <- function( x, y ) {
    sel <- !is.na(x) & !is.na(y)
    x <- x[sel]
    y <- y[sel]
    usr <- par( "usr" ) 
    on.exit( par( usr ) ) 
    par( usr= c( 0, 1, 0, 1 ) ) 
    cc <- cor( x, y, method=cor.method )
    text( 0.5, 0.5, sprintf( "%.2f", cc ), cex= 1 + abs( cc ) ) 
  }

  xlim0 <- xlim
  ylim0 <- ylim

  upper.panel <- function( x, y, xlim= xlim0, ylim= ylim0 ) {
    par( new= T ) 
    sel <- !is.na(x) & !is.na(y)
    x <- x[sel]
    y <- y[sel]
    if( is.null( xlim ) ) xlim= range( x )
    if( is.null( ylim ) ) ylim= range( y )
    smoothScatter( x, y, xlim= xlim, ylim= ylim ) 
    if( ! is.null( controls ) ) {
      points( x[ controls ], y[ controls ], pch= 19, cex= 0.3, col= "#ee333366" ) 
    }
    abline( 0, 1, col= "#cccccc" ) 
  }

  xypanel <- function( x, y ) {
    sel <- !is.na(x) & !is.na(y)
    x <- x[sel]
    y <- y[sel]
    smoothScatter( x, y ) 
    if( ! is.null( controls ) ) {
      points( x[ controls ], y[ controls ], pch= 19, cex= 0.3, col= "#ee333366" ) 
    }
    abline( 0, 1, col= "#cccccc" ) 
  }


  if( normal.plot )
    pairs( x, lower.panel= lower.panel, upper.panel= upper.panel, ...  )
  else 
    pairsxy( x, y, panel= xypanel )


}


#'@export
#'@rdname genePairs
pairsxy <- function( x, y, panelfunc= NULL, xlim= NULL, ylim= NULL ) {

  nrows <- ncol( y ) # y changes between rows
                      # so row 2 has on y axis y[,2]
  ncols <- ncol( x ) # x changes between columns
                      # so col 2 has on x axis x[,2]
  rn <- colnames( y ) # rownames
  cn <- colnames( x ) # colnames

  if( nrows != 1 || ncols != 1 ) 
    oldpar <- par( mfrow= c( nrows, ncols ), mar= c( 4, 4, 1, 1 ) )
  on.exit( par( oldpar ) )

  if( is.null( panelfunc ) ) panelfunc <- plot

  for( r in 1:nrows ) {
    yy <- y[,r]

    for( c in 1:ncols ) {
      xx <- x[,c]

      if( c == 1 ) ylab <- rn[ r ]
      else         ylab <- NA
      if( r == nrows ) xlab <- cn[ c ]
      else             xlab <- NA
      
      if( is.null( xlim ) ) xlim0= range( xx )
      else xlim0 = xlim
      if( is.null( ylim ) ) ylim0= range( yy )
      else ylim0 = ylim
        

      plot( NULL, type= "n", xlim= xlim0, ylim= ylim0, xlab= xlab, ylab= ylab )
      par( new= TRUE )
      panelfunc( xx, yy )
    }
  }

}


