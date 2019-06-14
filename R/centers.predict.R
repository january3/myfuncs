#'@rdname centers.predict
#'@method predict fclust
#'@export
# cmeans / e1071 package
predict.fclust <- function( cl, x ) {
  centers.predict( cl$centers, x )
}

# kmeans
#'@rdname centers.predict
#'@method predict fclust
#'@export
predict.kmeans <- function( cl, x ) {
  centers.predict( cl$centers, x )
}

#' useful for predicting cluster membership from cmeans and kmeans
#'
#' useful for predicting cluster membership from cmeans and kmeans
#'
#' useful for predicting cluster membership from cmeans and kmeans
#'@export
centers.predict <- function( centers, x ) {
  which.cl <- function( xx ) 
    which.min( apply( centers, 1, function( y ) sum( ( y - xx )^2 ) ) )
  ret <- apply( x, 1, which.cl )
  names( ret ) <- rownames( x )
  ret
}

