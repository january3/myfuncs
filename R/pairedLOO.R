#' RF LOO removing whole group of subjects
#'
#' RF LOO removing whole group of subjects
#'
#' RF LOO removing whole group of subjects. For each unique value G of group,
#' the training set consists of all samples for which group is different from
#' G, and the test set contains only the samples for which group is equal G
#' @param data Variables for machine learning
#' @param class Response variable (two classes task)
#' @param group factor defining how the samples are grouped
#' @export
pairedLOO <- function( data, class, group ) {
  require( randomForest )

  i <- 1
  N <- length( class )
  ret <- NULL
  for( p in unique(group) ) {
    catf( "%s %d/%d\r", p, i, N )
    i <- i + 1

    train <- group != p
    test  <- group == p

    rf <- randomForest( data[train,], class[train] )

    pr <- predict( rf, newdata=data[test,], type="prob" )
    ret <- rbind( ret, data.frame(pr, reality=class[test] ) )
  }

  catf( "\n" )

  ret <- data.frame( ret )

  ret$prediction <- ret[,1]
  ret$decision   <- unique(class)[ (ret[,1] < ret[,2]) + 1 ]
  #ret$reality    <- groups
  ret
}


