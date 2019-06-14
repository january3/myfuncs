#' kfold cross-validation with additional features
#'
#' kfold cross-validation with additional features
#'
#' @param response response (categorical)
#' @param grouping_var for LGOCV (leave one group out CV)
#' @param folds number of folds
#' @param x data matrix (rows=variables, cols=samples)
#' @export
#' @importFrom randomForest randomForest
#' @importFrom caret createFolds
mykfold <- function(x, response, grouping_var=NULL, folds=10) {
  
  if(is.null(grouping_var)) grouping_var <- 1:length(x)

  gvuq <- unique(grouping_var)
  kf <- createFolds(gvuq, k=folds)
  kf <- sapply(kf, function(f) which(grouping_var %in% gvuq[f]), simplify=F)

  ret <- sapply(kf, function(f) {
    cat(".")
    sel <- setdiff(1:length(response), f)
    rf.m <- randomForest(t(x[,sel]), factor(response[sel]))
    predict( rf.m, newdata= t(x[,f,drop=F]), type= 'prob' )
  }, simplify=F)
  cat("\n")

  ret <- Reduce(rbind, ret)


  ret <- data.frame( ret )
  ret$decision <- colnames( ret )[ ( ret[,1] < ret[,2] ) + 1 ]
  ret$reality  <- response[Reduce(c, kf)]
  ret$prediction <- ret[,1]
  return( ret )


}


#' Balance a factor
#'
#' given a factor, sample the overrepresented groups such that each factor
#' level is represented the same number of times
#' 
#' @param x a vector
#' @export
balanceset <- function( x ) {

  if( ! is.factor( x ) ) x <- factor( x )

  counts <- summary( x )
  min.c  <- min( counts )
  min.c.n <- names( counts )[ which.min( counts ) ]

  sel <- which( x == min.c.n )
  num.min <- length( sel )
  # catf( "\n%s %d %d\n", min.c.n, min.c, num.min )

  for( l in levels( x )[ levels( x ) != min.c.n ] ) {
    sel <- c( sel, sample( which( x == l ), num.min ) )
  }

  return( sel )
}



#' Determine optimal number of variables
#'
#' like xpredict, but constructs a secondary, reduced model and reports
#' only AUC for each of the 100 replicates
#'
#' @param x,y data matrix, rows=variables, cols=samples. row names matter
#' @param x.response,y.response vector of responses
#' @param rep number of replicates for each number of variables
#' @param nvar number of variables to test
#' @seealso xpredict
#' @export
xpredict.nvar <- function(x, y, x.response, y.response, rep=100,
  nvar=c(2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 25, 30, 40, 50, 75, 100, 125))  {

  set.x <- rownames(x)
  set.y <- rownames(y)

  common <- intersect(set.x, set.y)

  x <- x[common,,drop=F]
  y <- y[common,,drop=F]
  print(dim(x))
  print(dim(y))

  res <- sapply(1:rep, function(i) {
    catf("\r%d", i)
    rf <- randomForest(t(x), factor(x.response), importance=T)
    var.l <- rownames(rf$importance)[order(-rf$importance[,4])]
    sapply(nvar, function(n) {
      sel <- var.l[1:n]
      x2 <- x[sel,]
      rf2 <- randomForest(t(x2), factor(x.response))
      pr <- predict(rf2, newdata=t(y), type="prob")
      roc(response=y.response, predictor=pr[,1])$auc[1]
    })
  })

  cat("\n")
  res
}


#' Cross predict from one data set to another
#'
#' Given two matrices, train the model on the first, apply to the second
#'
#' @param x,y data matrix, rows=variables, cols=samples. row names matter
#' @param x.response,y.response vector of responses
#' @param nvar If not NULL, build a secondary model with nvar variables
#' @param rep number of replicates for each number of variables
#' @param cleanx drop samples from x if they are absent in y
#' @param cleany drop samples from y if they are absent in x
#' @param cutoff.method See "methods" from optimal.cutpoints
#' @param return.list if TRUE, returns a list with all additional information rather than a data frame
#' @param rf if rf model provided, it will be used instead of creating a new one
#' @import OptimalCutpoints
#' @importFrom randomForest randomForest
#' @export
xpredict <- function(x, y, x.response, y.response, nvar=NULL, 
  cleanx=FALSE, cleany=FALSE, cutoff.method="MaxProdSpSe", 
  return.list=FALSE,
  rf=NULL) {

  set.x <- rownames(x)
  set.y <- rownames(y)

  common <- intersect(set.x, set.y)

  x <- x[common,,drop=F]
  y <- y[common,,drop=F]

  if(cleanx) 
    x <- x[ , !colnames(x) %in% colnames(y),drop=F]

  if(cleany)
    y <- y[ , !colnames(y) %in% colnames(x),drop=F]

  if(!is.factor(x.response))
    x.response <- factor(x.response)

  if(is.null(rf)) 
    rf <- randomForest(t(x), x.response, importance=T)

  if(!is.null(nvar)) {
    sel <- order(-rf$importance[,4])[1:nvar]
    x <- x[sel,]
    rf <- randomForest(t(x), x.response, importance=T)
  }



  ret <- data.frame(predict(rf, newdata=t(y), type="prob"))

  cutpoints <-
    optimal.cutpoints("pr", "re", 
    data=data.frame(pr=rf$votes[,1], re=x.response),
    tag.healthy=rf$classes[2],
    methods=cutoff.method)
    
  cutoff <- 
    cutpoints[[cutoff.method]][[1]]$optimal.cutoff$cutoff[1]

  attr(ret, "cutoff") <- cutoff
  ret$decision <- colnames( ret )[ ( ret[,1] < ret[,2] ) + 1 ]
  ret$decision2 <- colnames(ret)[ (ret[,1] < cutoff) + 1 ]
  ret$reality  <- y.response
  ret$prediction <- ret[,1]
  if(return.list) {
    list(ret=ret, rf=rf, cutoff=cutoff, cutpoints=cutpoints)
  } else {
    ret
  }
}




#' plot a series of ROC prediction objects as boxplot
#'
#' @param x a series of roc predictions (a list)
#' @param xlim xlim for the plot
#' @export
rocseries_boxplot <- function(x, xlim=c(0.25, 1), rcol="#ccddcc", bty="n", mm=0.4, ...) {
  require(pROC)
  oldpar <- par(mar= c(5,1,4,9)+0.1)
  on.exit(par(oldpar))

  s <- sapply(x, function(xx) as.vector(roc(response=xx$reality, predictor=xx$prediction, ci=T)$ci))
  print(s)
  n <- ncol(s)
  y <- -1:-n

  plot(x=NULL, y=NULL, xlim=xlim, ylim=c(-(n+0.5), -0.5), bty=bty, yaxt="n", ylab="", xlab="AUC", ...) 
  segments(s[1,], y, s[3,], y, col="grey", lwd=4)

  #rect((1:n)-mm, s[1,], (1:n)+mm, s[3,], col=rcol)
  abline(v=0.5, col="#993333", lwd=3)
  abline(v=(6:10)/10, col="grey", lwd=1)
  #segments((1:n)-mm, s[2,], (1:n)+mm, s[2,], lwd=2)
  axis(4, at=y, labels=names(x), tick=F, las=2, cex.axis=1.5, mgp=c(0,0,0))
  points(s[2,], y, pch=18, cex=1.4)
}

#' Return a table with AUC for each prediction in a series
#'
#' based on prediction objects, returns a table with auc, ci and p-value
#' method: method to adjust for multiple testing
#' 
#' @export
rocseries_table <- function(x, method="fdr") {
  require(pROC)

  s <- sapply(x, function(xx) as.vector(roc(response=xx$reality, predictor=xx$prediction, ci=T)$ci))
  s <- t(s)
  pvals <- sapply(x, auc.pval)

  ret <- data.frame(name=names(x), AUC=s[,2], CI.L=s[,1], CI.R=s[,3], p.value=pvals)
  ret$q.value <- p.adjust(ret$p.value, method=method)
  ret$star <- pval2star(ret$q.value, thresholds=c(0.05, 0.01, 0.001))
  ret
}


#' shorthand to return the AUC calculated by pROC
#'
#' @param x list or data frame containing columns "reality" and "predictor"
#' @return a named vector containing AUC, confidence intervals and a p-value
#' @importFrom pROC roc
#' @export
auc <- function(x) {
  pval <- auc.pval(x)
  ret <- c(roc(response=x$reality, predictor=x$prediction, ci=TRUE)$ci[1:3], pval)
  names(ret) <- c("CI.L", "AUC", "CI.R", "p.Value")
	ret
}

#' shorthand to return the the p-value of AUC calculated by pROC
#'
#' run the Wilcoxon test on predictions vs reality
#' @param only.pval if FALSE, returns the whole result of wilcox.test
#' @param x list or data frame containing columns "reality" and "predictor"
#' @export
auc.pval <- function(x, only.pval=TRUE) {
  ret <- wilcox.test(x$prediction ~ x$reality)
  if(only.pval) ret <- ret$p.value
  ret
}

