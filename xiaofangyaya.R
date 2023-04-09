#' Calculate classification model statistical indicators
#'
#' @param n Denotes the decimals retained in the result \code{\link[base]{round}}
#' @description Calculate performance metrics of classification algorithms through functions
#' @param tru Actual value of data
#' @param pre Model predicted value
#' @return Return classification model statistics
#' @export
#'
#' @examples
#' xiaofangyaya(c(1,1,1,0,0,0),c(0,0,1,0,1,0),2)
#'
xiaofangyaya<-function(tru,pre,n=2){
  tab <- table(tru,pre)
  if(!all(dim(tab)==c(2,2)))
    stop("MUST be a 2*2 table")
  tn = tab[2,2]
  fp = tab[2,1]
  fn = tab[1,2]
  tp = tab[1,1]
  sensitivity = tp / ( tp + fn )
  specificity = tn / ( tn + fp )
  positive = tp / (tp + fp)
  negative = tn / (tn + fn)
  hitrate = (tp + tn)/(tp + tn + fp + fn)
  result<- paste("Sensitivity = ",round(sensitivity,n),
                 "\nSpecificity = ",round(specificity,n),
                 "\nPositive predictive Value = ",round(positive,n),
                 "\nNegative predictive Value = ",round(negative,n),
                 "\nAccuracy = ",round(hitrate,n),
                 "\n",sep="")
  cat(result)
}
