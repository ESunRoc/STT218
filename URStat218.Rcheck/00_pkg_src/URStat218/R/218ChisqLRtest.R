#' Chi-Squared Likliehood Ratio Test
#'
#'  This function calculates the likeliehood ratios for the independence test of the same name.
#'
#' @param tab An I x J contingency table passed through \code{table()}.
#' @return Returns the likelihood ratio test statistic, G^2, the p-value of the test, and the degrees of freedom,
#'     (I-1)(J-1).
#' @export
#' @examples

chisq.LRtest <- function(tab){
  exp <- matrix(margin.table(tab,1))%*%t(matrix(margin.table(tab,2)))/margin.table(tab)
  G2 <- 2*sum(tab*log(tab/exp))

  df1 <- (nrow(tab)-1)*(ncol(tab)-1)
  pv <- pchisq(G2, df=df1, lower.tail=F)
  return(cat("\nLog-Likelihood Chi-squared test\n\n","G-squared = ",round(G2,5),
             ", ","df = ", df1,", ", "p-value = ",pv,"\n\n",sep=""))
}

