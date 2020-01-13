#' Odds Ratio
#'
#' Calculates the odds ratios for an inputted 2 x 2 table
#'
#' @param tab A 2 x 2 contingency table passed through \code{table()}.
#' @param conf.level The confidence level you are testing at (default: 0.95)
#' @return Returns the odds ratio test statistic, theta, the confidence level
#'     of the interval, and the confidence interval itself.
#' @examples
#' # create some 2x2 contingency table 
#' table <- matrix(c(766,110,702,89),  
#'   nrow=2, ncol=2, byrow=TRUE)
#' # pass it through odds at default confidence level of 0.95
#' odds(table)
#' # change confidence level to reflect desired
#' odds(table, conf.level=.90)
#' @export
#' 


odds <- function(tab, conf.level=0.95){
  n11 <- tab[1,1]
  n12 <- tab[1,2]
  n21 <- tab[2,1]
  n22 <- tab[2,2]
  if(n12 | n21 == 0) {warning("Divide by 0")}

  theta <- (n11*n22)/(n12*n21)

  alpha <- 1-conf.level
  z <- abs(qnorm(alpha/2))

  lower <- exp(log(theta)-z*sqrt(1/n11+1/n12+1/n21+1/n22))
  upper <- exp(log(theta)+z*sqrt(1/n11+1/n12+1/n21+1/n22))

  return(cat("Odds Ratio: ",round(theta,5), "\n",conf.level*100,
             "% Confidence Interval:", "(",round(lower,5),",", round(upper,5),")", sep=""))
}
