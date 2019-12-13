#' Relative Risk
#'
#'  This function calculates the relative risk for an inputted 2 x 2 table
#'
#' @param tab A 2 x 2 contingency table passed through \code{table()}.
#' @param conf.level The confidence level you are testing at; defaults to 95\%
#' @return Returns the relative risk test statistic, r, the confidence level
#'     of the interval, and the confidence interval itself.
#' @export
#' @examples

relrisk = function(tab, conf.level=0.95){
  n11 <- tab[1,1]
  n12 <- tab[1,2]
  n21 <- tab[2,1]
  n22 <- tab[2,2]
  n1 <- sum(tab[1,])
  n2 <- sum(tab[2,])

  p1 <- n11/n1
  p2 <- n21/n2
  r <- p1/p2

  alpha <- 1-conf.level
  z <- abs(qnorm(alpha/2))

  lower <- exp(log(r)-z*sqrt(n12/(n11*n1)+n22/(n21*n2)))
  upper <- exp(log(r)+z*sqrt(n12/(n11*n1)+n22/(n21*n2)))

  return(cat("Relative Risk: ",round(r,5), "\n",conf.level*100,
             "% Confidence Interval:", "(",round(lower,5),",", round(upper,5),")", sep=""))
}
