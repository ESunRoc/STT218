#' Relative Risk
#'
#'  This function calculates the relative risk for an inputted 2 x 2 table
#'
#' @param tab A 2 x 2 contingency table passed through \code{table()}.
#' @param conf.level The confidence level you are testing at; defaults to 95\%
#' @return Returns the relative risk test statistic, r, the confidence level
#'     of the interval, and the confidence interval itself.
#' @examples
#' # create some 2x2 contingency table 
#' table <- matrix(c(766,110,702,89), nrow=2, ncol=2, byrow=TRUE)
#' # pass it through relrisk at default confidence level of 0.95
#' relrisk(table)
#' # change confidence level to reflect desired
#' relrisk(table, conf.level=.90)
#' @export
#' 

relrisk = function(tab, conf.level=0.95){
  # check if table is not 2x2, warn that only the first 2x2 will be used
  if(nrow(tab) | ncol(tab) > 2) {
    warning("Only rows/columns 1 and 2 will be used")
    }
  n11 <- tab[1,1]
  n12 <- tab[1,2]
  n21 <- tab[2,1]
  n22 <- tab[2,2]
  n1 <- sum(tab[1,])
  n2 <- sum(tab[2,])
  p1 <- n11/n1
  if(n1 == 0) {warning("Divide by 0")}
  p2 <- n21/n2
  if(n2 == 0) {warning("Divide by 0")}
  r <- p1/p2

  alpha <- 1-conf.level
  z <- abs(qnorm(alpha/2))

  lower <- exp(log(r)-z*sqrt(n12/(n11*n1)+n22/(n21*n2)))
  upper <- exp(log(r)+z*sqrt(n12/(n11*n1)+n22/(n21*n2)))

  return(cat("Relative Risk: ",round(r,5), "\n",conf.level*100,
             "% Confidence Interval:", "(",round(lower,5),",", round(upper,5),")", sep=""))
}
