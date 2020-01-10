#' Linear Trend Test
#'
#' This function performs a Mantel Haenszel Chi-Square Test for Two Way Tables.
#'
#' @param x A contingency table of structure matrix,
#'     or factor (the latter being passed through as \code{table()})
#' @param row.scores Numerical value assigned to rows for calculation purposes; Defaults to Null
#' @param col.scores Numerical value assigned to columns for calculation purposes; Defaults to Null
#' @return The M^2 test statistic, the parameters for the test (r and df), the type of
#'     test performed, and the resultant p-value.
#' @examples 
#' # create some table (for the purposes of this example, a 4x4)
#' table <- matrix(c(1.3.10,6,2,3,10,7,1,6,14,12,0,1,9,11), nrow=4, ncol=4, byrow=TRUE)
#' # pass the table directly through linear.trend if the table has already been given scores
#' linear.trend(table)
#' # change the default arguments in linear.trend to apply scores as needed
#' linear.trend(table, row.scores=c(10000,20000,30000,40000), col.scores = c(-2,-1,1,2))
#' @export
#' 

linear.trend <- function (x, row.scores = NULL, col.scores = NULL) {
  if(is.null(row.scores)){
    row.scores <- seq(1, nrow(x))
  }else{
    row.scores <- row.scores
  }
  if(is.null(col.scores)){
    col.scores <- seq(1, ncol(x))
  }else{
    col.scores <- col.scores
  }
  n <- sum(x)
  p1 <- sum(t(t(x) * col.scores) * row.scores)
  p2 <- sum(row.scores * rowSums(x))
  p3 <- sum(col.scores * colSums(x))
  p4 <- sum(row.scores ^2 * rowSums(x))
  p5 <- sum(row.scores * rowSums(x))^2
  p6 <- sum(col.scores ^2 * colSums(x))
  p7 <- sum(col.scores * colSums(x))^2
  num <- p1 - (p2 * p3/n)
  den <- sqrt((p4 - p5/n) * (p6 - p7/n))
  r <- num/den
  M2 <- (n - 1) * r^2
  pvalue <- 1 - stats::pchisq(M2, 1)
  df <- 1
  names(M2) <- "M^2"
  names(df) <- "df"
  names(r) <-"r"
  METH <- "Mantel Haenszel Chi-Square Test for Two Way Tables"
  structure(list(statistic = M2, parameters = c(r,df), p.value = pvalue,
                 method = METH, correlation = r), class = "htest")
}

