#' Local Odds Ratio
#'
#' Calculates the local odds ratios for an inputted I x J table
#'
#' @param tab An I x J contingency table passed through \code{table()}.
#' @return \code{local.odds()} returns a matrix of the local odds ratios for the provided table
#' @examples
#' # make some table (for the purposes of this example, 3x3)
#' table <- matrix(c(5134,2829,5661,3946,715,4588), nrow=2, ncol=2, byrow=TRUE)
#' # then pass it through local.odds to examine the association between adjacent cells
#' local.odds(table)
#' @export
#' 


local.odds <- function(tab){
  dim1 <- dim(tab)[1]
  dim2 <- dim(tab)[2]
  results <- matrix(NA, dim1-1, dim2-1)
  for(i in 1:c(dim1-1)){
    for(j in 1:c(dim2-1)){
      results[i,j] <- tab[i,j]*tab[i+1,j+1]/(tab[i,j+1]*tab[i+1,j])
    }
  }
  return(results)
}
