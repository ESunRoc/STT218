#' Local Odds Ratio
#'
#' Calculates the local odds ratios for an inputted I x J table
#'
#' @param tab An I x J contingency table passed through \code{table()}.
#' @return \code{local.odds()} returns a matrix of the local odds ratios for the provided table
#' @export
#' @examples


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
