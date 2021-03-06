% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/218LocalOddsRatio.R
\name{local.odds}
\alias{local.odds}
\title{Local Odds Ratio}
\usage{
local.odds(tab)
}
\arguments{
\item{tab}{An I x J contingency table passed through \code{table()}.}
}
\value{
\code{local.odds()} returns a matrix of the local odds ratios for the provided table
}
\description{
Calculates the local odds ratios for an inputted I x J table
}
\examples{
# make some table (for the purposes of this example, 3x3)
table <- matrix(c(5134,2829,5661,3946,715,4588), nrow=2, ncol=2, byrow=TRUE)
# then pass it through local.odds to examine the association between adjacent cells
local.odds(table)
}
