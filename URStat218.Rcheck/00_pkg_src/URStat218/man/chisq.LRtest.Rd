% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ChisqLRtestPackage.R
\name{chisq.LRtest}
\alias{chisq.LRtest}
\title{Chi-Squared Likliehood Ratio Test}
\usage{
chisq.LRtest(tab)
}
\arguments{
\item{tab}{An I x J contingency table passed through \code{table()}.}
}
\value{
Returns the likelihood ratio test statistic, G^2, the p-value of the test, and the degrees of freedom,
    (I-1)(J-1).
}
\description{
This function calculates the likeliehood ratios for the independence test of the same name.
}
