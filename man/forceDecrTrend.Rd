% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logiBin.R
\name{forceDecrTrend}
\alias{forceDecrTrend}
\title{Force a numerical variable to follow a monotonically decreasing trend}
\usage{
forceDecrTrend(binObj, xVars)
}
\arguments{
\item{binObj}{- An object returned by getBins or any other function (except createBins) in this package}

\item{xVars}{- A vector of the name of variables}
}
\value{
Returns a list containing 3 objects. Similar to the getBins function
}
\description{
This function forces a variable to follow a monotonically decreasing trend by grouping bins. In case such a trend can not be forced a message is printed to the console
}
\examples{
b1 <- getBins(loanData, "bad_flag", c('age', 'score'), minCr=0.6, minProp = 0.01)
b1 <- forceDecrTrend(b1, c('score','age'))

}
