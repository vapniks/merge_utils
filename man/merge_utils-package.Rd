\name{merge_utils-package}
\alias{merge_utils-package}
\alias{merge_utils}
\docType{package}
\title{Functions to aid merging disparate datasets}
\description{This package contains various functions for checking data,
recoding variables, and merging dataframes.}
\details{
\tabular{ll}{Package: \tab mergeutils\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-07-17\cr
License: \tab GNU GPLv3\cr}
This package is useful when you have to merge together several different
data sources. You can make sure the variables are coded consistently
across dataframes using the various recode functions: recodeMatches,
recodeAs, recodeDF, and recodeDFas. Then you can merge them together
using multimerge which allows some flexibility in which rows are
included for each dataframe. Then you can check your results using the
checkVar and checkDF functions. You can also check for duplicate rows
across dataframes using the dupsBetweenDFs function.
There are also some utility functions such as colwise2 (a variation of
the plyr function colwise), unfactor (turn a factor into a
numeric/character vector), matchStrings (find nearest matches from one
set of strings to another).}
\author{Joe Bloggs

Maintainer: Who to complain to <vapniks@yahoo.com>}
\references{
http://www.cookbook-r.com/Manipulating_data/Comparing_data_frames/
}
\keyword{package
  manip
  utilities}
\seealso{
  \code{\link[car:car-package]{recode}}
  \code{\link{merge}}
}
\examples{}
