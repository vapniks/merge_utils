\name{recodeDFas}
\alias{recodeDFas}
\title{Recode variables in dataframe to match codes in another dataframe}
\usage{
  recodeDFas(df1, df2, cols1, cols2 = cols1)
}
\arguments{
  \item{df1}{dataframe to be recoded}

  \item{df2}{dataframe to copy codes from}

  \item{cols1}{numeric/character vector indicating
  variables of first dataframe to be recoded}

  \item{cols2}{numeric/character vector indicating
  variables of second dataframe to copy codes from}
}
\value{
  a dataframe (recoded version of 'df1')
}
\description{
  Recode variables in dataframe to match codes in another
  dataframe
}
\details{
  This is a wrapper around \code{\link{recodeAs}}. Each
  variable of 'df1' listed in 'cols1' will be recoded to
  the corresponding variable of 'df2' listed in 'cols2'. If
  length(cols2) < length(cols1) then the final variable in
  'cols2' will be used for all excess variables in 'cols1'.
  If 'cols2' is not supplied then it will take the same
  value as 'cols1'.
}
\author{
  Ben Veal
}

