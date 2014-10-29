\name{checkVar}
\alias{checkVar}
\title{Perform sanity checks on a single variable.}
\usage{
  checkVar(var, data, type, min_len, max_len, min, max,
    vals, valstype = "all", min_uniq, max_uniq, max_na,
    pred, silent = FALSE, stoponfail = FALSE)
}
\arguments{
  \item{var}{the variable to check (or it's name as a
  string if the data arg is supplied)}

  \item{data}{an optional dataframe containing the variable
  (otherwise 'var' is taken from the calling environment)}

  \item{type}{(optional) type of the variable (compared
  with typeof(var)), or can be "numeric" to check for
  either "integer" or "double"}

  \item{min_len}{(optional) minimum length of the variable
  (compared with length(var))}

  \item{max_len}{(optional) maximum length of the variable
  (compared with length(var))}

  \item{min}{(optional), minimum allowed value (compared
  with min(var))}

  \item{max}{(optional), maximum allowed value (compared
  with max(var))}

  \item{vals}{(optional), list of all unique non missing
  values (compared with uniqueNotNA(var))}

  \item{valstype}{(optional) used in conjunction with
  'vals'. If "all" (default) then 'vals' should contain all
  the same items as uniqueNotNA(var), if
  "subset"/"superset" then 'vals' should be a
  subset/superset of uniqueNotNA(var)}

  \item{min_uniq}{(optional) minimum number of unique
  values (compare with length(unique(var)))}

  \item{max_uniq}{(optional) maximum number of unique
  values (compare with length(unique(var)))}

  \item{max_na}{(optional) maximum number of missing values
  (compare with sum(is.na(var)))}

  \item{pred}{(optional) A function which takes a variable
  as input and returns TRUE/FALSE depending on whether the
  variable is valid or not.}

  \item{silent}{(optional) if TRUE then don't omit warning
  messages informing of error type (FALSE by default)}

  \item{stoponfail}{(optional) if TRUE then throw an error
  on the first check that fails (FALSE by default)}
}
\value{
  TRUE if all checks passed, FALSE otherwise.
}
\description{
  Perform sanity checks on a single variable.
}
\details{
  This function can be used after performing some data
  munging to check for mistakes.

  You can check the data type, length, max, min, unique, or
  missing values. You can also supply your own function to
  check the variable. For the 'min_uniq', 'max_uniq' and
  'max_na' variables, you can supply either a whole number
  indicating the number of cases, or a number between 0 & 1
  representing a proportion of cases.

  Note: if you need to repeatedly call the function on the
  same dataframe you can curry the data argument using the
  CurryL function in the functional library, e.g:
  checkalldata <- CurryL(checkVar,data=alldata) (it wont
  work with the non-lazy Curry function). To apply the
  function to all variables in a dataframe use the
  \code{\link{apply}} or \code{\link{checkDF}} functions.
}
\examples{
# create a function for checking variables in "ChickWeight" dataframe
checkalldata <- CurryL(checkVar,data=ChickWeight)
checkalldata(weight,type="numeric")
# check each and every variable of dataframe
apply(ChickWeight,2,function(x){checkVar(x,type="numeric")})
}
\author{
  Ben Veal
}
\seealso{
  \code{\link{checkDF}}, \code{\link{CurryL}},
  \code{\link{apply}}
}

