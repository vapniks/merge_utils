\name{checkDF}
\alias{checkDF}
\title{Perform sanity checks on a dataframe.}
\usage{
  checkDF(data, subset, min_rows, max_rows, min_cc, max_cc,
    min_uniq, max_uniq, max_na_row, max_na_all,
    silent = FALSE, stoponfail = FALSE, ...)
}
\arguments{
  \item{data}{dataframe to be checked}

  \item{subset}{(optional) logical expression indicating
  subset of 'data' to check (see \code{\link{subset}})}

  \item{min_rows}{(optional) minimum number of rows
  (compare with dim(data[subset,])[1]).}

  \item{max_rows}{(optional) maximum number of rows
  (compare with dim(data[subset,])[1])}

  \item{min_cc}{(optional) minimum number of complete cases
  (compare with sum(complete.cases(data[subset,])))}

  \item{max_cc}{(optional) maximum number of complete cases
  (compare with sum(complete.cases(data[subset,])))}

  \item{min_uniq}{(optional) minimum number of unique cases
  (compare with dim(unique(data[subset,]))[1]). Default
  value is 1.}

  \item{max_uniq}{(optional) maximum number of unique cases
  (compare with dim(unique(data[subset,]))[1])}

  \item{max_na_row}{(optional) maximum number of missing
  values for each row}

  \item{max_na_all}{(optional) maximum number of missing
  values overall}

  \item{silent}{(optional) if TRUE then don't omit warning
  messages informing of error type (FALSE by default)}

  \item{stoponfail}{(optional) if TRUE then throw an error
  on the first check that fails (FALSE by default)}

  \item{...}{(optional) additional variable specific
  arguments (see below)}
}
\value{
  TRUE if all checks passed, FALSE otherwise
}
\description{
  Perform sanity checks on a dataframe.
}
\details{
  This function can be used after performing some data
  munging to check for mistakes.

  You can restrict the checks to a subset of the dataframe
  by supplying a logical expression in the 'subset'
  argument. This expression will be evaluated in the
  context of the supplied dataframe (the 'data' argument),
  so you don't need to qualify the variable names. If all
  arguments apart from 'data', 'subset', 'silent' and
  'stoponfail' are unset/NULL then the function will check
  if all rows satisfy the subset logical expression (unless
  this is unset). The other arguments can used for checking
  the number of complete cases (i.e. rows with no missing
  values), unique cases, missing values, and variable
  specific checks (see below).

  For arguments with names beginning with 'min_' or 'max_'
  you can supply either a whole number indicating an amount
  of rows/columns, or a number between 0 & 1 indicating a
  proportion of rows/columns. For 'min_rows' & 'max_rows'
  proportions are interpreted as proportions of the whole
  data (before subsetting), whereas for other arguments
  proportions are interpreted as proportions of the
  subsetted data.

  To perform variable specific checks you can supply named
  argument where the names are regexp strings matching the
  names of the corresponding variables to check, and the
  values are argument lists to pass on to
  \code{\link{checkVar}} (which see). For example to check
  that each variable with "country" in its name is of type
  "character" and has between 10 and 300 unique values, use
  this argument:
  "country"=list(type="character",min_uniq=10,max_uniq=300)
  To ensure that a regexp matches only a single variable
  put a ^ at the front and $ at the end (e.g. "^country$").
  You can supply as many of these variable specific
  arguments as you want. Note that you do not need to
  supply the 'var' or 'data' arguments in the argument list
  for \code{\link{checkVar}}.

  By default a warning message will be issued when a check
  fails. This can be prevented by setting 'silent' to TRUE.
  If the 'stoponfail' argument is set to TRUE then an error
  will be thrown on the first check that fails, otherwise
  the return value of the function indicates whether all
  checks passed (TRUE) or not (FALSE).
}
\examples{
checkDF(ChickWeight,weight>Time)
checkDF(ChickWeight,min_uniq=10)
}
\author{
  Ben Veal
}
\seealso{
  \code{\link{checkVar}}
}

