.packageName <- "mergeutils"

##' @title Find complete cases/rows of data.frame with respect to a set of variables.
##' @details The row numbers of 'df' for which there are no missing values in any of the columns indicated by '...' are
##' returned.
##' The arguments passed in '...' may be character strings, character vectors or formula objects. For formula objects
##' all variables on both left and right hand side of the formula are used. For character strings and vectors the variables
##' whose names are given by those strings are used.
##' Variable transformations may also be used in the arguments, however it doesn't seem to work for the 'diff'
##' transformation (though 'lag' works), and I can't see how to easily fix this.
##' @param ... A comma seperated list of arguments indicating which variables to consider. The arguments may be
##' character strings, character vectors or formula objects. 
##' @param df A data.frame containing the data to be checked for complete cases.
##' @return A logical vector indicating which rows of df have no missing values for the variables passed in '...'
##' Note: currently this doesn't work for formulas which specify instruments (i.e. containing a | symbol)
##' @author Ben Veal
##' @export
complete.cases2 <- function(...,df)
{
  vars <- ""
  for(arg in list(...))
    {
      if(class(arg)[1]=="formula")
        vars <- paste(vars,str_c(get.vars(arg),collapse=","),sep=",")
      else if(class(arg)[1]=="character")
        {
          vars <- paste(vars,str_c(arg,collapse=","),sep=",")
        }
      else stop("Function args should be strings or formulas and a data.frame for the df arg.")
    }
  vars <- substring(vars,2,nchar(vars))
  evalstr("evalq(complete.cases(",vars,"),envir=as.data.frame(df))")
}

## TODO - check it works, I think it still needs work
##' @title Merge multiple dataframe by common columns.
##' @details Given a list of dataframes, and a list of vectors each of which contains the same number column names/numbers,
##' merge the dataframes by matching on the corresponding columns. The 'by' argument specifies, for each dataframe, which
##' columns to use for matching - it should be a list of numeric or character vectors.
##' The optional 'all' argument can be used to specify for each dataframe whether to keep all rows, or just those matching
##' some of the other dataframes - it should be a list of NULLs (meaning keep all rows) and/or vectors (indicating which
##' dataframes to match).
##' If either of the lists supplied to the 'by' or 'all' arguments are shorter than the list of dataframes then the final
##' element will be reused to cover missing specifications.
##' To avoid column name clashes, the corresponding dataframe indices will be appended to the names of any columns in the
##' results that would otherwise clash. Alternatively suffixes can be specified in the 'suffixes' argument, which should
##' be a list of strings.
##' @param data A list of dataframes to merge.
##' @param by A list of vectors each of which contains column names/numbers to be used for matching rows of the corresponding
##' dataframe in 'data'. Each of these vectors should be the same length.
##' @param all A list of vectors/NULL values, each indicating which rows of the corresponding dataframe in 'data' to keep.
##' A NULL entry (default) indicates that all rows will be kept. A vector entry should contain indices of dataframes in 'data'.
##' Then the only rows that will be kept are those that match all the dataframes with those indices.
##' @param suffixes A list of suffixes (one for each dataframe) to be appended to clashing column names in order to indicate
##' the original dataframe.
##' @return A single dataframe containing the merged data.
##' @author Ben Veal
##' @export
multimerge <- function(data,by,all=NULL,suffixes=NULL)
{
    len <- length(data)
    ## make sure all values in "by" are set
    if(is.null(by))
        Reduce(intersect)
        by <- list()
    for(i in 1:len)
        if(i > length(by)) by[[i]] <- by[[i-1]]
        else if(is.numeric(by[[i]]))
                 by[[i]] <- names(data[[i]])[by[[i]]]

                
    ## append suffixes to conflicting column names
    cols <- mapply(function(a,b){setdiff(names(a),ifelse(is.numeric(b),names(a)[b],b))},data,by,SIMPLIFY=FALSE)
    for(i in 1:len)
        {
            
            unique <- setdiff(names(data[[i]]),by[[i]])
            dups <- intersect(cols[[i]],unlist(cols[-i]))
            indices <- cols[[i]] %in% dups
            if(length(suffixes) >= i && !is.na(suffixes[[i]]))
                cols[[i]][indices] <- paste(cols[[i]][indices],suffixes[[i]],sep=".")
            else cols[[i]][indices] <- paste(cols[[i]][indices],as.character(i),sep=".")
        }
    ## subset dataframes to appropriate rows according to values in "all" argument
    data2 <- list()
    for(i in 1:len)
        {
            if(length(all) < i || is.null(all[[i]]) || all[[i]] == c(i))
                data2[[i]] <- data[[i]]
            else
                ## first need to find which rows to keep
                keep <- rep(FALSE,dim(data[[i]])[1])
                for(j in all[[i]])
                    {
                        ## loop over 'by' columns, filtering out non-matching rows each time
                        for(k in 1:length(by[[i]]))
                            keep <- keep & (data[[i]][,by[[i]][k]] %in% data[[j]][,by[[j]][k]])
                        data2[[i]] <- data[[i]][keep,]
                    }
        }
    ## finally, merge the data
    accum <- data2[[1]]
    for(i in 2:len)
        accum <- merge(accum,data2[[i]],by.x=by[[i-1]],by.y=by[[i]],suffixes=c())
    return(accum)
}

##' @title Like 'unique' but omits NA's or NaN's, suppresses warnings, and returns result as character vector.
##' @param x A vector.
##' @param warn Whether to issue warnings from 'unique' (default is FALSE).
##' @return The unique values of 'x' with NA & NaN values removed.
##' @author Ben Veal
##' @export 
uniqueNotNA <- function(x,warn=FALSE)
{
    if(warn)
        uvals <- unique(x)
    else
        uvals <- suppressWarnings(unique(x))
    
    as.character(uvals[!is.na(uvals)])
}

##' @title Internal function used by 'contents'
##' @param x 
##' @return A string
##' @author Ben Veal
.contents <- function(x)
{
    uvals <- uniqueNotNA(x)
    if(suppressWarnings(!anyNA(as.numeric(uvals))))
        "numeric"
    else if(setequal(uvals,c("FALSE","TRUE")))
        "logical"
    else
        "character"
}

##' @title Find contents of variables/factors - either "numeric", "logical" or "character"
##' @details If 'x' is a vector or matrix then a single string is returned, if 'x' is a list or
##' dataframe then a character vector is returned with each entry indicating the contents of the
##' corresponding element/column.
##' @param x 
##' @return A single string or a character vector
##' @seealso \code{\link{is.character.contents}}, \code{\link{is.numeric.contents}}, \code{\link{is.logical.contents}}
##' @author Ben Veal
##' @export
contents <- function(x)
{
    if(class(x)=="data.frame")
        apply(x,2,.contents)
    else if(class(x)=="list")
        sapply(x,.contents,simplify=TRUE)
    else if(class(x) %in% c("factor","numeric","logical","integer","double","character"))
        .contents(x)
    else
        "unknown"
}

##' @title Test if contents of vector are character/string values.
##' @details Unlike \code{\link{is.character}} this works with factors.
##' @param x 
##' @return TRUE or FALSE
##' @seealso \code{\link{contents}}, \code{\link{is.numeric.contents}}, \code{\link{is.logical.contents}}
##' @author Ben Veal
##' @export 
is.character.contents <- function(x)
{
    contents(x)=="character"
}

##' @title Test if contents of vector are numeric values.
##' @details Unlike \code{\link{is.character}} this works with factors, and will also return TRUE if the
##' contents are strings containing only numbers.
##' @param x 
##' @return TRUE or FALSE
##' @seealso \code{\link{contents}}, \code{\link{is.character.contents}}, \code{\link{is.logical.contents}}
##' @author Ben Veal
##' @export 
is.numeric.contents <- function(x)
{
    contents(x)=="numeric"
}

##' @title Test if contents of vector are logical (TRUE/FALSE) values.
##' @details Unlike \code{\link{is.character}} this works with factors.
##' @param x 
##' @return TRUE or FALSE
##' @seealso \code{\link{contents}}, \code{\link{is.character.contents}}, \code{\link{is.numeric.contents}},
##' @author Ben Veal
##' @export 
is.logical.contents <- function(x)
{
    contents(x)=="logical"
}

##' @title Internal function used by 'unfactor' 
##' @param x 
##' @return A vector
##' @author Ben Veal
.unfactor <- function(x)
{
    if(!class(x)=="factor")
        x
    else
        {
            c <- contents(x)
            switch(contents(x),
                   "character"=as.character(x),
                   "logical"=as.logical(x),
                   "numeric"=as.numeric(as.character(x)),
                   x)
        }
}

##' @title Convert factors into numeric/character/logical vectors.
##' @details This function converts factor vectors into a numeric, character or logical vectors
##' depending on the contents. If 'x' is a list or dataframe then any constituent factors will
##' be converted.
##' @param x A factor vector, list or dataframe.
##' @return A vector, list or dataframe containing no factors.
##' @author Ben Veal
##' @export 
unfactor <- function(x)
{
    if(class(x)=="data.frame")
        as.data.frame(lapply(x,.unfactor),stringsAsFactors=FALSE)
    else if(class(x)=="list")
        lapply(x,.unfactor)
    else if(class(x)=="factor")
        .unfactor(x)
    else
        x
}

##' @title Recode an ordered variable by reversing the codes.
##' @details Given a variable of integer codes (e.g. measured on a Likert scale), or an ordered factor, this function
##' will recode the variable with the codes reversed, and return the result.
##' @param var The variable containing the codes to be reversed.
##' @return The recoded variable with codes reversed.
##' @seealso \code{\link{recodeAs}}, \code{\link{recodeMatches}}, \code{\link{recodeVar}},
##' \code{\link{colwise2}} for recoding multiple dataframe columns simultaneously (in library(plyr)).
##' @author Ben Veal
##' @export 
recodeReverse <- function(var)
{
  stopifnot(is.ordered(var)||is.numeric(var))
  vals <- sort(unique(var))
  revvals <- sort(unique(var),decreasing=TRUE)
  var2 <- var
  for(i in 1:length(vals))
      var2[var==vals[i]] <- revvals[i]
  return(var2)
}

##' @title Recode values of variable/factor matching patterns
##' @details This function is like 'recodeVar' except that it uses regular expressions to match
##' the source variables (using 'grepl'). For each unique value of 'x' the regexps in 'patterns' are
##' tried in turn until a match is found, then the corresponding element of 'targets' is used to recode
##' the value. If length(patterns) > length(targets) then the final target will be used for all
##' excess patterns. The 'default' and 'keep.na' parameters are the same as for 'recodeVar'.
##' To do string matching instead of regular expression matching use fixed=TRUE
##' @param x The character vector/factor to recode.
##' @param patterns List of regular expressions for matching.
##' @param targets Values to recode corresponding matches in 'patterns' into.
##' @param default Default target value for those values of x that don't match any pattern.
##' When default=NULL, non-matching values of x will be kept in the output.
##' @param keep.na If TRUE then NA's in x will be retained in the output.
##' @param ignore.case If TRUE (default) then matching is not case sensitive. 
##' @param ignore.punc If TRUE (default) then punctuation will be ignored when matching.
##' @param ... optional arguments to grepl (e.g. use fixed=TRUE for string instead of regexp matching)
##' @return A vector
##' @seealso \code{\link{recode}} for recoding numbers (in library(car)), \code{\link{recodeVar}}, \code{\link{recodeAs}},
##' \code{\link{colwise2}} for recoding multiple dataframe columns simultaneously (in library(plyr)).
##' @author Ben Veal
##' @export
recodeMatches <- function(x,patterns,targets,default=NULL,keep.na=TRUE,ignore.case=TRUE,ignore.punc=TRUE,...)
{
    stopifnot(class(x) %in% c("character","factor"))
    src <- character()
    tgt <- character()
    dots <- list(...)
    if("fixed" %in% names(dots) && dots$fixed && ignore.punc)
        ignore.punc <- FALSE
    for(val in uniqueNotNA(x))
        {
            val2 <- ifelse(ignore.punc,gsub("[[:punct:]]","",val),val)
            matches <- sapply(patterns,function(pat){grepl(pat,val2,ignore.case=ignore.case,...)})
            if(any(matches))
                {
                    src <- append(src,val)
                    indx <- min(which(matches))
                    tgt <- append(tgt,ifelse(indx <= length(targets),targets[indx],last(targets)))
                }
        }
    if(length(src)>0)
        recodeVar(x,src,tgt,default=default,keep.na=keep.na)
    else
        x
}

##' @title Recode unique values of one variable to match unique values of another variable.
##' @details This function is a wrapper around \code{\link{recodeVar}}. It can be used when you need to
##' recode a character variable/factor so that the values correspond with those of another variable
##' (e.g. when merging datasets with slightly different value labels).
##' The \code{\link{matchstrings}} function is used to guess the best mapping between the values of 'A' and the values of 'B'.
##' Use this function with care.
##' @param A The character vector/factor to recode.
##' @param B The character vector/factor whose unique values are to be copied.
##' @return The recoded version of 'A'
##' @seealso \code{\link{recode}} for recoding numbers (in library(car)), \code{\link{recodeVar}}, \code{\link{recodeMatch}},
##' \code{\link{colwise2}} for recoding multiple dataframe columns simultaneously (in library(plyr)).
##' @author Ben Veal
##' @export
recodeAs <- function(A,B)
{
    stopifnot(class(A)=="factor"||class(A)=="character")
    stopifnot(class(B)=="factor"||class(B)=="character")
    Avals <- uniqueNotNA(A)
    Bvals <- uniqueNotNA(B)
    Bvals <- Bvals[matchStrings(Avals,Bvals)]
    for(i in 1:length(Avals))
        print(paste0("Recoding '",as.character(Avals[i]),"' to '",as.character(Bvals[i]),"'"))
    recodeVar(A,Avals,Bvals)
}

##' @title Recode collection of variables so that they all the same unique values.
##' @details This is a wrapper around \code{\link{recodeMatches}} & \code{\link{recode}} (in library(car))
##' for recoding all strings and numbers variables in a dataframe.
##' @param df A dataframe to recode
##' @param sPatterns list of regexps matching strings to be recoded
##' @param sTargets list of targets corresponding to patterns in 'sPatterns'
##' @param nRecodes specification for recoding numbers (see \code{\link{recode}})
##' @return a dataframe (recoded)
##' @author Ben Veal
##' @export
recodeDF <- function(df,sPatterns,sTargets=NA,nRecodes)
{
    fn1 <- colwise2(recodeMatches,is.character.contents,patterns=sPatterns,targets=sTargets)
    fn2 <- colwise2(recode,is.numeric.contents,recodes=nRecodes)
    fn2(fn1(df))
}

##' @title Recode variables in dataframe to match codes in another dataframe
##' @details This is a wrapper around \code{\link{recodeAs}}.
##' Each variable of 'df1' listed in 'cols1' will be recoded to the corresponding variable of 'df2'
##' listed in 'cols2'. If length(cols2) < length(cols1) then the final variable in 'cols2' will be
##' used for all excess variables in 'cols1'. If 'cols2' is not supplied then it will take the same
##' value as 'cols1'.
##' @param df1 dataframe to be recoded
##' @param df2 dataframe to copy codes from
##' @param cols1 numeric/character vector indicating variables of first dataframe to be recoded 
##' @param cols2 numeric/character vector indicating variables of second dataframe to copy codes from
##' @return a dataframe (recoded version of 'df1')
##' @author Ben Veal
##' @export 
recodeDFas <- function(df1,df2,cols1,cols2=cols1)
{
    x <- df1
    for(i in 1:length(cols1))
        {
            col1 <- cols1[i]
            col2 <- ifelse(i<=length(cols2),cols2[i],last(cols2))
            x[,col1] <- recodeAs(df1[,col1],df2[,col2])
        }
    x
}

##' @title Like \code{\link{colwise}}, but the returned function will return unaffected columns alongside affected ones
##' if possible.
##' @details This function works like \code{\link{colwise}} except that the function it returns also returns the columns
##' not specified by the '.cols' argument (as long as they are compatible). If the unaffected columns have a different
##' number of rows than the affected ones, then only the affected rows will be returned.
##' The columns will be returned in the same order as the original dataframe argument to the function.
##' @param .fun a function to apply to each column
##' @param .cols either a function that tests columns for inclusion, or a quoted object giving which columns to process
##' @param ... other arguments passed on to ‘.fun’
##' @return A function which accepts a dataframe as argument and applies '.fun' to each of the columns specified in '.cols'.
##' Further arguments to the function will be passed on to '.fun'.
##' @author Ben Veal
##' @export
colwise2 <- function(.fun,.cols=true,...)
{
    namesfn <- colwise(names,.cols)
    applyfn <- colwise(.fun,.cols,...)
    function(df,...)
        {
            colnames <- names(namesfn(df))
            x <- applyfn(df,...)
            if(dim(x)[1]==dim(df)[1])
                {
                    df[,colnames] <- x
                    df
                }
            else
                {
                    print("New columns incompatible with old ones")
                    x
                }
        }
}

##' @title Match strings in A with strings in B
##' @details This function tries to match the strings in 'A' to the closest ones in 'B'.
##' By default it tries to ensure that every element of 'B' is matched by at least one element of 'A'.
##' So if 'A' and 'B' have the same length it calculates an exact pairwise matching between 'A' & 'B'.
##' If length(A) < length(B) then some elements of 'B' will not be matched.
##' If length(A) > length(B) then some elements of 'B' will be matched more than once.
##' Alternatively, if the 'onto' parameter is set to FALSE then each element of 'A' will be matched with the
##' most similar element of 'B' regardless of whether or not that element is matched by another element of 'A'.
##' @param A A character vector
##' @param B A character vector
##' @param onto If TRUE (default) then ensure all strings in B are matched by at least one string in A if possible.
##' @return A vector whose i'th entry indicates the element of B that matches the i'th element of A
##' @author Ben Veal
##' @export
matchStrings <- function(A,B,onto=TRUE)
{
    distMat <- stringdistmatrix(A,B,method="jw")
    matchByDistance(distMat)
}

##' @title Match elements of one set with closest elements of another set, according to their mutual distances.
##' @details This function tries to match elements of one set with the closest elements of another set according
##' to the distances between these elements as supplied by 'distMat'.
##' By default it tries to ensure that every element of the second set (corresponding to the columns of 'distMat')
##' is matched with at least one element of the first set (corresponding to the rows of 'distMat').
##' So if 'distMat' is a square matrix it calculates an exact pairwise matching between row elements & column elements.
##' If No. rows of 'distMat' < No. of columns of 'distMat' then some columns will not be matched. 
##' If No. rows of 'distMat' > No. of columns of 'distMat' then some columns will be matched more than once.
##' Alternatively, if the 'onto' parameter is set to FALSE then each row will be matched with the closest column
##' regardless of whether or not that column is matched by another row.
##' @param distMat A distance matrix - the (i,j) element indicates the distance between the i'th element of the
##' first set of things and the j'th element of the second set of things.
##' @param onto If TRUE (default) then ensure all columns are matched by at least one row if possible.
##' @return A vector whose i'th entry indicates the element in the 2nd set matching the i'th element in the 1st set.
##' @author Ben Veal
##' @export 
matchByDistance <- function(distMat,onto=TRUE)
{
    if(!onto) return(apply(distMat,1,which.min))
    ## For each row/column rank the distances and return matrix of (row rank + column rank).
    rankMat <- t(apply(distMat,1,rank))+apply(distMat,2,rank)
    ## Match each row with the column which has smallest (row rank + column rank).
    row2col <- apply(rankMat,1,which.min)
    ## Find which columns have been matched & which haven't.
    usedcols <- unique(row2col)
    unusedcols <- setdiff(1:dim(distMat)[2],usedcols)
    ## Match any columns which haven't been matched if possible
    if(length(unusedcols) > 0)
        {
            ## Find duplicate matches
            dups <- lapply(usedcols,function(x){which(row2col==x)})
            ## For any duplicate matches, keep the row which is closest to the column,
            ## and collect the remaining rows.
            remaining <- numeric()
            for(i in 1:length(dups))
                if(length(dups[[i]]) > 1)
                    {
                        closest <- which.min(distMat[dups[[i]],usedcols[i]])
                        remaining <- union(dups[[i]][-closest],remaining)
                    }
            ## Match the remaining rows with the unmatched columns
            if(length(remaining) > 0)
                {
                    if(length(remaining)==1)
                        row2col[remaining] <- unusedcols[which.min(distMat[remaining,unusedcols])]
                    else if(length(unusedcols)==1)
                        row2col[remaining] <- unusedcols
                    else
                        row2col[remaining] <- unusedcols[matchByDistance(distMat[remaining,unusedcols])]
                }
        }
    return(row2col)
}

##' @title Perform sanity checks on a single variable.
##' @details This function can be used after performing some data munging to check for mistakes.
##' 
##' You can check the data type, length, max, min, unique, or missing values.
##' You can also supply your own function to check the variable.
##' For the 'min_uniq', 'max_uniq' and 'max_na' variables, you can supply either a whole number indicating
##' the number of cases, or a number between 0 & 1 representing a proportion of cases.
##' 
##' Note: if you need to repeatedly call the function on the same dataframe you can curry the data argument using
##' the CurryL function in the functional library, e.g: checkalldata <- CurryL(checkVar,data=alldata)
##' (it wont work with the non-lazy Curry function). To apply the function to all variables in a dataframe use the
##' \code{\link{apply}} or \code{\link{checkDF}} functions.
##' @param var the variable to check (or it's name as a string if the data arg is supplied)
##' @param data an optional dataframe containing the variable (otherwise 'var' is taken from the calling environment)
##' @param type (optional) type of the variable (compared with typeof(var)), or can be "numeric" to check for either
##' "integer" or "double"
##' @param min_len (optional) minimum length of the variable (compared with length(var))
##' @param max_len (optional) maximum length of the variable (compared with length(var))
##' @param min (optional), minimum allowed value (compared with min(var))
##' @param max (optional), maximum allowed value (compared with max(var))
##' @param vals (optional), list of all unique non missing values (compared with uniqueNotNA(var))
##' @param valstype (optional) used in conjunction with 'vals'. If "all" (default) then 'vals' should contain all the same
##' items as uniqueNotNA(var), if "subset"/"superset" then 'vals' should be a subset/superset of uniqueNotNA(var)
##' @param min_uniq (optional) minimum number of unique values (compare with length(unique(var)))
##' @param max_uniq (optional) maximum number of unique values (compare with length(unique(var)))
##' @param max_na (optional) maximum number of missing values (compare with sum(is.na(var)))
##' @param pred (optional) A function which takes a variable as input and returns TRUE/FALSE depending on whether the
##' variable is valid or not.
##' @param silent (optional) if TRUE then don't omit warning messages informing of error type (FALSE by default)
##' @param stoponfail (optional) if TRUE then throw an error on the first check that fails (FALSE by default)
##' @return TRUE if all checks passed, FALSE otherwise.
##' @seealso \code{\link{checkDF}}, \code{\link{CurryL}}, \code{\link{apply}}
##' @examples # create a function for checking variables in "ChickWeight" dataframe
##' checkalldata <- CurryL(checkVar,data=ChickWeight)
##' checkalldata(weight,type="numeric")
##' # check each and every variable of dataframe
##' apply(ChickWeight,2,function(x){checkVar(x,type="numeric")})
##' @author Ben Veal
##' @export
checkVar <- function(var,data,type,min_len,max_len,min,max,vals,valstype="all",
                     min_uniq,max_uniq,max_na,pred,silent=FALSE,stoponfail=FALSE)
{
    subvar <- substitute(var)
    if(is.symbol(subvar))
        varname <- deparse(subvar)
    else if(is.character(subvar) & length(subvar)==1)
        varname <- var
    else varname <- "unknown"
    if(!missing(data)) var <- data[[varname]]
    actualtype <- typeof(var)
    isnumeric <- actualtype %in% c("double","integer","numeric")
    len <- length(var)
    ok <- TRUE
    # useful macros to save some typing
    report <- defmacro(str,expr={msg <- paste(str,"for",varname);
                                 if(stoponfail) stop(msg);
                                 if(!silent) print(msg);
                                 ok <- FALSE})
    mintest <- defmacro(val,tot,min,str,expr={if(val < min | (min <= 1 & val/tot < min)) report(str)})
    maxtest <- defmacro(val,tot,max,str,expr={if(val/tot > max | (max >= 1 & val > max)) report(str)})
    # perform the checks
    if(!missing(type))
        {
            if(!((type=="numeric" & isnumeric)|(type==actualtype)))
                report(paste0("Expected '",type,"' type but got '",actualtype,"' type"))
        }
    if(!missing(min_len))
        mintest(len,1,min_len,paste("Length is <",min_len))
    if(!missing(max_len))
        maxtest(len,1,max_len,paste("Length is >",max_len))
    if(!isnumeric & (!missing(min) | !missing(max)))
        report(paste0("Expected numeric type, but got '",actualtype,"' type"))
    if(isnumeric & !missing(min))
        mintest(min(var,na.rm=TRUE),1,min,paste("Found values <",as.character(min)))
    if(isnumeric & !missing(max))
        maxtest(max(var,na.rm=TRUE),1,max,paste("Found values >",as.character(max)))
    if(!missing(vals)|!missing(min_uniq)|!missing(max_uniq))
        {
            uvals <- uniqueNotNA(var)
            ulen <- length(uvals)
            if(!missing(vals))
                {
                    vals <- uniqueNotNA(vals)                    
                    if((valstype=="all" & !setequal(vals,uvals))
                       |(valstype=="subset" & !(all(vals %in% uvals)))
                       |(valstype=="superset" & !(all(uvals %in% vals))))
                        report("Invalid values")
                }
            if(!missing(min_uniq))
                mintest(ulen,len,min_uniq,"Not enough unique values")
            if(!missing(max_uniq))
                maxtest(ulen,len,max_uniq,"Too many unique values")
        }
    if(!missing(max_na))
        maxtest(sum(is.na(var)),len,max_na,"Too many missing values")
    if(!missing(pred))
        {
            if(!pred(var))
                report(paste(deparse(substitute(pred)),"returns false"))
        }
    if(ok & !silent) print(paste("All checks passed for",varname,"variable"))
    return(ok)
}

##' @title Perform sanity checks on a dataframe. 
##' @details This function can be used after performing some data munging to check for mistakes.
##' 
##' You can restrict the checks to a subset of the dataframe by supplying a logical expression in the 'subset'
##' argument. This expression will be evaluated in the context of the supplied dataframe (the 'data' argument),
##' so you don't need to qualify the variable names. If all arguments apart from 'data', 'subset', 'silent' and 'stoponfail'
##' are unset/NULL then the function will check if all rows satisfy the subset logical expression (unless this is unset).
##' The other arguments can used for checking the number of complete cases (i.e. rows with no missing values), unique cases,
##' missing values, and variable specific checks (see below).
##' 
##' For arguments with names beginning with 'min_' or 'max_' you can supply either a whole number indicating
##' an amount of rows/columns, or a number between 0 & 1 indicating a proportion of rows/columns.
##' For 'min_rows' & 'max_rows' proportions are interpreted as proportions of the whole data (before subsetting),
##' whereas for other arguments proportions are interpreted as proportions of the subsetted data.
##'
##' To perform variable specific checks use the 'vars' argument to specify which variables to check. 'vars' can be
##' either a numeric vector of column numbers, or a character vector of regexps matching column names. The matching
##' columns will be individually checked by the \code{\link{checkVar}} function. To specify which checks to perform
##' supply a list of arguments for \code{\link{checkVar}} in the 'checks' argument. You do not need to include the data
##' or var arguments in this list. For example to check that all variables with names matching "country" or "name" have
##' type "character" and between 10 & 300 unique values you could do:
##' 
##' checkDF(data,vars=c("country","name"),checks=list(type="character",min_uniq=10,max_uniq=300))
##' 
##' To ensure that a regexp matches only a single variable put a ^ at the front and $ at the end (e.g. "^country$").
##' Note: the values of the 'silent' and 'stoponfail' args will be passed on the \code{\link{checkVar}} by default
##' but you can override these values by passing new values in the 'checks' arg.
##' 
##' By default a warning message will be issued when a check fails. This can be prevented by setting 'silent' to TRUE.
##' If the 'stoponfail' argument is set to TRUE then an error will be thrown on the first check that fails,
##' otherwise the return value of the function indicates whether all checks passed (TRUE) or not (FALSE).
##' @param data dataframe to be checked
##' @param subset (optional) logical expression indicating subset of 'data' to check (see \code{\link{subset}})
##' @param min_rows (optional) minimum number of rows (compare with dim(data[subset,])[1]). 
##' @param max_rows (optional) maximum number of rows (compare with dim(data[subset,])[1])
##' @param min_cc (optional) minimum number of complete cases (compare with sum(complete.cases(data[subset,])))
##' @param max_cc (optional) maximum number of complete cases (compare with sum(complete.cases(data[subset,])))
##' @param min_uniq (optional) minimum number of unique cases (compare with dim(unique(data[subset,]))[1]). Default value is 1.
##' @param max_uniq (optional) maximum number of unique cases (compare with dim(unique(data[subset,]))[1])
##' @param max_na_row (optional) maximum number of missing values for each row
##' @param max_na_all (optional) maximum number of missing values overall
##' @param silent (optional) if TRUE then don't omit warning messages informing of error type (FALSE by default)
##' @param stoponfail (optional) if TRUE then throw an error on the first check that fails (FALSE by default)
##' @param vars (optional) either a numeric or character vector, or a regexp matching names of variables to check
##' @param checks (optional) a list of a arguments to be passed to \code{\link{checkVar}}
##' @return TRUE if all checks passed, FALSE otherwise
##' @examples checkDF(ChickWeight,weight>time)
##' checkDF(ChickWeight,min_uniq=10)
##' @seealso \code{\link{checkVar}}
##' @author Ben Veal
##' @export 
checkDF <- function(data,subset,min_rows,max_rows,min_cc,max_cc,min_uniq,max_uniq,
                    max_na_row,max_na_all,silent=FALSE,stoponfail=FALSE,vars=NULL,checks=NULL)
{
    nrows1 <- dim(data)[1]
    framename <- deparse(substitute(data))
    subsetstr <- deparse(substitute(subset))
    if(subsetstr!="")
        {
            data <- data[with(data,eval(parse(text=subsetstr))),]
            subsetmsg <- paste0("rows satisfying '",subsetstr,"'")
        }
    else subsetmsg <- "rows"
    nrows2 <- dim(data)[1]
    ncols2 <- dim(data)[2]
    ok <- TRUE
    # useful macros to save some typing
    report <- defmacro(str,expr={msg <- paste(str,"for",framename,"dataframe");
                                 if(stoponfail) stop(msg);
                                 if(!silent) print(msg);
                                 ok <- FALSE})
    mintest <- defmacro(val,tot,min,str,expr={if(val < min | (min <= 1 & val/tot < min)) report(paste("Not enough",str))})
    maxtest <- defmacro(val,tot,max,str,expr={if(val/tot > max | (max >= 1 & val > max)) report(paste("Too many",str))})
    # do dataframe wide checks
    if(!missing(min_rows))
        mintest(nrows2,nrows1,min_rows,subsetmsg)
    # if only the 'subset' & 'data' args are supplied then just check that all rows satisfy the 'subset' expression
    else if(subsetstr!="" & missing(max_rows) & missing(min_cc) & missing(max_cc) & missing(min_uniq) & missing(max_uniq)
             & missing(max_na_row) & missing(max_na_all) & length(vars)==0)
        mintest(nrows2,nrows1,nrows1,subsetmsg)
    if(!missing(max_rows))
        maxtest(nrows2,nrows1,max_rows,subsetmsg)
    if(!missing(min_cc))
        mintest(sum(complete.cases(data)),nrows2,min_cc,"complete cases")
    if(!missing(max_cc))
        maxtest(sum(complete.cases(data)),nrows2,max_cc,"complete cases")
    if(!missing(min_uniq))
        mintest(dim(unique(data))[1],nrows2,min_uniq,"unique cases")
    if(!missing(max_uniq))
        maxtest(dim(unique(data))[1],nrows2,max_uniq,"unique cases")
    if(!missing(max_na_row))
        {
            whichrows <- which(!apply(data,2,function(x){checkVar(x,max_na=max_na_row)}))
            if(length(whichrows) > 0)
                {
                    report("Too many missing values in rows")
                    whichrows[1:min(length(whichrows),10)]
                }
        }
    if(!missing(max_na_all))
        maxtest(sum(is.na(data)),nrows2*ncols2,max_na_all,"missing values")
    # do variable specific checks
    varnames <- names(data)
    if(length(vars) > 0 & length(checks) > 0) {
        if(!("silent" %in% names(checks)))
            checks <- c(checks,silent=silent)
        if(!("stoponfail" %in% names(checks)))
            checks <- c(checks,stoponfail=stoponfail)
        for(i in 1:length(vars)) {
            ## first get the appropriate column numbers 
            if(class(vars[i])=="character") cols <- grep(vars[i],varnames)
            else cols <- vars[i]
            ## now check each column
            for(j in cols)
                if(with(data,!do.call(checkVar,args=c(var=as.symbol(varnames[j]),checks))))
                    ok <- FALSE 
        }
    }
    # finished all checks
    if(ok & !silent) print(paste("All checks passed for",framename,"dataframe"))
    return(ok)
}


