


################################
##
## Class: TParameter
##
################################

setClass("TParameter", representation(df = "numeric"), contains = "Parameter")

## Access Methods
if(!isGeneric("df")) setGeneric("df", function(x, df1, df2, log = FALSE) standardGeneric("df"))
setMethod("df", "TParameter", function(x, df1, df2, log = FALSE) x@df)
## Replace Methods
if(!isGeneric("df<-")) setGeneric("df<-", function(object, value) standardGeneric("df<-"))
setReplaceMethod("df", "TParameter", function(object, value){ object@df <- value; object})

validTParameter <- function(object){
  if(length(df(object)) != 1)
    stop("df has to be a numeric of length 1")    
  if(df(object) <= 0)
    stop("df has to be positive")
  else return(TRUE)
}

setValidity("TParameter", validTParameter)

################################
##
## Class: Student distribution
##
################################

setClass("Td", contains = "AbscontDistribution")

## Initialize method
setMethod("initialize", "Td",
          function(.Object, df = 1) {
            .Object@img <- new("Reals")
            .Object@param <- new("TParameter", df = df, name = "Parameter of a Student distribution" )
            .Object@r <- function(n){ rt(n, df = dfSub) }
            body(.Object@r) <- substitute({ rt(n, df = dfSub) }, 
                                          list(dfSub = df))
            .Object@d <- function(x, ...){ dt(x, df = dfSub, ...) }
            body(.Object@d) <- substitute({ dt(x, df = dfSub, ...) },
                                          list(dfSub = df))
            .Object@p <- function(x, ...){ pt(x, df = dfSub, ...) }
            body(.Object@p) <- substitute({ pt(x, df = dfSub, ...) },
                                          list(dfSub = df))
            .Object@q <- function(x, ...){ qt(x, df = dfSub, ...) }
            body(.Object@q) <- substitute({ qt(x, df = dfSub, ...) },
                                          list(dfSub = df))
            .Object
          })

## wrapped access methods
setMethod("df", "Td", function(x, df1, df2, log = FALSE) df(param(x)))

## wrapped replace methods
setMethod("df<-", "Td", function(object, value) new("Td", df = value))
