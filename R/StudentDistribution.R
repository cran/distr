


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

setClass("T", contains = "AbscontDistribution")

## Initialize method
setMethod("initialize", "T",
          function(.Object, df = 1) {
            .Object@img <- new("Reals")
            .Object@param <- new("TParameter", df = df, name = "Parameter of a Student distribution" )
            .Object@r <- function(n){ rt(n, df = df) }
            .Object@d <- function(x, ...){ dt(x, df = df, ...) }
            .Object@p <- function(x, ...){ pt(x, df = df, ...) }
            .Object@q <- function(x, ...){ qt(x, df = df, ...) }
            .Object
          })

## wrapped access methods
setMethod("df", "T", function(x, df1, df2, log = FALSE) df(param(x)))

## wrapped replace methods
setMethod("df<-", "T", function(object, value) new("T", df = value))
