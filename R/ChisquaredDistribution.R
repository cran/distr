
################################
##
## Klasse: ChisqParameter
##
################################

setClass("ChisqParameter", representation(df = "numeric", ncp = "numeric"), contains = "Parameter")

## Access Methods
if(!isGeneric("df")) setGeneric("df", function(x, df1, df2, log = FALSE) standardGeneric("df"))
if(!isGeneric("ncp")) setGeneric("ncp", function(object) standardGeneric("ncp"))
setMethod("df", "ChisqParameter", function(x, df1, df2, log = FALSE) x@df)
setMethod("ncp", "ChisqParameter", function(object) object@ncp)
## Replace Methods
if(!isGeneric("df<-")) setGeneric("df<-", function(object, value) standardGeneric("df<-"))
if(!isGeneric("ncp<-")) setGeneric("ncp<-", function(object, value) standardGeneric("ncp<-"))
setReplaceMethod("df", "ChisqParameter", function(object, value){ object@df <- value; object})
setReplaceMethod("ncp", "ChisqParameter", function(object, value){ object@ncp <- value; object})

validChisqParameter <- function(object){
  if(length(df(object)) != 1)
    stop("df has to be a numeric of length 1")    
  if(df(object) <= 0)
    stop("df has to be positive")
  if(length(ncp(object)) != 1)
    stop("ncp has to be a numeric of length 1")    
  if(ncp(object) < 0)
    stop("ncp has to be not negative")
  else return(TRUE)
}

setValidity("ChisqParameter", validChisqParameter)


################################
##
## Klasse: Chi squared distribution
##
################################

setClass("Chisq", contains = "AbscontDistribution")

## Initialize method
setMethod("initialize", "Chisq",
          function(.Object, df = 1, ncp = 0) {
            .Object@img <- new("Reals")
            .Object@param <- new("ChisqParameter", df = df, ncp = ncp, name = "Parameter of a chi squared distribution" )
            .Object@r <- function(n){ rchisq(n, df = dfSub, ncp = ncpSub) }
            body(.Object@r) <- substitute({ rchisq(n, df = dfSub, ncp = ncpSub) },
                                          list(dfSub = df, ncpSub = ncp))
            .Object@d <- function(x, ...){ dchisq(x, df = dfSub, ncp = ncpSub, ...) }
            body(.Object@d) <- substitute({ dchisq(x, df = dfSub, ncp = ncpSub, ...) },
                                          list(dfSub = df, ncpSub = ncp))
            .Object@p <- function(x, ...){ pchisq(x, df = dfSub, ncp = ncpSub, ...) }
            body(.Object@p) <- substitute({ pchisq(x, df = dfSub, ncp = ncpSub, ...) },
                                          list(dfSub = df, ncpSub = ncp))
            .Object@q <- function(x, ...){ qchisq(x, df = dfSub, ncp = ncpSub, ...) }
            body(.Object@q) <- substitute({ qchisq(x, df = dfSub, ncp = ncpSub, ...) },
                                          list(dfSub = df, ncpSub = ncp))
            .Object
          })

## wrapped access methods
setMethod("df", "Chisq", function(x, df1, df2, log = FALSE) df(param(x)))
setMethod("ncp", "Chisq", function(object) ncp(param(object)))

## wrapped replace methods
setMethod("df<-", "Chisq", function(object, value) new("Chisq", df = value, ncp = ncp(object)))
setMethod("ncp<-", "Chisq", function(object, value) new("Chisq", df = df(object), ncp = value))

setMethod("+", c("Chisq","Chisq"),
          function(e1,e2){
            newdf <- df(e1) + df(e2)
            newncp <- ncp(e1) + ncp(e2)
            return(new("Chisq", df = newdf, ncp = newncp))
          })

