


################################
##
## Class: TParameter
##
################################

setClass("TParameter", representation(df = "numeric", ncp = "numeric"), contains = "Parameter")

## Access Methods
if(!isGeneric("df")) setGeneric("df", function(x, ...) standardGeneric("df"))
if(!isGeneric("ncp")) setGeneric("ncp", function(object) standardGeneric("ncp"))
setMethod("df", "TParameter", function(x, ...) x@df)
setMethod("ncp", "TParameter", function(object) object@ncp)
## Replace Methods
if(!isGeneric("df<-")) setGeneric("df<-", function(object, value) standardGeneric("df<-"))
if(!isGeneric("ncp<-")) setGeneric("ncp<-", function(object, value) standardGeneric("ncp<-"))
setReplaceMethod("df", "TParameter", function(object, value){ object@df <- value; object})
setReplaceMethod("ncp", "TParameter", function(object, value){ object@ncp <- value; object})

validTParameter <- function(object){
  if(length(df(object)) != 1)
    stop("df has to be a numeric of length 1")    
  if(df(object) <= 0)
    stop("df has to be positive")
  if(length(ncp(object)) != 1)
    stop("ncp has to be a numeric of length 1")      
  else return(TRUE)
}

setValidity("TParameter", validTParameter)

################################
##
## Class: Student distribution
##
################################

setClass("Td",  prototype = prototype(r = function(n){ rt(n,  df = 1, ncp = 0) },
                                  d = function(x, ...){ dt(x,  df = 1, ncp = 0, ...) },
                                  p = function(x, ...){ pt(x,  df = 1, ncp = 0, ...) },
                                  q = function(x, ...){ qt(x,  df = 1, ncp = 0, ...) },
                                  img = new("Reals"),
                                  param = new("TParameter", df = 1, ncp = 0,
                                          name = gettext("Parameter of a Student distribution" )),
                                  .withArith = FALSE,
                                  .withSim = FALSE),
       contains = "AbscontDistribution")

## Initialize method
setMethod("initialize", "Td",
          function(.Object, df = 1, ncp = 0) {
            .Object@img <- new("Reals")
            .Object@param <- new("TParameter", df = df, ncp = ncp, 
                                  name = gettext("Parameter of a Student distribution"))
            .Object@r <- function(n){ rt(n, df = dfSub, ncp = ncpSub) }
            body(.Object@r) <- substitute({ rt(n, df = dfSub, ncp = ncpSub) }, 
                                          list(dfSub = df, ncpSub = ncp))
            .Object@d <- function(x, ...){ dt(x, df = dfSub, ncp = ncpSub, ...) }
            body(.Object@d) <- substitute({ dt(x, df = dfSub, ncp = ncpSub, ...) },
                                          list(dfSub = df, ncpSub = ncp))
            .Object@p <- function(x, ...){ pt(x, df = dfSub, ncp = ncpSub, ...) }
            body(.Object@p) <- substitute({ pt(x, df = dfSub, ncp = ncpSub, ...) },
                                          list(dfSub = df, ncpSub = ncp))
            .Object@q <- function(x, ...){ qt(x, df = dfSub, ncp = ncpSub, ...) }
            body(.Object@q) <- substitute({ qt(x, df = dfSub, ncp = ncpSub, ...) },
                                          list(dfSub = df, ncpSub = ncp))
            .Object@.withSim   <- FALSE
            .Object@.withArith <- FALSE
            .Object
          })

## wrapped access methods
setMethod("df", "Td", function(x, ...) df(param(x)))
setMethod("ncp", "Td", function(object) ncp(param(object)))

## wrapped replace methods
setMethod("df<-", "Td", function(object, value) new("Td", df = value, ncp = ncp(object)))
setMethod("ncp<-", "Td", function(object, value) new("Td", df = df(object), ncp = value))
