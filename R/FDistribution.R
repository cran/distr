
################################
##
## Class: FParameter
##
################################

setClass("FParameter", representation(df1 = "numeric", df2 = "numeric"), contains = "Parameter")
## gleiches Problem mit Parameter ncp wie bei BetaDistribution

## Access Methods
if(!isGeneric("df1")) setGeneric("df1", function(object) standardGeneric("df1"))
if(!isGeneric("df2")) setGeneric("df2", function(object) standardGeneric("df2"))
setMethod("df1", "FParameter", function(object) object@df1)
setMethod("df2", "FParameter", function(object) object@df2)
## Replace Methods
if(!isGeneric("df1<-")) setGeneric("df1<-", function(object, value) standardGeneric("df1<-"))
if(!isGeneric("df2<-")) setGeneric("df2<-", function(object, value) standardGeneric("df2<-"))
setReplaceMethod("df1", "FParameter", function(object, value){ object@df1 <- value; object})
setReplaceMethod("df2", "FParameter", function(object, value){ object@df2 <- value; object})

validFParameter <- function(object){
  if(length(df1(object)) != 1)
    stop("df1 has to be a numeric of length 1")    
  if(df1(object) <= 0)
    stop("df1 has to be positive")
  if(length(df2(object)) != 1)
    stop("df2 has to be a numeric of length 1")    
  if(df2(object) <= 0)
    stop("df2 has to be positive")
  else return(TRUE)
}

setValidity("FParameter", validFParameter)

################################
##
## Class: F distribution
##
################################

setClass("Fd", contains = "AbscontDistribution")

## Initialize method
setMethod("initialize", "Fd",
          function(.Object, df1 = 1, df2 = 1) {
            .Object@img <- new("Reals")
            .Object@param <- new("FParameter", df1 = df1, df2 = df2, name = "Parameter of a F distribution")
            .Object@r <- function(n){ rf(n, df1 = df1Sub, df2 = df2Sub) }
            body(.Object@r) <- substitute({ rf(n, df1 = df1Sub, df2 = df2Sub) },
                                          list(df1Sub = df1, df2Sub = df2))
            .Object@d <- function(x, ...){ df(x, df1 = df1Sub, df2 = df2Sub, ...) }
            body(.Object@d) <- substitute({ df(x, df1 = df1Sub, df2 = df2Sub, ...) },
                                          list(df1Sub = df1, df2Sub = df2))
            .Object@p <- function(x, ...){ pf(x, df1 = df1Sub, df2 = df2Sub, ...) }
            body(.Object@p) <- substitute({ pf(x, df1 = df1Sub, df2 = df2Sub, ...) },
                                          list(df1Sub = df1, df2Sub = df2))
            .Object@q <- function(x, ...){ qf(x, df1 = df1Sub, df2 = df2Sub, ...) }
            body(.Object@q) <- substitute({ qf(x, df1 = df1Sub, df2 = df2Sub, ...) },
                                          list(df1Sub = df1, df2Sub = df2))
            .Object
          })

## wrapped access methods
setMethod("df1", "Fd", function(object) df1(param(object)))
setMethod("df2", "Fd", function(object) df2(param(object)))

## wrapped replace methods
setMethod("df1<-", "Fd", function(object, value) new("Fd", df1 = value, df2 = df2(object)))
setMethod("df2<-", "Fd", function(object, value) new("Fd", df1 = df1(object), df2 = value))
