
################################
##
## Class: BetaParameter
##
################################

setClass("BetaParameter", representation(shape1 = "numeric", shape2 = "numeric"), contains = "Parameter")

## Access methods
if(!isGeneric("shape1")) setGeneric("shape1", function(object) standardGeneric("shape1"))
if(!isGeneric("shape2")) setGeneric("shape2", function(object) standardGeneric("shape2"))

setMethod("shape1", "BetaParameter", function(object) object@shape1)
setMethod("shape2", "BetaParameter", function(object) object@shape2)

## Replace Methoden
if(!isGeneric("shape1<-")) setGeneric("shape1<-", function(object, value) standardGeneric("shape1<-"))
if(!isGeneric("shape2<-")) setGeneric("shape2<-", function(object, value) standardGeneric("shape2<-"))

setReplaceMethod("shape1", "BetaParameter", function(object, value){ object@shape1 <- value; object})
setReplaceMethod("shape2", "BetaParameter", function(object, value){ object@shape2 <- value; object})


validBetaParameter <- function(object){
  if(shape1(object) <= 0)
    stop("shape1 has to be positive")
  if(shape2(object) <= 0)
    stop("shape2 has to be positive")
  else return(TRUE)
}

setValidity("BetaParameter", validBetaParameter)


################################
##
## Class: BetaDistribution
##
################################

setClass("Beta", contains = "AbscontDistribution")

## initialize method
setMethod("initialize", "Beta",
          function(.Object, shape1 = 1, shape2 = 1) {
            .Object@img <- new("Reals")
            .Object@param <- new("BetaParameter", shape1 = shape1, shape2 = shape2, name = "Parameter of a beta distribution")
            .Object@r <- function(n){ rbeta(n, shape1 = shape1, shape2 = shape2) }
            .Object@d <- function(x, ...){ dbeta(x, shape1 = shape1, shape2 = shape2) }
            .Object@p <- function(x, ...){ pbeta(x, shape1 = shape1, shape2 = shape2, ...) }
            .Object@q <- function(x, ...){ qbeta(x, shape1 = shape1, shape2 = shape2, ...) }
            .Object
          })

## wrapped access methods
setMethod("shape1", "Beta", function(object) shape1(param(object)))
setMethod("shape2", "Beta", function(object) shape2(param(object)))

## wrapped replace methods
setMethod("shape1<-", "Beta", function(object, value) new("Beta", shape1 = value, shape2 = shape2(object)))
setMethod("shape2<-", "Beta", function(object, value) new("Beta", shape1 = shape1(object), shape2 = value))
