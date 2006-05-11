
################################
##
## Class: BetaParameter
##
################################

setClass("BetaParameter", representation(shape1 = "numeric", shape2 = "numeric", ncp = "numeric"), 
          contains = "Parameter", prototype= c(shape1 = 1, shape2 = 1, ncp = 0))

## Access methods
if(!isGeneric("shape1")) setGeneric("shape1", function(object) standardGeneric("shape1"))
if(!isGeneric("shape2")) setGeneric("shape2", function(object) standardGeneric("shape2"))
if(!isGeneric("ncp")) setGeneric("ncp", function(object) standardGeneric("ncp"))

setMethod("shape1", "BetaParameter", function(object) object@shape1)
setMethod("shape2", "BetaParameter", function(object) object@shape2)
setMethod("ncp", "BetaParameter", function(object) object@ncp)

## Replace Methoden
if(!isGeneric("shape1<-")) setGeneric("shape1<-", function(object, value) standardGeneric("shape1<-"))
if(!isGeneric("shape2<-")) setGeneric("shape2<-", function(object, value) standardGeneric("shape2<-"))
if(!isGeneric("ncp<-")) setGeneric("ncp<-", function(object, value) standardGeneric("ncp<-"))

setReplaceMethod("shape1", "BetaParameter", function(object, value){ object@shape1 <- value; object})
setReplaceMethod("shape2", "BetaParameter", function(object, value){ object@shape2 <- value; object})
setReplaceMethod("ncp", "BetaParameter", function(object, value){ object@ncp <- value; object})


validBetaParameter <- function(object){
  if(length(shape1(object)) != 1)
    stop("shape1 has to be a numeric of length 1")    
  if(shape1(object) <= 0)
    stop("shape1 has to be positive")
  if(length(shape2(object)) != 1)
    stop("shape2 has to be a numeric of length 1")    
  if(shape2(object) <= 0)
    stop("shape2 has to be positive")
  if(length(ncp(object)) != 1)
    stop("ncp has to be a numeric of length 1")      
  else return(TRUE)
}

setValidity("BetaParameter", validBetaParameter)


################################
##
## Class: BetaDistribution
##
################################

setClass("Beta",  prototype = prototype(r = function(n){ rbeta(n,  shape1 = 1, shape2 = 1, ncp = 0) },
                                  d = function(x, ...){ dbeta(x,  shape1 = 1, shape2 = 1, ncp = 0, ...) },
                                  p = function(x, ...){ pbeta(x,  shape1 = 1, shape2 = 1, ncp = 0, ...) },
                                  q = function(x, ...){ qbeta(x,  shape1 = 1, shape2 = 1, ncp = 0, ...) },
                                  img = new("Reals"),
                                  param = new("BetaParameter", shape1 = 1, shape2 = 1, ncp = 0,
                                          name = gettext("Parameter of a beta distribution")),
                                  .withArith = FALSE,
                                  .withSim = FALSE),
         contains = "AbscontDistribution")

## initialize method
setMethod("initialize", "Beta",
          function(.Object, shape1 = 1, shape2 = 1, ncp = 0) {
            .Object@img <- new("Reals")
            .Object@param <- new("BetaParameter", shape1 = shape1, shape2 = shape2, ncp = ncp,
                                  name = gettext("Parameter of a beta distribution"))
            .Object@r <- function(n){ rbeta(n, shape1 = shape1Sub, shape2 = shape2Sub, ncp = ncpSub) }
            body(.Object@r) <- substitute({ rbeta(n, shape1 = shape1Sub, shape2 = shape2Sub, ncp = ncpSub) },
                                          list(shape1Sub = shape1, shape2Sub = shape2, ncpSub = ncp))
            .Object@d <- function(x, ...){ dbeta(x, shape1 = shape1Sub, shape2 = shape2Sub, ncp = ncpSub) }
            body(.Object@d) <- substitute({ dbeta(x, shape1 = shape1Sub, shape2 = shape2Sub, ncp = ncpSub) },
                                          list(shape1Sub = shape1, shape2Sub = shape2, ncpSub = ncp))
            .Object@p <- function(x, ...){ pbeta(x, shape1 = shape1Sub, shape2 = shape2Sub, ncp = ncpSub, ...) }
            body(.Object@p) <- substitute({ pbeta(x, shape1 = shape1Sub, shape2 = shape2Sub, ncp = ncpSub, ...) },
                                          list(shape1Sub = shape1, shape2Sub = shape2, ncpSub = ncp))
            .Object@q <- function(x, ...){ qbeta(x, shape1 = shape1Sub, shape2 = shape2Sub, ncp = ncpSub, ...) }
            body(.Object@q) <- substitute({ qbeta(x, shape1 = shape1Sub, shape2 = shape2Sub, ncp = ncpSub, ...) },
                                          list(shape1Sub = shape1, shape2Sub = shape2, ncpSub = ncp))
            .Object@.withSim   <- FALSE
            .Object@.withArith <- FALSE
            .Object
          })

## wrapped access methods
setMethod("shape1", "Beta", function(object) shape1(param(object)))
setMethod("shape2", "Beta", function(object) shape2(param(object)))
setMethod("ncp", "Beta", function(object) ncp(param(object)))

## wrapped replace methods
setMethod("shape1<-", "Beta", function(object, value) new("Beta", shape1 = value, shape2 = shape2(object), ncp = ncp(object)))
setMethod("shape2<-", "Beta", function(object, value) new("Beta", shape1 = shape1(object), shape2 = value, ncp = ncp(object)))
setMethod("ncp<-", "Beta", function(object, value) new("Beta", shape1 = shape1(object), shape2 = shape2(object), ncp = value))
