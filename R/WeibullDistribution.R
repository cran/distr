
################################
##
## Class: WeibullParameter
##
################################

setClass("WeibullParameter", representation(shape = "numeric", scale = "numeric"), contains = "Parameter")

## Access Methods
if(!isGeneric("shape")) setGeneric("shape", function(object) standardGeneric("shape"))
if(!isGeneric("scale")) setGeneric("scale", function(x, center = TRUE, scale = TRUE) standardGeneric("scale"))
setMethod("shape", "WeibullParameter", function(object) object@shape)
setMethod("scale", "WeibullParameter", function(x, center = TRUE, scale = TRUE) x@scale)
## Replace Methods
if(!isGeneric("shape<-")) setGeneric("shape<-", function(object, value) standardGeneric("shape<-"))
if(!isGeneric("scale<-")) setGeneric("scale<-", function(object, value) standardGeneric("scale<-"))
setReplaceMethod("shape", "WeibullParameter", function(object, value){ object@shape <- value; object})
setReplaceMethod("scale", "WeibullParameter", function(object, value){ object@scale <- value; object})

validWeibullParameter <- function(object){
  if(shape(object) <= 0)
    stop("shape has to be positive")
  if(scale(object) <= 0)
    stop("scale has to be positive")
  else return(TRUE)
}

setValidity("WeibullParameter", validWeibullParameter)



################################
##
## Class: Weibull distribution
##
################################

setClass("Weibull", contains = "AbscontDistribution")

## Initialize method
setMethod("initialize", "Weibull",
          function(.Object, shape = 1, scale = 1) {
            .Object@img <- new("Reals")
            .Object@param <- new("WeibullParameter", shape = shape, scale = scale, name = "Parameter of a Weibull distribution")
            .Object@r <- function(n){ rweibull(n, shape = shape, scale = scale) }
            .Object@d <- function(x, ...){ dweibull(x, shape = shape, scale = scale, ...) }
            .Object@p <- function(x, ...){ pweibull(x, shape = shape, scale = scale, ...) }
            .Object@q <- function(x, ...){ qweibull(x, shape = shape, scale = scale, ...) }
            .Object
          })

## wrapped access methods
setMethod("shape", "Weibull", function(object) shape(param(object)))
setMethod("scale", "Weibull", function(x, center = TRUE, scale = TRUE) scale(param(x)))

## wrapped replace methods
setMethod("shape<-", "Weibull", function(object, value) new("Weibull", shape = value, scale = scale(object)))
setMethod("scale<-", "Weibull", function(object, value) new("Weibull", shape = shape(object), scale = value))
