
################################
##
## Class: LogisParameter
##
################################

setClass("LogisParameter", representation(location = "numeric", scale = "numeric"), contains = "Parameter")

## Access Methods
if(!isGeneric("location")) setGeneric("location", function(object) standardGeneric("location"))
if(!isGeneric("scale")) setGeneric("scale", function(x, center = TRUE, scale = TRUE) standardGeneric("scale"))
setMethod("location", "LogisParameter", function(object) object@location)
setMethod("scale", "LogisParameter", function(x, center = TRUE, scale = TRUE) x@scale)
## Replace Methods
if(!isGeneric("location<-")) setGeneric("location<-", function(object, value) standardGeneric("location<-"))
if(!isGeneric("scale<-")) setGeneric("scale<-", function(object, value) standardGeneric("scale<-"))
setReplaceMethod("location", "LogisParameter", function(object, value){ object@location <- value; object})
setReplaceMethod("scale", "LogisParameter", function(object, value){ object@scale <- value; object})

validLogisParameter <- function(object){
  if(scale(object) <= 0)
    stop("scale has to be positive")
  else return(TRUE)
}

setValidity("LogisParameter", validLogisParameter)


################################
##
## Class: logistic distribution
##
################################

setClass("Logis", contains = "AbscontDistribution")

## Initialize method
setMethod("initialize", "Logis",
          function(.Object, location = 0, scale = 1) {
            .Object@img <- new("Reals")
            .Object@param <- new("LogisParameter", location = location, scale = scale, name = "Parameter of a logistic distribution")
            .Object@r <- function(n){ rlogis(n, location = location, scale = scale) }
            .Object@d <- function(x, ...){ dlogis(x, location = location, scale = scale, ...) }
            .Object@p <- function(x, ...){ plogis(x, location = location, scale = scale, ...) }
            .Object@q <- function(x, ...){ qlogis(x, location = location, scale = scale, ...) }
            .Object
          })

## wrapped access methods
setMethod("location", "Logis", function(object) location(param(object)))
setMethod("scale", "Logis", function(x, center = TRUE, scale = TRUE) scale(param(x)))

## wrapped replace methods
setMethod("location<-", "Logis", function(object, value) new("Logis", location = value, scale = scale(object)))
setMethod("scale<-", "Logis", function(object, value) new("Logis", location = location(object), scale = value))
