
################################
##
## Class: CauchyParameter
##
################################

setClass("CauchyParameter", representation(location = "numeric", scale = "numeric"), contains = "Parameter")

## Access methods
if(!isGeneric("location")) setGeneric("location", function(object) standardGeneric("location"))
if(!isGeneric("scale")) setGeneric("scale", function(x, center = TRUE, scale = TRUE) standardGeneric("scale"))
setMethod("location", "CauchyParameter", function(object) object@location)
setMethod("scale", "CauchyParameter", function(x, center = TRUE, scale = TRUE) x@scale)
## Replace Methods
if(!isGeneric("location<-")) setGeneric("location<-", function(object, value) standardGeneric("location<-"))
if(!isGeneric("scale<-")) setGeneric("scale<-", function(object, value) standardGeneric("scale<-"))
setReplaceMethod("location", "CauchyParameter", function(object, value){ object@location <- value; object})
setReplaceMethod("scale", "CauchyParameter", function(object, value){ object@scale <- value; object})

validCauchyParameter <- function(object){
  if(length(location(object)) != 1)
    stop("location has to be a numeric of length 1")    
  if(length(scale(object)) != 1)
    stop("scale has to be a numeric of length 1")    
  if(scale(object) <= 0)
    stop("scale has to be positive")
  else return(TRUE)
}

setValidity("CauchyParameter", validCauchyParameter)



################################
##
## Class: CauchyDistribution
##
################################

setClass("Cauchy", contains = "AbscontDistribution")

## initialize method
setMethod("initialize", "Cauchy",
          function(.Object, location = 0, scale = 1) {
            .Object@img <- new("Reals")
            .Object@param <- new("CauchyParameter", location = location, scale = scale, name = "Parameter of a Cauchy distribution")
            .Object@r <- function(n){ rcauchy(n, location = locationSub, scale = scaleSub) }
            body(.Object@r) <- substitute({ rcauchy(n, location = locationSub, scale = scaleSub) },
                                          list(locationSub = location, scaleSub = scale))
            .Object@d <- function(x, ...){ dcauchy(x, location = locationSub, scale = scaleSub, ...) }
            body(.Object@d) <- substitute({ dcauchy(x, location = locationSub, scale = scaleSub, ...) },
                                          list(locationSub = location, scaleSub = scale))
            .Object@p <- function(x, ...){ pcauchy(x, location = locationSub, scale = scaleSub, ...) }
            body(.Object@p) <- substitute({ pcauchy(x, location = locationSub, scale = scaleSub, ...) },
                                          list(locationSub = location, scaleSub = scale))
            .Object@q <- function(x, ...){ qcauchy(x, location = locationSub, scale = scaleSub, ...) }
            body(.Object@q) <- substitute({ qcauchy(x, location = locationSub, scale = scaleSub, ...) },
                                          list(locationSub = location, scaleSub = scale))
            .Object
          })

## wrapped access methods
setMethod("location", "Cauchy", function(object) location(param(object)))
setMethod("scale", "Cauchy", function(x, center = TRUE, scale = TRUE) scale(param(x)))

## wrapped replace methods
setMethod("location<-", "Cauchy", function(object, value) new("Cauchy", location = value, scale = scale(object)))
setMethod("scale<-", "Cauchy", function(object, value) new("Cauchy", location = location(object), scale = value))
