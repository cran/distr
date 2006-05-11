
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

setClass("Cauchy", prototype = prototype(r = function(n){ rcauchy(n,  location = 0, scale = 1) },
                                  d = function(x, ...){ dcauchy(x,  location = 0, scale = 1, ...) },
                                  p = function(x, ...){ pcauchy(x,  location = 0, scale = 1, ...) },
                                  q = function(x, ...){ qcauchy(x,  location = 0, scale = 1, ...) },
                                  img = new("Reals"),
                                  param = new("CauchyParameter", location = 0, scale = 1, 
                                               name = gettext("Parameter of a Cauchy distribution")),
                                  .withArith = FALSE,
                                  .withSim = FALSE),
          contains = "AbscontDistribution")

## initialize method
setMethod("initialize", "Cauchy",
          function(.Object, location = 0, scale = 1) {
            .Object@img <- new("Reals")
            .Object@param <- new("CauchyParameter", location = location, scale = scale, name = gettext("Parameter of a Cauchy distribution"))
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
            .Object@.withSim   <- FALSE
            .Object@.withArith <- FALSE
            .Object
          })

## wrapped access methods
setMethod("location", "Cauchy", function(object) location(param(object)))
setMethod("scale", "Cauchy", function(x, center = TRUE, scale = TRUE) scale(param(x)))

## wrapped replace methods
setMethod("location<-", "Cauchy", function(object, value) new("Cauchy", location = value, scale = scale(object)))
setMethod("scale<-", "Cauchy", function(object, value) new("Cauchy", location = location(object), scale = value))

## some exact arithmetic methods
setMethod("+", c("Cauchy","numeric"),
          function(e1, e2){
            if (length(e2)>1) stop("length of operator must be 1")
            Cauchy(location = location(e1) + e2, scale = scale(e1))  
          })

setMethod("*", c("Cauchy","numeric"),
          function(e1, e2){
            if (length(e2)>1) stop("length of operator must be 1")
            ifelse(e2==0, Dirac(0), Cauchy(location = location(e1) * e2, scale = scale(e1) * abs(e2)))  
          })

setIs("Cauchy", "Td", test = function(obj) {identical(all.equal(location(obj),0),TRUE) && 
                                            identical(all.equal(scale(obj),1), TRUE)}, 
       coerce = function(obj) {new("Td")},
       replace = function(obj, value){new("Td", df=value@df, ncp=value@ncp)}) 
       ## if location==0 and scale==1 a T-distribution with df = 1
#setAs(from="Cauchy", to="Td",
#      def=function(from) {if(identical(all.equal(location(from),0),TRUE) && 
#                             identical(all.equal(scale(from),1), TRUE))
#                                new("Td")
#                          else stop("only a Cauchy(0,1) object can be coerced to a Td object")}, 
#      replace = function(from, value){if(identical(all.equal(location(from),0),TRUE) && 
#                                                    identical(all.equal(scale(from),1), TRUE))
#                                          new("Td", df=value@df, ncp=value@ncp)
#                                      else stop("only a Cauchy(0,1) object can be coerced to a Td object")}
#      ) 
