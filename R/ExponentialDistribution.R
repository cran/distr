################################
##
## Class: ExpParameter
##
################################

setClass("ExpParameter", representation(rate = "numeric"), contains = "Parameter")

## Access methods
if(!isGeneric("rate")) setGeneric("rate", function(object) standardGeneric("rate"))
setMethod("rate", "ExpParameter", function(object) object@rate)
## Replace methods
if(!isGeneric("rate<-")) setGeneric("rate<-", function(object, value) standardGeneric("rate<-"))
setReplaceMethod("rate", "ExpParameter", function(object, value){ object@rate <- value; object})

validExpParameter <- function(object){
  if(rate(object) <= 0)
    stop("rate has to be positive")
  else return(TRUE)
}

setValidity("ExpParameter", validExpParameter)


################################
##
## Class: exponential distribution
##
################################

setClass("Exp", contains = "AbscontDistribution")

## Initialize method
setMethod("initialize", "Exp",
          function(.Object, rate = 1) {
            .Object@img <- new("Reals")
            .Object@param <- new("ExpParameter", rate = rate, name = "Parameter of a exponential distribution")
            .Object@r <- function(n){ rexp(n, rate = rate) }
            .Object@d <- function(x, ...){ dexp(x, rate = rate, ...) }
            .Object@p <- function(x, ...){ pexp(x, rate = rate, ...) }
            .Object@q <- function(x, ...){ qexp(x, rate = rate, ...) }
            .Object
          })

## wrapped access methods
setMethod("rate", "Exp", function(object) rate(param(object)))

## wrapped replace methods
setMethod("rate<-", "Exp", function(object, value) new("Exp", rate = value))

setMethod("+", c("Exp","Exp"),
          function(e1,e2){
            if(is.logical(all.equal(rate(e1),rate(e2))))    
              return(new("Gamma", shape = 2, scale = rate(e1)))
            
            return(as(e1, "AbscontDistribution") + e2)
          })
setMethod("*", c("Exp","numeric"),
          function(e1, e2){
            if(e2 == 0) return(new("Dirac", location = 0))
            if(e2 > 0) return(Exp(rate = rate(e1) / e2))
            return(-1 * as(Exp(rate = rate(e1) / abs(e2)), "AbscontDistribution"))
          })
