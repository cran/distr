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
  if(length(rate(object)) != 1)
    stop("rate has to be a numeric of length 1")    
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
            .Object@r <- function(n){ rexp(n, rate = rateSub) }
            body(.Object@r) <- substitute({ rexp(n, rate = rateSub) },
                                          list(rateSub = rate))
            .Object@d <- function(x, ...){ dexp(x, rate = rateSub, ...) }
            body(.Object@d) <- substitute({ dexp(x, rate = rateSub, ...) },
                                          list(rateSub = rate))
            .Object@p <- function(x, ...){ pexp(x, rate = rateSub, ...) }
            body(.Object@p) <- substitute({ pexp(x, rate = rateSub, ...) },
                                          list(rateSub = rate))
            .Object@q <- function(x, ...){ qexp(x, rate = rateSub, ...) }
            body(.Object@q) <- substitute({ qexp(x, rate = rateSub, ...) },
                                          list(rateSub = rate))
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
