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

setClass("Exp",  prototype = prototype(r = function(n){ rexp(n, rate = 1) },
                                  d = function(x, ...){ dexp(x, rate = 1, ...) },
                                  p = function(x, ...){ pexp(x, rate = 1, ...) },
                                  q = function(x, ...){ qexp(x, rate = 1, ...) },
                                  img = new("Reals"),
                                  param = new("ExpParameter", rate = 1, name = gettext("Parameter of a exponential distribution")),
                                  .withArith = FALSE,
                                  .withSim = FALSE),
      contains = "ExpOrGammaOrChisq")

## Initialize method
setMethod("initialize", "Exp",
          function(.Object, rate = 1, .withArith = FALSE) {
            .Object@img <- new("Reals")
            .Object@param <- new("ExpParameter", rate = rate, name = gettext("Parameter of a exponential distribution"))
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
            .Object@.withSim   <- FALSE
            .Object@.withArith <- .withArith
            .Object
          })

## wrapped access methods
setMethod("rate", "Exp", function(object) rate(param(object)))

## wrapped replace methods
setMethod("rate<-", "Exp", function(object, value) new("Exp", rate = value))

##obsolete by setIs
#setMethod("+", c("Exp","Exp"),
#          function(e1,e2){
#            if(is.logical(all.equal(rate(e1),rate(e2))))    
#              return(new("Gamma", shape = 2, scale = rate(e1), .withArith = TRUE))
#            
#            return(as(e1, "AbscontDistribution") + e2)
#          })
setMethod("*", c("Exp","numeric"),
          function(e1, e2){
            if (length(e2)>1) stop("length of operator must be 1")
            if(e2 == 0) return(new("Dirac", location = 0, .withArith = TRUE))
            if(e2 > 0) return(new("Exp",rate = rate(e1) / e2, .withArith = TRUE))
            return(-1 * as(Exp(rate = rate(e1) / abs(e2)), "AbscontDistribution"))
          })

setIs("Exp", "Gammad", coerce = function(obj) {new("Gammad", shape = 1, scale = 1/rate(obj))},
                      replace = function(obj, value) {new("Gammad", shape = value@shape, scale = value@scale)}) 
   ## a Gamma distribution with shape = 1 and scale = 1/rate(obj)

setIs("Exp", "Weibull", coerce = function(obj) {new("Weibull", shape = 1, scale = 1/rate(obj))},
                       replace = function(obj, value) {new("Weibull", shape = value@shape, scale = value@scale)}) 
   ## a Weibull distribution with shape = 1 and scale = 1/rate(obj)

#setAs(to = "Exp", from = "Gammad", def = function(from) {new("Gammad", shape = 1, scale = 1/rate(from))},
#       replace = function(from, value) {new("Gammad", shape = value@shape, scale = value@scale)})
#setAs(from = "Exp", to = "Weibull", def = function(from) {new("Weibull", shape = 1, scale = 1/rate(from))},
#       replace = function(from, value) {new("Weibull", shape = value@shape, scale = value@scale)})
