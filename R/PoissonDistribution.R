
################################
##
## Class: PoisParameter
##
################################

setClass("PoisParameter", representation(lambda = "numeric"), contains = "Parameter")

## Access Methods
if(!isGeneric("lambda")) setGeneric("lambda", function(object) standardGeneric("lambda"))
setMethod("lambda", "PoisParameter", function(object) object@lambda)
## Replace Methods
if(!isGeneric("lambda<-")) setGeneric("lambda<-", function(object, value) standardGeneric("lambda<-"))
setReplaceMethod("lambda", "PoisParameter", function(object, value){ object@lambda <- value; object})

validPoisParameter <- function(object){
  if(length(lambda(object)) != 1)
    stop("lambda has to be a numeric of length 1")    
  if(lambda(object) < 0)
    stop("lambda has to be not negative")
  else return(TRUE)
}

setValidity("PoisParameter", validPoisParameter)

################################
##
## Class: Poisson distribution 
##
################################

setClass("Pois",  prototype = prototype(r = function(n){ rpois(n, lambda = 1) },
                                  d = function(x, ...){ dpois(x, lambda = 1, ...) },
                                  p = function(x, ...){ ppois(x, lambda = 1, ...) },
                                  q = function(x, ...){ qpois(x, lambda = 1, ...) },
                                  img = new("Naturals"),
                                  param = new("PoisParameter", lambda = 1, name = gettext("Parameter of a Poisson distribution")),
                                  .withArith = FALSE,
                                  .withSim = FALSE),
      contains = "DiscreteDistribution")

setMethod("initialize", "Pois",
          function(.Object, lambda = 1, .withArith = FALSE) {
            .Object@img <- new("Naturals")
            .Object@param <- new("PoisParameter", lambda = lambda, name = gettext("Parameter of a Poisson distribution") )
            .Object@support <- 0:(qpois(1 - getdistrOption("TruncQuantile"), lambda = lambda) + 2)
            .Object@r <- function(n){ rpois(n, lambda = lambdaSub) }
            body(.Object@r) <- substitute({ rpois(n, lambda = lambdaSub) },
                                          list(lambdaSub = lambda))
            .Object@d <- function(x, ...){ dpois(x, lambda = lambdaSub, ...) }
            body(.Object@d) <- substitute({ dpois(x, lambda = lambdaSub, ...) },
                                          list(lambdaSub = lambda))
            .Object@p <- function(p, ...){ ppois(p, lambda = lambdaSub, ...) }
            body(.Object@p) <- substitute({ ppois(p, lambda = lambdaSub, ...) },
                                          list(lambdaSub = lambda))
            .Object@q <- function(q, ...){ qpois(q, lambda = lambdaSub, ...) }
            body(.Object@q) <- substitute({ qpois(q, lambda = lambdaSub, ...) },
                                          list(lambdaSub = lambda))
            .Object@.withSim   <- FALSE
            .Object@.withArith <- .withArith
            .Object
          })


## wrapped access methods
setMethod("lambda", "Pois", function(object) lambda(param(object)))
## wrapped replace methods
setMethod("lambda<-", "Pois", function(object, value) new("Pois", lambda = value))

setMethod("+", c("Pois","Pois"),
          function(e1,e2){
            new("Pois", lambda = lambda(e1) + lambda(e2), .withArith = TRUE)
          })
