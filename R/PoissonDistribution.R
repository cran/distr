
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

setClass("Pois", contains = "DiscreteDistribution")

setMethod("initialize", "Pois",
          function(.Object, lambda = 1) {
            .Object@img <- new("Naturals")
            .Object@param <- new("PoisParameter", lambda = lambda, name = "Parameter of a Poisson distribution" )
            .Object@support <- 0:(qpois(1 - TruncQuantile, lambda = lambda) + 2)
            .Object@r <- function(n){ rpois(n, lambda = lambda) }
            .Object@d <- function(x, ...){ dpois(x, lambda = lambda, ...) }
            .Object@p <- function(p, ...){ ppois(p, lambda = lambda, ...) }
            .Object@q <- function(q, ...){ qpois(q, lambda = lambda, ...) }
            .Object
          })


## wrapped access methods
setMethod("lambda", "Pois", function(object) lambda(param(object)))
## wrapped replace methods
setMethod("lambda<-", "Pois", function(object, value) new("Pois", lambda = value))

setMethod("+", c("Pois","Pois"),
          function(e1,e2){
            new("Pois", lambda = lambda(e1) + lambda(e2))
          })

