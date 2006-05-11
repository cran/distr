#################################
###
### Class: DiracParameter
###
#################################

setClass("DiracParameter", representation(location = "numeric"), contains = "Parameter")

### Access Methods
if(!isGeneric("location")) setGeneric("location", function(object) standardGeneric("location"))
setMethod("location", "DiracParameter", function(object) object@location)
### Replace Methods
if(!isGeneric("location<-")) setGeneric("location<-", function(object, value) standardGeneric("location<-"))
setReplaceMethod("location", "DiracParameter", function(object, value){ object@location <- value; object})

validDiracParameter <- function(object){
  if(length(location(object)) != 1)
    stop("location has to be a numeric of length 1")    
  else return(TRUE)
}

setValidity("DiracParameter", validDiracParameter)



#################################
###
### Class: Dirac distribution
###
#################################

setClass("Dirac",  prototype = prototype(r = function(n){ array(0, n)},
                                  d = function(x){ y <- rep(0,length(x))
                                                        as.numeric(x == y)},
                                  p = function(p){ as.numeric(p + 10^-10 >= 0) },
                                  q = function(q){ if ((q<0)||(q>1)) stop("Quantile has to be in [0,1]")
                                                   else return(0) },
                                  img = new("Reals"),
                                  param = new("DiracParameter", location = 0, name = gettext("Parameter of a Dirac distribution")),
                                  .withArith = FALSE,
                                  .withSim = FALSE),
     contains = "DiscreteDistribution")

setMethod("initialize", "Dirac",
          function(.Object, location = 0, .withArith = FALSE) {
            .Object@img <- new("Reals")
            .Object@param <- new("DiracParameter", location = location, name = gettext("Parameter of a Dirac distribution"))
            .Object@support <- location
            .Object@r <- function(n){ array(location, n)}
            .Object@d <- function(x){ y <- rep(location,length(x))
                                      as.numeric(x == y)}
            .Object@p <- function(p){ as.numeric(p + 10^-10 >= location) }
            .Object@q <- function(q){
              if(q < 0) stop("Quantile has to be in [0,1]")
              if(q > 1) stop("Quantile has to be in [0,1]")
              location }
            .Object@.withSim   <- FALSE
            .Object@.withArith <- .withArith
            .Object
          })

### wrapped access methods
setMethod("location", "Dirac", function(object) location(param(object)))
### wrapped replace methods
setMethod("location<-", "Dirac", function(object, value) new("Dirac", location = value))


## some exact arithmetic methods
setMethod("+", c("Dirac","numeric"),
          function(e1, e2){
            if (length(e2)>1) stop("length of operator must be 1")
            new("Dirac", location = location(e1) + e2, .withArith = TRUE)  
          })

setMethod("*", c("Dirac","numeric"),
          function(e1, e2){
            if (length(e2)>1) stop("length of operator must be 1")
            new("Dirac", location = location(e1) * e2, .withArith = TRUE)  
          })

setMethod("+", c("Dirac","Dirac"),
          function(e1,e2){
            new("Dirac", location = location(e1) + location(e2), .withArith = TRUE)
          })

setMethod("*", c("Dirac","Dirac"),
          function(e1,e2){
            new("Dirac", location = location(e1) * location(e2), .withArith = TRUE)
          })

setMethod("-", c("Dirac","Dirac"),
          function(e1,e2){
            new("Dirac", location = location(e1) - location(e2), .withArith = TRUE)
          })

setMethod("/", c("Dirac","Dirac"),
          function(e1,e2){
            if(isTRUE(all.equal(location(e2),0))) stop("location parameter of divisor must not be 0")
            new("Dirac", location = location(e1) / location(e2), .withArith = TRUE)
          })

setMethod("+", c("Dirac","UnivariateDistribution"),
          function(e1,e2){
            location(e1) + e2
          })

setMethod("*", c("Dirac","UnivariateDistribution"),
          function(e1,e2){
            location(e1) * e2
          })

setMethod("+", c("UnivariateDistribution","Dirac"),
          function(e1,e2){
            location(e2) + e1
          })

setMethod("*", c("UnivariateDistribution","Dirac"),
          function(e1,e2){
            location(e2) * e1
          })
