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

setClass("Dirac", contains = "DiscreteDistribution")

setMethod("initialize", "Dirac",
          function(.Object, location = 0) {
            .Object@img <- new("Reals")
            .Object@param <- new("DiracParameter", location = location, name = "Parameter of a Dirac distribution" )
            .Object@support <- location
            .Object@r <- function(n){ array(location, n)}
            .Object@d <- function(x){ y <- rep(location,length(x))
                                      as.numeric(x == y)}
            .Object@p <- function(p){ as.numeric(p + 10^-10 >= location) }
            .Object@q <- function(q){
              if(q < 0) stop("Quantile has to be in [0,1]")
              if(q > 1) stop("Quantile has to be in [0,1]")
              location }
            .Object
          })

### wrapped access methods
setMethod("location", "Dirac", function(object) location(param(object)))
### wrapped replace methods
setMethod("location<-", "Dirac", function(object, value) new("Dirac", location = value))
