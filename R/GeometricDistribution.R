################################
##
## Class: GeomParameter
##
################################

setClass("GeomParameter", representation(prob = "numeric"), contains = "Parameter")

## Access Methods
if(!isGeneric("prob")) setGeneric("prob", function(object) standardGeneric("prob"))
setMethod("prob", "GeomParameter", function(object) object@prob)
## Replace Methods
if(!isGeneric("prob<-")) setGeneric("prob<-", function(object, value) standardGeneric("prob<-"))
setReplaceMethod("prob", "GeomParameter", function(object, value){ object@prob <- value; object})


validGeomParameter <- function(object){
  if(prob(object) <= 0)
    stop("prob has to be in (0,1]")
  if(prob(object) > 1)
    stop("prob has to be in (0,1]")
  else return(TRUE)
}

setValidity("GeomParameter", validGeomParameter)

################################
##
## Class: geometric distribution 
##
################################

setClass("Geom", contains = "DiscreteDistribution")

setMethod("initialize", "Geom",
          function(.Object, prob = 0.5) {
            .Object@img <- new("Naturals")
            .Object@param <- new("GeomParameter", prob = prob, name = "Parameter of a geometric distribution")
            .Object@support <- 0:qgeom(1 - TruncQuantile, prob <- prob)
            .Object@r <- function(n){ rgeom(n, prob = prob) }
            .Object@d <- function(x, ...){ dgeom(x, prob = prob, ...) }
            .Object@p <- function(p, ...){ pgeom(p, prob = prob, ...) }
            .Object@q <- function(q, ...){ qgeom(q, prob = prob, ...) }
            .Object
          })


## wrapped access methods
setMethod("prob", "Geom", function(object) prob(param(object)))
## wrapped replace methods
setMethod("prob<-", "Geom", function(object, value) new("Geom", prob = value))


