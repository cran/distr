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
  if(length(prob(object)) != 1)
    stop("prob has to be a numeric of length 1")    
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
            .Object@r <- function(n){ rgeom(n, prob = probSub) }
            body(.Object@r) <- substitute({ rgeom(n, prob = probSub) },
                                          list(probSub = prob))
            .Object@d <- function(x, ...){ dgeom(x, prob = probSub, ...) }
            body(.Object@d) <- substitute({ dgeom(x, prob = probSub, ...) },
                                          list(probSub = prob))
            .Object@p <- function(p, ...){ pgeom(p, prob = probSub, ...) }
            body(.Object@p) <- substitute({ pgeom(p, prob = probSub, ...) },
                                          list(probSub = prob))
            .Object@q <- function(q, ...){ qgeom(q, prob = probSub, ...) }
            body(.Object@q) <- substitute({ qgeom(q, prob = probSub, ...) },
                                          list(probSub = prob))
            .Object
          })


## wrapped access methods
setMethod("prob", "Geom", function(object) prob(param(object)))
## wrapped replace methods
setMethod("prob<-", "Geom", function(object, value) new("Geom", prob = value))


