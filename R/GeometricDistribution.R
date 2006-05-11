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

setClass("Geom",  prototype = prototype(r = function(n){ rgeom(n, prob = 0.5) },
                                  d = function(x, ...){ dgeom(x, prob = 0.5, ...) },
                                  p = function(x, ...){ pgeom(x, prob = 0.5, ...) },
                                  q = function(x, ...){ qgeom(x, prob = 0.5, ...) },
                                  img = new("Naturals"),
                                  param = new("GeomParameter", prob = 0.5, name = gettext("Parameter of a geometric distribution")),
                                  .withArith = FALSE,
                                  .withSim = FALSE),
     contains = "Nbinom")

setMethod("initialize", "Geom",
          function(.Object, prob = 0.5) {
            .Object@img <- new("Naturals")
            .Object@param <- new("GeomParameter", prob = prob, name = gettext("Parameter of a geometric distribution"))
            .Object@support <- 0:qgeom(1 - getdistrOption("TruncQuantile"), prob <- prob)
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
            .Object@.withSim   <- FALSE
            .Object@.withArith <- FALSE
            .Object
          })


## wrapped access methods
setMethod("prob", "Geom", function(object) prob(param(object)))
## wrapped replace methods
setMethod("prob<-", "Geom", function(object, value) new("Geom", prob = value))

setMethod("size", "Geom", function(object) return(1))
## wrapped replace methods
#setMethod("size<-", "Geom", function(object, value) { 
#                    if(value>1) 
#                          {stop("This gives an Nbinom distribution; try 'size(as(<objectname>,\"Nbinom\"))'")
#                          }
#                     else object } )

#setIs("Geom", "Nbinom", coerce = function(obj) {new("Nbinom", size = 1, prob = prob(obj))},
#                       replace = function(obj, value) {new("Nbinom", size = value@size, prob = value@prob)}) 
#   ## a negativ binomial-distribution with size = 1 and prob = prob(obj)
#setAs(from = "Geom", to = "Nbinom", def = function(from) {new("Nbinom", size = 1, prob = prob(from))},
#       replace = function(from, value) {new("Nbinom", size = value@size, prob = value@prob)})
