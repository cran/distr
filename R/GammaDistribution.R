
################################
##
## Class: GammaParameter
##
################################

setClass("GammaParameter", representation(shape = "numeric", scale = "numeric"), contains = "Parameter")

## Access Methods
if(!isGeneric("shape")) setGeneric("shape", function(object) standardGeneric("shape"))
if(!isGeneric("scale")) setGeneric("scale", function(x, center = TRUE, scale = TRUE) standardGeneric("scale"))
setMethod("shape", "GammaParameter", function(object) object@shape)
setMethod("scale", "GammaParameter", function(x, center = TRUE, scale = TRUE) x@scale)
## Replace Methods
if(!isGeneric("shape<-")) setGeneric("shape<-", function(object, value) standardGeneric("shape<-"))
if(!isGeneric("scale<-")) setGeneric("scale<-", function(object, value) standardGeneric("scale<-"))
setReplaceMethod("shape", "GammaParameter", function(object, value){ object@shape <- value; object})
setReplaceMethod("scale", "GammaParameter", function(object, value){ object@scale <- value; object})


validGammaParameter <- function(object){
  if(length(shape(object)) != 1)
    stop("shape has to be a numeric of length 1")    
  if(shape(object) <= 0)
    stop("shape has to be positive")
  if(length(scale(object)) != 1)
    stop("scale has to be a numeric of length 1")    
  if(scale(object) <= 0)
    stop("scale has to be positive")
  else return(TRUE)
}

setValidity("GammaParameter", validGammaParameter)


################################
##
## Class: gamma distribution
##
################################

setClass("Gamma", contains = "AbscontDistribution")

## Initialize method
setMethod("initialize", "Gamma",
          function(.Object, shape = 1, scale = 1) {
            .Object@img <- new("Reals")
            .Object@param <- new("GammaParameter", shape = shape, scale = scale, name = "Parameter of a gamma distribution")
            .Object@r <- function(n){ rgamma(n, shape = shapeSub, scale = scaleSub) }
            body(.Object@r) <- substitute({ rgamma(n, shape = shapeSub, scale = scaleSub) },
                                          list(shapeSub = shape, scaleSub = scale))
            .Object@d <- function(x, ...){ dgamma(x, shape = shapeSub, scale = scaleSub, ...) }
            body(.Object@d) <- substitute({ dgamma(x, shape = shapeSub, scale = scaleSub, ...) },
                                          list(shapeSub = shape, scaleSub = scale))
            .Object@p <- function(x, ...){ pgamma(x, shape = shapeSub, scale = scaleSub, ...) }
            body(.Object@p) <- substitute({ pgamma(x, shape = shapeSub, scale = scaleSub, ...) },
                                          list(shapeSub = shape, scaleSub = scale))
            .Object@q <- function(x, ...){ qgamma(x, shape = shapeSub, scale = scaleSub, ...) }
            body(.Object@q) <- substitute({ qgamma(x, shape = shapeSub, scale = scaleSub, ...) },
                                          list(shapeSub = shape, scaleSub = scale))
            .Object
          })

## wrapped access methods
setMethod("shape", "Gamma", function(object) shape(param(object)))
setMethod("scale", "Gamma", function(x, center = TRUE, scale = TRUE) scale(param(x)))

## wrapped replace methods
setMethod("shape<-", "Gamma", function(object, value) new("Gamma", shape = value, scale = scale(object)))
setMethod("scale<-", "Gamma", function(object, value) new("Gamma", shape = shape(object), scale = value))

setMethod("+", c("Gamma","Gamma"),
          function(e1,e2){
            newshape <- shape(e1) + shape(e2)
            if(is.logical(all.equal(scale(e1),scale(e2))))    
              return(new("Gamma", shape = newshape, scale = scale(e1)))
            
            return(as(e1, "AbscontDistribution") + e2)
          })
setMethod("*", c("Gamma","numeric"),
          function(e1, e2){
            if(e2 == 0) return(new("Dirac", location = 0))
            if(e2 > 0) return(Gamma(shape = shape(e1),
                                    scale = scale(e1) * e2))
            return(-1 * as(Gamma(shape = shape(e1),
                                 scale = scale(e1) * (-e2)), "AbscontDistribution"))
          })
