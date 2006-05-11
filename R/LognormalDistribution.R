
################################
##
## Class: LnormParameter
##
################################

setClass("LnormParameter", representation(meanlog = "numeric", sdlog = "numeric"), contains = "Parameter")

## Access Methods
if(!isGeneric("meanlog")) setGeneric("meanlog", function(object) standardGeneric("meanlog"))
if(!isGeneric("sdlog")) setGeneric("sdlog", function(object) standardGeneric("sdlog"))
setMethod("meanlog", "LnormParameter", function(object) object@meanlog)
setMethod("sdlog", "LnormParameter", function(object) object@sdlog)
## Replace Methods
if(!isGeneric("meanlog<-")) setGeneric("meanlog<-", function(object, value) standardGeneric("meanlog<-"))
if(!isGeneric("sdlog<-")) setGeneric("sdlog<-", function(object, value) standardGeneric("sdlog<-"))
setReplaceMethod("meanlog", "LnormParameter", function(object, value){ object@meanlog <- value; object})
setReplaceMethod("sdlog", "LnormParameter", function(object, value){ object@sdlog <- value; object})


validLnormParameter <- function(object){
  if(length(sdlog(object)) != 1)
    stop("sdlog has to be a numeric of length 1")    
  if(sdlog(object) <= 0)
    stop("sdlog has to be positive")
  else return(TRUE)
}

setValidity("LnormParameter", validLnormParameter)



################################
##
## Class: lognormal distribution
##
################################

setClass("Lnorm",  prototype = prototype(r = function(n){ rlnorm(n, meanlog = 0, sdlog = 1) },
                                  d = function(x, ...){ dlnorm(x, meanlog = 0, sdlog = 1, ...) },
                                  p = function(x, ...){ plnorm(x, meanlog = 0, sdlog = 1, ...) },
                                  q = function(x, ...){ qlnorm(x, meanlog = 0, sdlog = 1, ...) },
                                  img = new("Reals"),
                                  param = new("LnormParameter", meanlog = 0, sdlog = 1, 
                                  name = gettext("Parameter of a lognormal distribution")),
                                  .withArith = FALSE,
                                  .withSim = FALSE),
      contains = "AbscontDistribution")

## Initialize method
setMethod("initialize", "Lnorm",
          function(.Object, meanlog = 0, sdlog = 1) {
            .Object@img <- new("Reals")
            .Object@param <- new("LnormParameter", meanlog = meanlog, sdlog = sdlog, 
                                  name = gettext("Parameter of a lognormal distribution"))
            .Object@r <- function(n){ rlnorm(n, meanlog = meanlogSub, sdlog = sdlogSub) }
            body(.Object@r) <- substitute({ rlnorm(n, meanlog = meanlogSub, sdlog = sdlogSub) },
                                          list(meanlogSub = meanlog, sdlogSub = sdlog))
            .Object@d <- function(x, ...){ dlnorm(x, meanlog = meanlogSub, sdlog = sdlogSub, ...) }
            body(.Object@d) <- substitute({ dlnorm(x, meanlog = meanlogSub, sdlog = sdlogSub, ...) },
                                          list(meanlogSub = meanlog, sdlogSub = sdlog))
            .Object@p <- function(x, ...){ plnorm(x, meanlog = meanlogSub, sdlog = sdlogSub, ...) }
            body(.Object@p) <- substitute({ plnorm(x, meanlog = meanlogSub, sdlog = sdlogSub, ...) },
                                          list(meanlogSub = meanlog, sdlogSub = sdlog))
            .Object@q <- function(x, ...){ qlnorm(x, meanlog = meanlogSub, sdlog = sdlogSub, ...) }
            body(.Object@q) <- substitute({ qlnorm(x, meanlog = meanlogSub, sdlog = sdlogSub, ...) },
                                          list(meanlogSub = meanlog, sdlogSub = sdlog))
            .Object@.withSim   <- FALSE
            .Object@.withArith <- FALSE
            .Object
          })

## wrapped access methods
setMethod("meanlog", "Lnorm", function(object) meanlog(param(object)))
setMethod("sdlog", "Lnorm", function(object) sdlog(param(object)))

## wrapped replace methods
setMethod("meanlog<-", "Lnorm", function(object, value) new("Lnorm", meanlog = value, sdlog = sdlog(object)))
setMethod("sdlog<-", "Lnorm", function(object, value) new("Lnorm", meanlog = meanlog(object), sdlog = value))
