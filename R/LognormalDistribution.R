
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

setClass("Lnorm", contains = "AbscontDistribution")

## Initialize method
setMethod("initialize", "Lnorm",
          function(.Object, meanlog = 0, sdlog = 1) {
            .Object@img <- new("Reals")
            .Object@param <- new("LnormParameter", meanlog = meanlog, sdlog = sdlog, name = "Parameter of a lognormal distribution")
            .Object@r <- function(n){ rlnorm(n, meanlog = meanlog, sdlog = sdlog) }
            .Object@d <- function(x, ...){ dlnorm(x, meanlog = meanlog, sdlog = sdlog, ...) }
            .Object@p <- function(x, ...){ plnorm(x, meanlog = meanlog, sdlog = sdlog, ...) }
            .Object@q <- function(x, ...){ qlnorm(x, meanlog = meanlog, sdlog = sdlog, ...) }
            .Object
          })

## wrapped access methods
setMethod("meanlog", "Lnorm", function(object) meanlog(param(object)))
setMethod("sdlog", "Lnorm", function(object) sdlog(param(object)))

## wrapped replace methods
setMethod("meanlog<-", "Lnorm", function(object, value) new("Lnorm", meanlog = value, sdlog = sdlog(object)))
setMethod("sdlog<-", "Lnorm", function(object, value) new("Lnorm", meanlog = meanlog(object), sdlog = value))
