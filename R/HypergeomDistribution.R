################################
##
## Class: HyperParameter
##
################################

setClass("HyperParameter", representation(m = "numeric", n = "numeric", k = "numeric"), contains = "Parameter")

## Access Methods
if(!isGeneric("m")) setGeneric("m", function(object) standardGeneric("m"))
if(!isGeneric("n")) setGeneric("n", function(object) standardGeneric("n"))
if(!isGeneric("k")) setGeneric("k", function(object) standardGeneric("k"))
setMethod("m", "HyperParameter", function(object) object@m)
setMethod("n", "HyperParameter", function(object) object@n)
setMethod("k", "HyperParameter", function(object) object@k)
## Replace Methods
if(!isGeneric("m<-")) setGeneric("m<-", function(object, value) standardGeneric("m<-"))
if(!isGeneric("n<-")) setGeneric("n<-", function(object, value) standardGeneric("n<-"))
if(!isGeneric("k<-")) setGeneric("k<-", function(object, value) standardGeneric("k<-"))
setReplaceMethod("m", "HyperParameter", function(object, value){ object@m <- value; object})
setReplaceMethod("n", "HyperParameter", function(object, value){ object@n <- value; object})
setReplaceMethod("k", "HyperParameter", function(object, value){ object@k <- value; object})



validHyperParameter <- function(object){
  if(m(object) < 0)
    stop("m has to be a not negative natural")
  if(n(object) < 0)
    stop("n has to be a not negative natural")
  if(k(object) < 0)
    stop("k has to be a not negative natural")
  
  if(!identical(floor(m(object)), m(object)))
    stop("m has to be a not negative natural")
  if(!identical(floor(n(object)), n(object)))
    stop("n has to be a not negative natural")
  if(!identical(floor(k(object)), k(object)))
    stop("k has to be a not negative natural")
  if(k(object) > m(object) + n(object))
    stop("k has to be less or equal than m + n")    
  else return(TRUE)
}

setValidity("HyperParameter", validHyperParameter)



################################
##
## Class: hypergeometric distribution
##
################################

setClass("Hyper", contains = "DiscreteDistribution")

setMethod("initialize", "Hyper",
          function(.Object, m = 1, n = 1, k = 1) {
            .Object@img <- new("Naturals")
            .Object@param <- new("HyperParameter", m = m, n = n, k = k, name = "Parameter of a hypergeometric distribution" )
            .Object@support <- 0:k
            .Object@r <- function(nn){ rhyper(nn, m = m, n = n, k = k) }
            .Object@d <- function(x, ...){ dhyper(x, m = m, n = n, k = k, ...) }
            .Object@p <- function(p, ...){ phyper(p, m = m, n = n, k = k, ...) }
            .Object@q <- function(q, ...){ qhyper(q, m = m, n = n, k = k, ...) }
            .Object
          })


## wrapped access methods
setMethod("m", "Hyper", function(object) m(param(object)))
setMethod("n", "Hyper", function(object) n(param(object)))
setMethod("k", "Hyper", function(object) k(param(object)))
## wrapped replace methods
setMethod("m<-", "Hyper", function(object, value) new("Hyper", m = value, n = n(object), k = k(object)))
setMethod("n<-", "Hyper", function(object, value) new("Hyper", m = m(object), n = value, k = k(object)))
setMethod("k<-", "Hyper", function(object, value) new("Hyper", m = m(object), n = n(object), k = value))



