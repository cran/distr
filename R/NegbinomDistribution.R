################################
##
## Class: NbinomParameter
##
################################

setClass("NbinomParameter", representation(size = "numeric", prob = "numeric"), contains = "Parameter")

## Access Methods
if(!isGeneric("size")) setGeneric("size", function(object) standardGeneric("size"))
if(!isGeneric("prob")) setGeneric("prob", function(object) standardGeneric("prob"))
setMethod("size", "NbinomParameter", function(object) object@size)
setMethod("prob", "NbinomParameter", function(object) object@prob)
## Replace Methods
if(!isGeneric("size<-")) setGeneric("size<-", function(object, value) standardGeneric("size<-"))
if(!isGeneric("prob<-")) setGeneric("prob<-", function(object, value) standardGeneric("prob<-"))
setReplaceMethod("size", "NbinomParameter", function(object, value){ object@size <- value; object})
setReplaceMethod("prob", "NbinomParameter", function(object, value){ object@prob <- value; object})

validNbinomParameter <- function(object){
  if(prob(object) <= 0)
    stop("prob has to be in (0,1)")
  if(prob(object) >= 1)
    stop("prob has to be in (0,1)")
  if(size(object) < 0)
    stop("size has to be a not negative natural")
  if(!identical(floor(size(object)), size(object)))
    stop("size has to be a not negative natural")
  else return(TRUE)
}

setValidity("NbinomParameter", validNbinomParameter)


################################
##
## Class: negative binomial distribution
##
################################

setClass("Nbinom", contains = "DiscreteDistribution")

setMethod("initialize", "Nbinom",
          function(.Object, size = 1,prob = 0.5) {
            .Object@img <- new("Naturals")
            .Object@param <- new("NbinomParameter", size = size, prob = prob, name = "Parameter of a negative binomial distribution" )
            .Object@support <- 0:qnbinom(1 - TruncQuantile, size = size, prob = prob)
            .Object@r <- function(n){ rnbinom(n, size = size, prob = prob) }
            .Object@d <- function(x, ...){ dnbinom(x, size = size, prob = prob, ...) }
            .Object@p <- function(p, ...){ pnbinom(p, size = size, prob = prob, ...) }
            .Object@q <- function(q, ...){ qnbinom(q, size = size, prob = prob, ...) }
            .Object
          })

## wrapped access methods
setMethod("prob", "Nbinom", function(object) prob(param(object)))
setMethod("size", "Nbinom", function(object) size(param(object)))
## wrapped replace methods
setMethod("prob<-", "Nbinom", function(object, value) new("Nbinom", prob = value, size = size(object)))
setMethod("size<-", "Nbinom", function(object, value) new("Nbinom", prob = prob(object), size = value))

setMethod("+", c("Nbinom","Nbinom"),
          function(e1,e2){
            newsize <- size(e1) + size(e2)
            
            if(is.logical(all.equal(prob(e1),prob(e2))))    
              return(new("Nbinom", size = newsize, prob = prob(e1)))
            
            return(as(e1, "DiscreteDistribution") + e2)
          })
