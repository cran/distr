################################
##
## Class: BinomParameter
##
################################

setClass("BinomParameter", representation(size = "numeric", prob = "numeric"), contains = "Parameter")

## Access Methods
if(!isGeneric("size")) setGeneric("size", function(object) standardGeneric("size"))
if(!isGeneric("prob")) setGeneric("prob", function(object) standardGeneric("prob"))
setMethod("size", "BinomParameter", function(object) object@size)
setMethod("prob", "BinomParameter", function(object) object@prob)
## Replace Methods
if(!isGeneric("size<-")) setGeneric("size<-", function(object, value) standardGeneric("size<-"))
if(!isGeneric("prob<-")) setGeneric("prob<-", function(object, value) standardGeneric("prob<-"))
setReplaceMethod("size", "BinomParameter", function(object, value){ object@size <- value; object})
setReplaceMethod("prob", "BinomParameter", function(object, value){ object@prob <- value; object})

validBinomParameter <- function(object){
  if(prob(object) < 0)
    stop("prob has to be in [0,1]")
  if(prob(object) > 1)
    stop("prob has to be in [0,1]")
  if(size(object) < 1)
    stop("size has to be a natural greater than 0")
  if(!identical(floor(size(object)), size(object)))
    stop("size has to be a natural greater than 0")    
  else return(TRUE)
}

setValidity("BinomParameter", validBinomParameter)


################################
##
## Class: binomial distribution
##
################################

setClass("Binom", contains = "DiscreteDistribution")

setMethod("initialize", "Binom",
          function(.Object, size = 1,prob = 0.5) {
            .Object@img <- new("Naturals")
            .Object@param <- new("BinomParameter", size = size, prob = prob, name = "Parameter of a binomial distribution")
            .Object@support <- 0:size
            .Object@r <- function(n){ rbinom(n, size = size, prob = prob) }
            .Object@d <- function(x, ...){ dbinom(x, size = size, prob = prob, ...) }
            .Object@p <- function(p, ...){ pbinom(p, size = size, prob = prob, ...) }
            .Object@q <- function(q, ...){ qbinom(q, size = size, prob = prob, ...) }
            .Object
          })

## wrapped access methods
setMethod("prob", "Binom", function(object) prob(param(object)))
setMethod("size", "Binom", function(object) size(param(object)))
## wrapped replace methods
setMethod("prob<-", "Binom", function(object, value) new("Binom", prob = value, size = size(object)))
setMethod("size<-", "Binom", function(object, value) new("Binom", prob = prob(object), size = value))
## Faltung für zwei Binomialverteilungen e1 <- Bin(n1,p1) und e2 <- Bin(n2,p2)
## Fallunterscheidung 
## p1 == p2 und p1 != p2


setMethod("+", c("Binom","Binom"),
          function(e1,e2){
            newsize <- size(e1) + size(e2)
            
            if(is.logical(all.equal(prob(e1),prob(e2))))    
              return(new("Binom", prob = prob(e1), size = newsize))
            
            return(as(e1, "DiscreteDistribution") + e2)
          })

