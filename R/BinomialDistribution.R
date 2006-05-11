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
  if(length(prob(object)) != 1)
    stop("prob has to be a numeric of length 1")    
  if(prob(object) < 0)
    stop("prob has to be in [0,1]")
  if(prob(object) > 1)
    stop("prob has to be in [0,1]")
  if(length(size(object)) != 1)
    stop("size has to be a numeric of length 1")    
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

setClass("Binom", prototype = prototype(r = function(n){ rbinom(n, size = 1,prob = 0.5) },
                                  d = function(x, ...){ dbinom(x, size = 1,prob = 0.5, ...) },
                                  p = function(x, ...){ pbinom(x, size = 1,prob = 0.5, ...) },
                                  q = function(x, ...){ qbinom(x, size = 1,prob = 0.5, ...) },
                                  img = new("Naturals"),
                                  param = new("BinomParameter", size = 1, prob = 0.5, 
                                               name = gettext("Parameter of a binomial distribution")),
                                  .withArith = FALSE,
                                  .withSim = FALSE),
          contains = "DiscreteDistribution")

setMethod("initialize", "Binom",
          function(.Object, size = 1,prob = 0.5, .withArith = FALSE) {
            .Object@img <- new("Naturals")
            .Object@param <- new("BinomParameter", size = size, prob = prob, 
                name = gettext("Parameter of a binomial distribution"))
            .Object@support <- 0:size
            .Object@r <- function(n){ rbinom(n, size = sizeSub, prob = probSub) }
            body(.Object@r) <- substitute({ rbinom(n, size = sizeSub, prob = probSub) },
                                          list(sizeSub = size, probSub = prob))
            .Object@d <- function(x, ...){ dbinom(x, size = sizeSub, prob = probSub, ...) }
            body(.Object@d) <- substitute({ dbinom(x, size = sizeSub, prob = probSub, ...) },
                                          list(sizeSub = size, probSub = prob))
            .Object@p <- function(p, ...){ pbinom(p, size = sizeSub, prob = probSub, ...) }
            body(.Object@p) <- substitute({ pbinom(p, size = sizeSub, prob = probSub, ...) },
                                          list(sizeSub = size, probSub = prob))
            .Object@q <- function(q, ...){ qbinom(q, size = sizeSub, prob = probSub, ...) }
            body(.Object@q) <- substitute({ qbinom(q, size = sizeSub, prob = probSub, ...) },
                                          list(sizeSub = size, probSub = prob))
            .Object@.withSim   <- FALSE
            .Object@.withArith <- .withArith
            .Object
          })

## wrapped access methods
setMethod("prob", "Binom", function(object) prob(param(object)))
setMethod("size", "Binom", function(object) size(param(object)))
## wrapped replace methods
setMethod("prob<-", "Binom", function(object, value) new("Binom", prob = value, size = size(object)))
setMethod("size<-", "Binom", function(object, value) new("Binom", prob = prob(object), size = value))
## Convolution for two binomial distributions e1 <- Bin(n1,p1) and e2 <- Bin(n2,p2)
## Distinguish cases 
## p1 == p2 und p1 != p2


setMethod("+", c("Binom","Binom"),
          function(e1,e2){
            newsize <- size(e1) + size(e2)
            
            if(is.logical(all.equal(prob(e1),prob(e2))))    
              return(new("Binom", prob = prob(e1), size = newsize, .withArith = TRUE))
            
            return(as(e1, "DiscreteDistribution") + e2)
          })
