################################
##
## Class: UnifParameter
##
################################

setClass("UnifParameter", representation(Min = "numeric", Max = "numeric"), contains = "Parameter")

## Access Methods
if(!isGeneric("Min")) setGeneric("Min", function(object) standardGeneric("Min"))
if(!isGeneric("Max")) setGeneric("Max", function(object) standardGeneric("Max"))
setMethod("Min", "UnifParameter", function(object) object@Min)
setMethod("Max", "UnifParameter", function(object) object@Max)
## Replace Methods
if(!isGeneric("Min<-")) setGeneric("Min<-", function(object, value) standardGeneric("Min<-"))
if(!isGeneric("Max<-")) setGeneric("Max<-", function(object, value) standardGeneric("Max<-"))
setReplaceMethod("Min", "UnifParameter", function(object, value){ object@Min <- value; object})
setReplaceMethod("Max", "UnifParameter", function(object, value){ object@Max <- value; object})


validUnifParameter <- function(object){
  if(length(Min(object)) != 1)
    stop("Min has to be a numeric of length 1")    
  if(length(Max(object)) != 1)
    stop("Max has to be a numeric of length 1")    
  if(Min(object) >= Max(object))
    stop("Min has to be less than Max")
  else return(TRUE)
}

setValidity("UnifParameter", validUnifParameter)




################################
##
## Class: uniform distribution
##
################################

setClass("Unif",  prototype = prototype(r = function(n){ runif(n, min = 0, max = 1) },
                                  d = function(x, ...){ dunif(x,  min = 0, max = 1, ...) },
                                  p = function(x, ...){ punif(x,  min = 0, max = 1, ...) },
                                  q = function(x, ...){ qunif(x,  min = 0, max = 1, ...) },
                                  img = new("Reals"),
                                  param = new("UnifParameter", Min = 0, Max = 1, name = gettext("Parameter of a uniform distribution")),
                                  .withArith = FALSE,
                                  .withSim = FALSE),
        contains = "AbscontDistribution")

## Initialize method
setMethod("initialize", "Unif",
          function(.Object, Min = 0, Max = 1, .withArith = FALSE) {
            .Object@img <- new("Reals")
            .Object@param <- new("UnifParameter", Min = Min, Max = Max, name = gettext("Parameter of a uniform distribution"))
            .Object@r <- function(n){ runif(n, min = Min, max = Max) } 
            body(.Object@r) <- substitute({ runif(n, min = MinSub, max = MaxSub) },
                                          list(MinSub = Min, MaxSub = Max))
            .Object@d <- function(x, ...){ dunif(x, min = Min, max = Max, ...) }
            body(.Object@d) <- substitute({ dunif(x, min = MinSub, max = MaxSub) },
                                          list(MinSub = Min, MaxSub = Max))            
            .Object@p <- function(x, ...){ punif(x, min = Min, max = Max, ...) }
            body(.Object@p) <- substitute({ punif(x, min = MinSub, max = MaxSub) },
                                          list(MinSub = Min, MaxSub = Max))        
            .Object@q <- function(x, ...){ qunif(x, min = Min, max = Max, ...) }
            body(.Object@q) <- substitute({ qunif(x, min = MinSub, max = MaxSub) },
                                          list(MinSub = Min, MaxSub = Max))                    
            .Object@.withSim   <- FALSE
            .Object@.withArith <- .withArith
            .Object
          })

## wrapped access methods
setMethod("Min", "Unif", function(object) Min(param(object)))
setMethod("Max", "Unif", function(object) Max(param(object)))

## wrapped replace methods
setMethod("Min<-", "Unif", function(object, value) new("Unif", Min = value, Max = Max(object)))
setMethod("Max<-", "Unif", function(object, value) new("Unif", Min = Min(object), Max = value))

## extra methods for Unif distribution
setMethod("+", c("Unif","numeric"),
          function(e1, e2){
            if (length(e2)>1) stop("length of operator must be 1")
            new("Unif", Min = Min(e1) + e2, Max = Max(e1) + e2, .withArith = TRUE) 
          })
setMethod("*", c("Unif","numeric"),
          function(e1, e2){
            if (length(e2)>1) stop("length of operator must be 1")
            if(e2 == 0) return(new("Dirac", location = 0, .withArith = TRUE))
            if(e2 > 0) 
              new("Unif", Min = Min(e1) * e2, Max = Max(e1) * e2, .withArith = TRUE)
            else
              new("Unif", Min = Max(e1) * e2, Max = Min(e1) * e2, .withArith = TRUE)
          })

setIs("Unif", "Beta", test = function(obj) {identical(all.equal(Min(obj),0),TRUE)&&identical(all.equal(Max(obj),1), TRUE)}, 
       coerce = function(obj) {new("Beta", shape1 = 1, shape2 = 1)},
       replace = function(obj, value) {new("Beta", shape1 = value@shape1, shape2 = value@shape2, ncp = value@ncp)}) 
       ## if Min==0 and Max==1 a Beta Distribution with Parameters shape1 = 1, shape2 = 2
#setAs(from = "Unif", to = "Beta", 
#      def = function(from) {if(identical(all.equal(Min(from),0),TRUE)&&
#                               identical(all.equal(Max(from),1), TRUE))
#                                   new("Beta", shape1 = 1, shape2 = 1  )
#                            else stop("only a Unif(0,1) object can be coerced to a Beta object")},
#      replace = function(from, value) {if(identical(all.equal(Min(from),0),TRUE)&&
#                                          identical(all.equal(Max(from),1), TRUE))
#                                          new("Beta", shape1 = value@shape1, shape2 = value@shape2, ncp = value@ncp)
#                                       else stop("only a Unif(0,1) object can be coerced to a Beta object")   }
#      ) 
