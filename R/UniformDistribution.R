
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

setClass("Unif", contains = "AbscontDistribution")

## Initialize method
setMethod("initialize", "Unif",
          function(.Object, Min = 0, Max = 1) {
            .Object@img <- new("Reals")
            .Object@param <- new("UnifParameter", Min = Min, Max = Max, name = "Parameter of a uniform distribution")
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
            .Object
          })

## wrapped access methods
setMethod("Min", "Unif", function(object) Min(param(object)))
setMethod("Max", "Unif", function(object) Max(param(object)))

## wrapped replace methods
setMethod("Min<-", "Unif", function(object, value) new("Unif", Min = value, Max = Max(object)))
setMethod("Max<-", "Unif", function(object, value) new("Unif", Min = Min(object), Max = value))
