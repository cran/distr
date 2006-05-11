
################################
##
## Class: NormParameter
##
################################

setClass("NormParameter", representation(mean = "numeric", sd = "vector"), contains = "Parameter")

### intentionally mask sd in order to have an additional ... argument
sd <- function(x, ...){
dots <- list(...)
na.rm <- ifelse(hasArg(na.rm), dots$"na.rm", FALSE) 
stats::sd(x = x, na.rm = na.rm)}



## Access methods
if(!isGeneric("mean")) setGeneric("mean", function(x, ...) standardGeneric("mean"))
if(!isGeneric("sd")) setGeneric("sd", function(x, ...) standardGeneric("sd"))
setMethod("mean", "NormParameter", function(x, ...) x@mean)
setMethod("sd", signature(x = "NormParameter"), function(x, ...) x@sd)
## Replace Methoden
if(!isGeneric("mean<-")) setGeneric("mean<-", function(object, value) standardGeneric("mean<-"))
if(!isGeneric("sd<-")) setGeneric("sd<-", function(object, value) standardGeneric("sd<-"))
setReplaceMethod("mean", "NormParameter", function(object, value){ object@mean <- value; object})
setReplaceMethod("sd", "NormParameter", function(object, value){ object@sd <- as.matrix(value); object})


##validNormParameter <- function(object){
##    if(!is.matrix(sd(object)) && is.numeric(mean(object))) return(TRUE)
##    if(nrow(sd(object)) != ncol((sd(object))))
##        stop("Covariance matrix not sqared")
##    if(nrow(sd(object)) != length(mean(object)))
##        stop("Covariance matrix and mean vector do not have the same dimension")}

##setValidity("NormParameter", validNormParameter)


################################
##
## Class: UniNormParameter
##
################################

setClass("UniNormParameter", contains = "NormParameter")

validUniNormParameter <- function(object){
  if(length(mean(object)) != 1)
    stop("mean has to be a numeric of length 1")    
  if(length(sd(object)) != 1)
    stop("sd has to be a numeric of length 1")    
  sd <- as.numeric(sd(object))
  if(sd <= 0)
    stop("sd has to be positive")
  else return(TRUE)
}

setValidity("UniNormParameter", validUniNormParameter)


################################
##
## Class: normal distribution
##
################################

setClass("Norm",  prototype = prototype(r = function(n){ rnorm(n, mean = 0, sd = 1) },
                                  d = function(x, ...){ dnorm(x, mean = 0, sd = 1, ...) },
                                  p = function(x, ...){ pnorm(x, mean = 0, sd = 1, ...) },
                                  q = function(x, ...){ qnorm(x, mean = 0, sd = 1, ...) },
                                  img = new("Reals"),
                                  param = new("UniNormParameter", mean = 0, sd = 1, name = gettext("Parameter of a univariate normal distribution")),
                                  .withArith = FALSE,
                                  .withSim = FALSE),
      contains = "AbscontDistribution")

## Initialize method
setMethod("initialize", "Norm",
          function(.Object, mean = 0, sd = 1, .withArith = FALSE) {
            .Object@img <- new("Reals")
            .Object@param <- new("UniNormParameter", mean = mean, sd = sd, name = gettext("Parameter of a univariate normal distribution"))
            .Object@r <- function(n){ rnorm(n, mean = meanSub, sd = sdSub) }
            body(.Object@r) <- substitute({ rnorm(n, mean = meanSub, sd = sdSub) },
                                          list(meanSub = mean, sdSub = sd))
            .Object@d <- function(x, ...){ dnorm(x, mean = meanSub, sd = sdSub, ...) }
            body(.Object@d) <- substitute({ dnorm(x, mean = meanSub, sd = sdSub, ...) },
                                          list(meanSub = mean, sdSub = sd))
            .Object@p <- function(x, ...){ pnorm(x, mean = meanSub, sd = sdSub, ...) }
            body(.Object@p) <- substitute({ pnorm(x, mean = meanSub, sd = sdSub, ...) },
                                          list(meanSub = mean, sdSub = sd))
            .Object@q <- function(x, ...){ qnorm(x, mean = meanSub, sd = sdSub, ...) }
            body(.Object@q) <- substitute({ qnorm(x, mean = meanSub, sd = sdSub, ...) },
                                          list(meanSub = mean, sdSub = sd))
            .Object@.withSim   <- FALSE
            .Object@.withArith <- .withArith
            .Object
          })

## wrapped access methods
setMethod("mean", "Norm", function(x, ...) mean(param(x)))
setMethod("sd", signature(x = "Norm"), function(x) sd(param(x)))
## wrapped replace methods 
setMethod("mean<-", "Norm", function(object, value) new("Norm", mean = value, sd = sd(object)))
setMethod("sd<-", "Norm", function(object, value) new("Norm", mean = mean(object), sd = value))

## clipped moments für Normal distribution

###setMethod("m1df", "Norm", 
###          function(object){
###            function(t) -d(object)(t) * sd(param(object))^2 + mean(param(object)) * p(object)(t)
###          })
###
###setMethod("m2df", "Norm", 
###          function(object){
###            mean <- mean(param(object))
###            sd <- sd(param(object))
###            d <- d(object)
###            p <- p(object)
###            function(t) -(t-mean) * d(t) * sd^2 + p(t) * sd^2 - 2 * mean * d(t) * sd^2 + mean^2 * p(t) 
###          })
###
## Faltungsoperator für Normal distributions

setMethod("+", c("Norm","Norm"),
          function(e1,e2){
            new("Norm", sd = sqrt(sd(e1)^2 + sd(e2)^2), mean = mean(e1) + mean(e2),  .withArith = TRUE)
          })

## extra Methoden für Normal distribution
setMethod("+", c("Norm","numeric"),
          function(e1, e2){
            if (length(e2)>1) stop("length of operator must be 1")
            new("Norm", mean = mean(e1) + e2, sd = sd(e1), .withArith = TRUE) 
          })
setMethod("*", c("Norm","numeric"),
          function(e1, e2){
            if (length(e2)>1) stop("length of operator must be 1")
            if(e2 == 0) return(new("Dirac", location = 0, .withArith = TRUE))
            new("Norm", mean = mean(e1) * e2, sd = sd(e1) * abs(e2), .withArith = TRUE)
          })
