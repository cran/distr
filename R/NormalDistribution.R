
################################
##
## Class: NormParameter
##
################################

setClass("NormParameter", representation(mean = "numeric", sd = "vector"), contains = "Parameter")


## Access methods
if(!isGeneric("mean")) setGeneric("mean", function(x, ...) standardGeneric("mean"))
if(!isGeneric("sd")) setGeneric("sd", function(x, na.rm = FALSE) standardGeneric("sd"))
setMethod("mean", "NormParameter", function(x, ...) x@mean)
setMethod("sd", "NormParameter", function(x, na.rm = FALSE) x@sd)
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

setClass("Norm", contains = "AbscontDistribution")

## Initialize method
setMethod("initialize", "Norm",
          function(.Object, mean = 0, sd = 1) {
            .Object@img <- new("Reals")
            .Object@param <- new("UniNormParameter", mean = mean, sd = sd, name = "Parameter of a univariate normal distribution")
            .Object@r <- function(n){ rnorm(n, mean = mean, sd = sd) }
            .Object@d <- function(x, ...){ dnorm(x, mean = mean, sd = sd, ...) }
            .Object@p <- function(x, ...){ pnorm(x, mean = mean, sd = sd, ...) }
            .Object@q <- function(x, ...){ qnorm(x, mean = mean, sd = sd, ...) }
            .Object
          })

## wrapped access methods
setMethod("mean", "Norm", function(x, ...) mean(param(x)))
setMethod("sd", "Norm", function(x, na.rm = FALSE) sd(param(x)))
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
            new("Norm", sd = sqrt(sd(e1)^2 + sd(e2)^2), mean = mean(e1) + mean(e2))
          })

## extra Methoden für Normal distribution
setMethod("+", c("Norm","numeric"),
          function(e1, e2){
            new("Norm", mean = mean(e1) + e2, sd = sd(e1)) 
          })
setMethod("*", c("Norm","numeric"),
          function(e1, e2){
            if(e2 == 0) return(new("Dirac", location = 0))
            new("Norm", mean = mean(e1) * e2, sd = sd(e1) * abs(e2))
          })


