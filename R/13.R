################################
##
## virtal Class: rSpace
##
################################

setClass("rSpace", representation(name = "character"), contains = "VIRTUAL")

## Access methods
if(!isGeneric("name")) setGeneric("name", function(object) standardGeneric("name"))
setMethod("name", "rSpace", function(object) object@name)
## Replace methods
if(!isGeneric("name<-")) setGeneric("name<-", function(object, value) standardGeneric("name<-"))
setReplaceMethod("name", "rSpace", function(object, value){ object@name <- value; object})

################################
##
## Class: EuclideanSpace
##
################################

setClass("EuclideanSpace", representation(dimension = "numeric"), contains = "rSpace",
         prototype = c(dimension = 1, name = "Euclidean Space"))

## Access methods
if(!isGeneric("dimension")) setGeneric("dimension", function(object) standardGeneric("dimension"))
setMethod("dimension", "EuclideanSpace", function(object) object@dimension)
## Replace methods
if(!isGeneric("dimension<-")) setGeneric("dimension<-", function(object, value) standardGeneric("dimension<-"))
setReplaceMethod("dimension", "EuclideanSpace", function(object, value){ object@dimension <- value; object})

## Initialize method
setMethod("initialize", "EuclideanSpace",
          function(.Object, dimension = 1) {
            .Object@dimension <-  dimension
            .Object@name <- "Euclidean Space"
            validObject(.Object)
            .Object
          })

validEuclideanSpace <- function(object){
  if(dimension(object) < 1)
    stop("dimension has to be a natural greater than 0")
  if(!identical(floor(dimension(object)), dimension(object)))
    stop("dimension has to be a natural greater than 0")    
}

setValidity("EuclideanSpace", validEuclideanSpace)



## extra methods
if(!isGeneric("liesIn")) setGeneric("liesIn", function(object, x) standardGeneric("liesIn"))

setMethod("liesIn", signature(object = "EuclideanSpace", x = "numeric"), 
          function(object, x){
            if(dimension(object) ==  length(x))
              return(TRUE)
            return(FALSE)
          })

################################
##
## Class: Reals
##
################################

setClass("Reals",  contains = "EuclideanSpace")
setMethod("initialize", "Reals",
          function(.Object) {
            .Object@dimension <-  1
            .Object@name <- "Real Space"
            .Object
          })


################################
##
## Class: Naturals
##
################################

setClass("Naturals",  contains = "Reals")
setMethod("initialize", "Naturals",
          function(.Object) {
            .Object@dimension <-  1
            .Object@name <- "Natural Space"
            .Object
          })


setMethod("liesIn", signature(object = "Naturals", x = "numeric"), 
          function(object, x){
            if(length(x) !=  1)
              return(FALSE)
            if(x <= 0)
              return(FALSE)
            if(!identical(floor(x), x))
              return(FALSE)
            else return(TRUE)
          })

