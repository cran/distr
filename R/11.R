################################
##
## virtual Class: Parameter
##
################################

setClass("Parameter", representation(name = "character"), contains = "VIRTUAL")

## Access Methoden
if(!isGeneric("name")) setGeneric("name", function(object) standardGeneric("name"))
setMethod("name", "Parameter", function(object) object@name)
## Replace Methoden
if(!isGeneric("name<-")) setGeneric("name<-", function(object, value) standardGeneric("name<-"))
setReplaceMethod("name", "Parameter", function(object, value){ object@name <- value; object})
