################################
##
## virtual Class: Distribution
##
################################

setClass("Distribution", representation(img = "rSpace",
                                        param = "Parameter",
                                        r = "function",
                                        d = "function",
                                        p = "function",
                                        q = "function"),
         contains = "VIRTUAL")

## Access Methoden
if(!isGeneric("img")) setGeneric("img", function(object) standardGeneric("img"))
if(!isGeneric("param")) setGeneric("param", function(object) standardGeneric("param"))
if(!isGeneric("r")) setGeneric("r", function(object) standardGeneric("r"))
if(!isGeneric("d")) setGeneric("d", function(object) standardGeneric("d"))
if(!isGeneric("p")) setGeneric("p", function(object) standardGeneric("p"))
if(!isGeneric("q")) setGeneric("q", function(save = "default", status = 0, runLast = TRUE) standardGeneric("q"))
setMethod("img", "Distribution", function(object) object@img)
setMethod("param", "Distribution", function(object) object@param)
setMethod("r", "Distribution", function(object) object@r)
setMethod("d", "Distribution", function(object) object@d)
setMethod("p", "Distribution", function(object) object@p)
setMethod("q", "Distribution", function(save = "default", status = 0, runLast = TRUE) save@q)





################################
##
## Class: UnivariateParameter
##
################################

setClass("UnivariateParameter", representation(), contains = c("Parameter", "VIRTUAL"))


################################
##
## Class: UnivariateDistribution
##
################################

setClass("UnivariateDistribution", representation(),
         contains = c("Distribution", "VIRTUAL"))



setMethod("print", "UnivariateDistribution",
          function(x, ...){
            cat("Distribution Object of Class: ", class(x)[1], "\n")
            parameter = param(x)
            Names = slotNames(parameter)
            if(length(Names) > 1){
              for(i in Names[Names != "name"])
                cat(i, ": ", slot(parameter, i), "\n")
            }
          })
