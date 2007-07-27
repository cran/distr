################################
##
## Class: Distribution
##
################################

## Access Methods
setMethod("img", "Distribution", function(object) object@img)
setMethod("param", "Distribution", function(object) object@param)
setMethod("r", "Distribution", function(object) object@r)
setMethod("d", "Distribution", function(object) object@d)
setMethod("p", "Distribution", function(object) object@p)
setMethod("q", "Distribution", 
           function(save = "default", status = 0, runLast = TRUE) save@q)
           ### odd arg-list due to existing function in base package 


################################
##
## Class: UnivariateDistribution
##
################################


setMethod("dim", "UnivariateDistribution", function(x)1)


