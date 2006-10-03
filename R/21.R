################################
##
## Class: Distribution
##
################################

setClass("Distribution", representation(img = "rSpace",
                                        param = "OptionalParameter",
                                        r = "function",
                                        d = "OptionalFunction",
                                        p = "OptionalFunction",
                                        q = "OptionalFunction", # extended by P.R. 28-03-06
                                        .withSim = "logical",   ## 'internal' slots => no accessors/replacement functions
                                        .withArith = "logical"
                                        ))

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
## Class: UnivariateDistribution
##
################################

setClass("UnivariateDistribution", representation(),
          prototype= prototype(r = function(n){ rnorm(n, mean = 0, sd = 1) },
                     d = function(x, ...){ dnorm(x, mean = 0, sd = 1, ...) },
                     p = function(x, ...){ pnorm(x, mean = 0, sd = 1, ...) },
                     q = function(x, ...){ qnorm(x, mean = 0, sd = 1, ...) },
                     img = new("Reals"),
                     param = NULL,
                     .withArith = FALSE,
                     .withSim = FALSE), contains = "Distribution")

###produces difficulties in coercing...:
#
#setMethod("initialize", "UnivariateDistribution",
#          function(.Object, r = NULL, d = NULL, p = NULL, q = NULL, 
#                    param = NULL, img = new("Reals"),
#                    .withSim = FALSE, .withArith = FALSE) {
#            if(is.null(r)) {
#              stop("You have at least to give the slot r.")
#              return(invisible())}
#            ### Attention: no checking!!!
#            .Object@img <- img
#            .Object@param <- param
#            .Object@d <- d
#            .Object@p <- p
#            .Object@q <- q
#           .Object@r <- r
#            .Object@.withSim <- .withSim
#            .Object@.withArith <- .withArith
#            .Object })

### new 03.10.06:
## dim is already generic
#if(!isGeneric("dim")){ 
#    setGeneric("dim", function(x, ...) standardGeneric("dim"))
#}
setMethod("dim", "UnivariateDistribution", function(x)1)


setMethod("print", "UnivariateDistribution",
          function(x, ...){
            cat(gettextf("Distribution Object of Class: %s\n", class(x)[1]))
            if(x@.withArith && getdistrOption("WarningArith")) 
            {msga <- gettext("arithmetics on distributions are understood as operations on r.v.'s\n")
             msgb <- gettext("see 'distrARITH()'; for switching off this warning see '?distroptions'")
             warning(msga,msgb)}
            if(x@.withSim && getdistrOption("WarningSim")) 
            {msga <- gettext("slots d,p,q have been filled using simulations; for switching off this warning see '?distroptions'")
             warning(msga)}
            parameter = param(x)
            Names = slotNames(parameter)
            if(length(Names) > 1){
              for(i in Names[Names != "name"])
                cat(i, ": ", slot(parameter, i), "\n")
            }
          })

setMethod("show", "UnivariateDistribution",
          function(object)print(object))
