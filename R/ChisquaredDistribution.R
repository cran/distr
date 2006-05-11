
################################
##
## Klasse: ChisqParameter
##
################################

setClass("ChisqParameter", representation(df = "numeric", ncp = "numeric"), contains = "Parameter")

## Access Methods
if(!isGeneric("df")) setGeneric("df", function(x, df1, df2, log = FALSE) standardGeneric("df"))
if(!isGeneric("ncp")) setGeneric("ncp", function(object) standardGeneric("ncp"))
setMethod("df", "ChisqParameter", function(x, df1, df2, log = FALSE) x@df)
setMethod("ncp", "ChisqParameter", function(object) object@ncp)
## Replace Methods
if(!isGeneric("df<-")) setGeneric("df<-", function(object, value) standardGeneric("df<-"))
if(!isGeneric("ncp<-")) setGeneric("ncp<-", function(object, value) standardGeneric("ncp<-"))
setReplaceMethod("df", "ChisqParameter", function(object, value){ object@df <- value; object})
setReplaceMethod("ncp", "ChisqParameter", function(object, value){ object@ncp <- value; object})

validChisqParameter <- function(object){
  if(length(df(object)) != 1)
    stop("df has to be a numeric of length 1")    
  if(df(object) <= 0)
    stop("df has to be positive")
  if(length(ncp(object)) != 1)
    stop("ncp has to be a numeric of length 1")    
  if(ncp(object) < 0)
    stop("ncp has to be not negative")
  else return(TRUE)
}

setValidity("ChisqParameter", validChisqParameter)


################################
##
## Klasse: Chi squared distribution
##
################################

##
setClass("Chisq", prototype = prototype(r = function(n){ rchisq(n, df = 1, ncp = 0) },
                                  d = function(x, ...){ dchisq(x, df = 1, ncp = 0, ...) },
                                  p = function(x, ...){ pchisq(x, df = 1, ncp = 0, ...) },
                                  q = function(x, ...){ qchisq(x, df = 1, ncp = 0, ...) },
                                  img = new("Reals"),
                                  param = new("ChisqParameter", df = 1, ncp = 0, 
                                               name = gettext("Parameter of a chi squared distribution")),
                                  .withArith = FALSE,
                                  .withSim = FALSE),
         contains = "ExpOrGammaOrChisq")

## Initialize method
setMethod("initialize", "Chisq",
          function(.Object, df = 1, ncp = 0, .withArith = FALSE) {
            .Object@img <- new("Reals")
            .Object@param <- new("ChisqParameter", df = df, ncp = ncp, 
               name = gettext("Parameter of a chi squared distribution") )
            .Object@r <- function(n){ rchisq(n, df = dfSub, ncp = ncpSub) }
            body(.Object@r) <- substitute({ rchisq(n, df = dfSub, ncp = ncpSub) },
                                          list(dfSub = df, ncpSub = ncp))
            .Object@d <- function(x, ...){ dchisq(x, df = dfSub, ncp = ncpSub, ...) }
            body(.Object@d) <- substitute({ dchisq(x, df = dfSub, ncp = ncpSub, ...) },
                                          list(dfSub = df, ncpSub = ncp))
            .Object@p <- function(x, ...){ pchisq(x, df = dfSub, ncp = ncpSub, ...) }
            body(.Object@p) <- substitute({ pchisq(x, df = dfSub, ncp = ncpSub, ...) },
                                          list(dfSub = df, ncpSub = ncp))
            .Object@q <- function(x, ...){ qchisq(x, df = dfSub, ncp = ncpSub, ...) }
            body(.Object@q) <- substitute({ qchisq(x, df = dfSub, ncp = ncpSub, ...) },
                                          list(dfSub = df, ncpSub = ncp))
            .Object@.withSim   <- FALSE
            .Object@.withArith <- .withArith
            .Object
          })

## wrapped access methods
setMethod("df", "Chisq", function(x, df1, df2, log = FALSE) df(param(x)))
setMethod("ncp", "Chisq", function(object) ncp(param(object)))

## wrapped replace methods
setMethod("df<-", "Chisq", function(object, value) new("Chisq", df = value, ncp = ncp(object)))
setMethod("ncp<-", "Chisq", function(object, value) new("Chisq", df = df(object), ncp = value))

setMethod("+", c("Chisq","Chisq"),
          function(e1,e2){
            newdf <- df(e1) + df(e2)
            newncp <- ncp(e1) + ncp(e2)
            return(new("Chisq", df = newdf, ncp = newncp, .withArith = TRUE))
          })

setIs("Chisq", "Gammad", test = function(obj) {identical(all.equal(ncp(obj),0) , TRUE)},
       coerce = function(obj) {new("Gammad", shape = df(obj)/2, scale = 2)},
      replace = function(obj, value) {new("Gammad", shape = value@shape, scale = value@scale)}) 
   ## if ncp == 0 a Gamma distribution with shape = df(obj)/2 and scale = 2
#setAs(from = "Chisq", to = "Gammad", 
#      def = function(from) { if(identical(all.equal(ncp(from),0) , TRUE))
#                               new("Gammad", shape = df(from)/2, scale = 2)
#                             else stop("a Chisq object x can be coerced to a Gammad object only if ncp(x)=0")  },
#      replace = function(from, value) {if(identical(all.equal(ncp(from),0) , TRUE))
#                                         new("Gammad", shape = value@shape, scale = value@scale)
#                                       else stop("a Chisq object x can be coerced to a Gammad object only if ncp(x)=0")  }
#      ) 
