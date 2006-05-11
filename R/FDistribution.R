
################################
##
## Class: FParameter
##
################################

setClass("FParameter", representation(df1 = "numeric", df2 = "numeric", ncp = "numeric"), 
          contains = "Parameter")

## Access Methods
if(!isGeneric("df1")) setGeneric("df1", function(object) standardGeneric("df1"))
if(!isGeneric("df2")) setGeneric("df2", function(object) standardGeneric("df2"))
if(!isGeneric("ncp")) setGeneric("ncp", function(object) standardGeneric("ncp"))
setMethod("df1", "FParameter", function(object) object@df1)
setMethod("df2", "FParameter", function(object) object@df2)
setMethod("ncp", "FParameter", function(object) object@ncp)
## Replace Methods
if(!isGeneric("df1<-")) setGeneric("df1<-", function(object, value) standardGeneric("df1<-"))
if(!isGeneric("df2<-")) setGeneric("df2<-", function(object, value) standardGeneric("df2<-"))
if(!isGeneric("ncp<-")) setGeneric("ncp<-", function(object, value) standardGeneric("ncp<-"))
setReplaceMethod("df1", "FParameter", function(object, value){ object@df1 <- value; object})
setReplaceMethod("df2", "FParameter", function(object, value){ object@df2 <- value; object})
setReplaceMethod("ncp", "FParameter", function(object, value){ object@ncp <- value; object})

validFParameter <- function(object){
  if(length(df1(object)) != 1)
    stop("df1 has to be a numeric of length 1")    
  if(df1(object) <= 0)
    stop("df1 has to be positive")
  if(length(df2(object)) != 1)
    stop("df2 has to be a numeric of length 1")    
  if(df2(object) <= 0)
    stop("df2 has to be positive")
  if(length(ncp(object)) != 1)
    stop("ncp has to be a numeric of length 1")      
  else return(TRUE)
}

setValidity("FParameter", validFParameter)

################################
##
## Class: F distribution
##
################################

setClass("Fd",  prototype = prototype(r = function(n){ rf(n,  df1 = 1, df2 = 1, ncp = 0) },
                                  d = function(x, ...){ df(x, df1 = 1, df2 = 1, ncp = 0, ...) },
                                  p = function(x, ...){ pf(x, df1 = 1, df2 = 1, ncp = 0, ...) },
                                  q = function(x, ...){ qf(x, df1 = 1, df2 = 1, ncp = 0, ...) },
                                  img = new("Reals"),
                                  param = new("FParameter", df1 = 1, df2=1, ncp=0,
                                          name = gettext("Parameter of a F distribution")),
                                  .withArith = FALSE,
                                  .withSim = FALSE),
      contains = "AbscontDistribution")

## Initialize method
setMethod("initialize", "Fd",
          function(.Object, df1 = 1, df2 = 1, ncp = 0) {
            .Object@img <- new("Reals")
            .Object@param <- new("FParameter", df1 = df1, df2 = df2, ncp = ncp,
                                  name = gettext("Parameter of a F distribution"))
            .Object@r <- function(n){ rf(n, df1 = df1Sub, df2 = df2Sub, ncp = ncpSub) }
            body(.Object@r) <- substitute({ rf(n, df1 = df1Sub, df2 = df2Sub, ncp = ncpSub) },
                                          list(df1Sub = df1, df2Sub = df2, ncpSub = ncp))
            
            #### will probably change.... (when df for ncp!=0 available...)
            if(isTRUE(all.equal(ncp,0)))
               {df.0<- function(x, df1, df2, ncp = 0, log = FALSE)
                       {stats::df(x, df1, df2, log)} 
               }
            else  
               {### later perhaps with sfsmisc:
                  TQ <- getdistrOption("TruncQuantile")/2
                  xz <- qf(1-TQ, df1 = df1, df2 = df2, ncp = ncp)
                  xl <- c(0,xz)
                  pfun <- function(x){pf(x, df1=df1, df2=df2, ncp=ncp)}
                  dfun <- P2D(pfun, xl, ngrid = getdistrOption("DefaultNrGridPoints"))
                # by means of simulations
                # rfun <- function(n){rf(n, df1=df1, df2=df2, ncp=ncp)}
                # dfun <-R2D(rfun, nsim = 10^getdistrOption("RtoDPQ.e"), 
                #           n = getdistrOption("DefaultNrGridPoints"))
                df.0 <- function(x, df1, df2, ncp = 0, log = FALSE)
                        {dfun(x)}
               }                  
            .Object@d <- function(x, ...){ df.0(x, df1 = df1Sub, df2 = df2Sub, ncp = ncpSub) }
            body(.Object@d) <- substitute({ df.0(x, df1 = df1Sub, df2 = df2Sub, ncp = ncpSub) },
                                           list(df1Sub = df1, df2Sub = df2, ncpSub = ncp))
            .Object@p <- function(x, ...){ pf(x, df1 = df1Sub, df2 = df2Sub, ncp = ncpSub, ...) }
            body(.Object@p) <- substitute({ pf(x, df1 = df1Sub, df2 = df2Sub, ncp = ncpSub, ...) },
                                          list(df1Sub = df1, df2Sub = df2, ncpSub = ncp))
            .Object@q <- function(x, ...){ qf(x, df1 = df1Sub, df2 = df2Sub, ncp = ncpSub, ...) }
            body(.Object@q) <- substitute({ qf(x, df1 = df1Sub, df2 = df2Sub, ncp = ncpSub, ...) },
                                          list(df1Sub = df1, df2Sub = df2, ncpSub = ncp))
            .Object@.withSim   <- FALSE
            .Object@.withArith <- FALSE
            .Object
          })

## wrapped access methods
setMethod("df1", "Fd", function(object) df1(param(object)))
setMethod("df2", "Fd", function(object) df2(param(object)))
setMethod("ncp", "Fd", function(object) ncp(param(object)))

## wrapped replace methods
setMethod("df1<-", "Fd", function(object, value) new("Fd", df1 = value, df2 = df2(object), ncp=ncp(object)))
setMethod("df2<-", "Fd", function(object, value) new("Fd", df1 = df1(object), df2 = value, ncp=ncp(object)))
setMethod("ncp<-", "Fd", function(object, value) new("Fd", df1 = df1(object), df2 = df2(object), ncp=value))
