
################################
##
## Class: Laplace or Double Exponential distribution
##
################################

setClass("DExp",  prototype = prototype(r = function(n){ (2*rbinom(n,size=1,prob=0.5)-1)*rexp(n, rate = 1) },
                                  d = function(x, ...){ 0.5*dexp(abs(x), rate = 1, ...) },
                                  p = function(x, ...){ ifelse(x<=0, 
                                                          0.5 - 0.5*pexp( -x, rate = 1,...),
                                                          0.5 + 0.5*pexp(x, rate = 1, ...)) },
                                  q = function(x, ...){ ifelse(x<=0.5,
                                                          -qexp(1 - 2*x, rate = 1, ...),
                                                           qexp(2*x - 1, rate = 1, ...)) },
                                  img = new("Reals"),
                                  param = new("ExpParameter", rate = 1, name = gettext("Parameter of a exponential distribution")),
                                  .withArith = FALSE,
                                  .withSim = FALSE),
      contains = "AbscontDistribution")

## Initialize method
setMethod("initialize", "DExp",
          function(.Object, rate = 1, .withArith = FALSE) {
            .Object@img <- new("Reals")
            .Object@param <- new("ExpParameter", rate = rate, name = gettext("Parameter of a exponential distribution"))
            .Object@r <- function(n){ (2*rbinom(n,size=1,prob=0.5)-1)*rexp(n, rate = rateSub) }
            body(.Object@r) <- substitute({ (2*rbinom(n,size=1,prob=0.5)-1)*rexp(n, rate = rateSub) },
                                          list(rateSub = rate))
            .Object@d <- function(x, ...){ 0.5*dexp(abs(x), rate = rateSub, ...) }
            body(.Object@d) <- substitute({ 0.5*dexp(abs(x), rate = rateSub, ...) },
                                          list(rateSub = rate))
            .Object@p <- function(x, ...){ ifelse(x<=0, 
                                                  0.5 - 0.5*pexp(-x,rate = rateSub,...),
                                                  0.5 + 0.5*pexp(x, rate = rateSub, ...)) }
            body(.Object@p) <- substitute({ ifelse(x<=0, 
                                                  0.5 - 0.5*pexp(-x,rate = rateSub,...),
                                                  0.5 + 0.5*pexp(x, rate = rateSub, ...)) },
                                          list(rateSub = rate))
            .Object@q <- function(x, ...){ ifelse(x<=0.5,
                                                  -qexp(1 - 2*x, rate = rateSub, ...),
                                                   qexp(2*x - 1, rate = rateSub, ...)) }
            body(.Object@q) <- substitute({ ifelse(x<=0.5,
                                                  -qexp(1 - 2*x, rate = rateSub, ...),
                                                   qexp(2*x - 1, rate = rateSub, ...)) },
                                          list(rateSub = rate))
            .Object@.withSim   <- FALSE
            .Object@.withArith <- .withArith
            .Object
          })

## wrapped access methods
setMethod("rate", "DExp", function(object) rate(param(object)))

## wrapped replace methods
setMethod("rate<-", "DExp", function(object, value) new("DExp", rate = value))

setMethod("*", c("DExp","numeric"),
          function(e1, e2){
            if (length(e2)>1) stop("length of operator must be 1")
            if(e2 == 0) return(new("Dirac", location = 0, .withArith = TRUE))
               else return(new("DExp",rate = rate(e1) / abs(e2), .withArith = TRUE))
          })

