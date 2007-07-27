## Access Methods

setMethod("gaps", signature(object = "AbscontDistribution"),  
           function(object) object@gaps)

## ReplaceMethods

setReplaceMethod("gaps", signature(object = "AbscontDistribution"),  
          function(object, value)
                  {dsvalue <- deparse(substitute(value))
                   if(!is.null(value)){
                      if(!is.matrix(value))
                         stop("value must either be a matrix or NULL")
                      if(!ncol(value)==2)
                         stop("if matrix, value must have 2 columns")
                      l <- length(value)
                      if(!identical(1:l, order(c(t(value)))))
                         stop(gettextf("c(t(%s)) must be increasing",
                              dsvalue))
                      colnames(value) <- c("from", "to")
                      }
                   object@gaps <- value; object})
                                                             

  setMethod("setgaps", signature(object = "AbscontDistribution"), 
            function(object, exactq = 6, ngrid = 50000, ...){
       object1 <- object
       lower <- getLow(object, eps = getdistrOption("TruncQuantile")*2)
       upper <- getUp(object, eps = getdistrOption("TruncQuantile")*2)
       #lower <- 0 ; upper <- 8
       dist <- upper - lower
       grid <- seq(from = lower - 0.1 * dist, to = upper + 0.1 * dist, 
                          length = ngrid) 
       dxg <- d(object)(grid)
       
       ix <-  1:ngrid
       ix0 <- (dxg < 1/10^exactq)&(grid>=lower)&(grid<=upper)     
       if(any(ix0)){
          ixc <- ix[ix0]
          dixc <- c(2,diff(ixc))
          dixc0 <- dixc>1
          l2 <- length(ixc)
          ixc2 <- seq(l2)
          ixcl <- ixc2[dixc0]
          ixcr <- c(ixcl[-1]-1,l2)
          gridl <- grid[ixc[ixcl]]
          gridr <- grid[ixc[ixcr]]
          
          mattab.d <- cbind(gridl, gridr)
          
          ox <- order(mattab.d[,1])
          mattab.d <- matrix(mattab.d[ox,], ncol = 2)
          } else mattab.d <- NULL
          eval(substitute( "slot<-"(object,'gaps', value = mattab.d)))
       return(invisible())
  }
  )
 
## Arithmetics

setMethod("+", c("AbscontDistribution","AbscontDistribution"),
function(e1,e2){
            ### Step 1 : Truncation
            
            lower <- min(getLow(e1), getLow(e2))
            upper <- max(getUp(e1) , getUp(e2))

            ### Step 2 : Discretizing

            n <- getdistrOption("DefaultNrFFTGridPointsExponent")
            h <- (upper-lower)/2^n

            dpe1 <- .discretizeP(e1, lower, upper, h)
            dpe2 <- .discretizeP(e2, lower, upper, h)

            x <- seq(from = 2*lower, to = 2*upper, by = h)

            ### Step 3 : Zero-Padding

            dpe1 <- c(dpe1, numeric(2^n))
            dpe2 <- c(dpe2, numeric(2^n))

            ## Step 4: computation of DFT

            ftpe1 <- fft(dpe1); ftpe2 <- fft(dpe2)
            ## convolution theorem for DFTs
            d2 <- c(0,Re(fft(ftpe1*ftpe2, inverse = TRUE)) / length(ftpe1))

            ## density & cdf (steps 5--7)
            dfun <- .makeDNew(x, d2, h)
            pfun <- .makePNew(x, d2, h, .notwithLArg(e1)||.notwithLArg(e2) )


            ## quantile function
            yL <-  if ((q(e1)(0) == -Inf)||(q(e2)(0) == -Inf))
                 -Inf else getLow(e1)+getLow(e2)
            yR <-  if ((q(e1)(1) ==  Inf)||(q(e2)(1) ==  Inf))
                  Inf else getUp(e1)+getUp(e2)

            px.l <- pfun(x + 0.5*h)
            px.u <- pfun(x + 0.5*h, lower.tail = FALSE)
            
            qfun <- .makeQNew(x + 0.5*h, px.l, px.u,
                              .notwithLArg(e1)||.notwithLArg(e2), yL, yR)
            
            rfun <- function(n){}
            body(rfun) <- substitute({ f(n) + g(n) },
                                     list(f = e1@r, g = e2@r))

            object <- new("AbscontDistribution", r = rfun, d = dfun, p = pfun,
                          q = qfun, .withSim = FALSE, .withArith = TRUE)

            rm(d2, dpe1,dpe2, ftpe1,ftpe2)
            rm(h, px.l, px.u, rfun, dfun, qfun, pfun, upper, lower)
            object
          })


###setMethod("m1df", "AbscontDistribution",
###   function(object){
###     lower <- q(object)(TruncQuantile)
###     upper <- q(object)(1 - TruncQuantile)
###     
###     gitter.x <- seq(from = lower, to = upper, length = DefaultNrGridPoints)
###     
###    integrand <- function(x) x * d(object)(x)
###     
###     tmp <- function(t) integrate(integrand, lower = lower, upper = t)$value
###     
###     gitter.y <- sapply(gitter.x, tmp)
###     
###     approxfun(gitter.x, gitter.y, rule = 2)
###   })


###setMethod("m2df", "AbscontDistribution", 
###   function(object){
###     lower <- q(object)(TruncQuantile)
###     upper <- q(object)(1 - TruncQuantile)
###     
###     gitter.x <- seq(from = lower, to = upper, length = DefaultNrGridPoints)
###     
###     integrand <- function(x) x^2 * d(object)(x)
###     
###     tmp <- function(t) integrate(integrand, lower = lower, upper = t)$value
###     
###     gitter.y <- sapply(gitter.x, tmp)
###     
###     approxfun(gitter.x, gitter.y, rule = 2)
###   })

## binary operators for absolut continuous distributions


setMethod("*", c("AbscontDistribution","numeric"),
          function(e1, e2) {Distr <-  .multm(e1,e2, "AbscontDistribution")                               
                            if(is(Distr, "AffLinDistribution"))
                                 Distr@X0 <- e1
                            Distr})
setMethod("+", c("AbscontDistribution","numeric"),
           function(e1, e2) {Distr <-  .plusm(e1,e2, "AbscontDistribution")                               
                            if(is(Distr, "AffLinDistribution"))
                                 Distr@X0 <- e1
                            Distr})                            
setMethod("*", c("AffLinAbscontDistribution","numeric"),
          function(e1, e2) .multm(e1,e2, "AffLinAbscontDistribution"))
setMethod("+", c("AffLinAbscontDistribution","numeric"),
           function(e1, e2) .plusm(e1,e2, "AffLinAbscontDistribution"))

## Group Math for absolutly continuous distributions
setMethod("Math", "AbscontDistribution",
          function(x){
            rnew <- function(n, ...){}
            body(rnew) <- substitute({ f(g(n, ...)) },
                              list(f = as.name(.Generic), g = x@r))
            object <- new("AbscontDistribution", r = rnew,
                           .withSim = TRUE, .withArith = TRUE)
            object
          })

## exact: abs for absolutly continuous distributions
setMethod("abs", "AbscontDistribution",
          function(x){
            if (.isEqual(p(x)(0),0)) return(x)
            rnew <- function(n, ...){}
            body(rnew) <- substitute({ abs(g(n, ...)) },
                                         list(g = x@r))
            if (is.null(gaps(x)))
                gapsnew <- NULL
            else {VZW <- gaps(x)[,1] <= 0 & gaps(x)[,2] >= 0
                  gapsnew <- t(apply(abs(gaps(x)), 1, sort))
                  gapsnew[VZW,2] <- pmin(-gaps(x)[VZW,1], gaps(x)[VZW,2])
                  gapsnew[VZW,1] <- 0}
            
            lower <- max(0, getLow(x))
            upper <- max(-getLow(x) , abs(getUp(x)))

            n <- getdistrOption("DefaultNrFFTGridPointsExponent")
            h <- (upper-lower)/2^n

            xx <- x
            x.g <- seq(from = lower, to = upper, by = h)

            dnew <- function(x, log = FALSE){
                    o.warn <- getOption("warn"); options(warn = -1)
                    dx <- (x>=0) * (d(xx)(x) + d(xx)(-x)) 
                    options(warn = o.warn)
                    if (log) dx <- log(dx)
                    return(dx)
            }
            
            pnew <- function(q, lower.tail = TRUE, log.p = FALSE){
                    px <- (q>=0) * (p(x)(q) - p(x)(-q))                    
                    if (!lower.tail) px <- 1 - px
                    if (log.p) px <- log(px)
                    return(px)
            }

            px.l <- pnew(x.g + 0.5*h)
            px.u <- pnew(x.g + 0.5*h, lower.tail = FALSE)
            
            yR <- max(q(x)(1), abs(q(x)(0)))

            qnew <- .makeQNew(x.g + 0.5*h, px.l, px.u,
                              notwithLLarg = FALSE,  lower, yR)

            object <- new("AbscontDistribution", r = rnew, p = pnew,
                           q = qnew, d = dnew, gaps = gapsnew, 
                           .withSim = x@.withSim, .withArith = TRUE)
            object
          })

## exact: exp for absolutly continuous distributions
setMethod("exp", "AbscontDistribution",
          function(x){
            rnew <- function(n, ...){}
            body(rnew) <- substitute({ exp(g(n, ...)) },
                                         list(g = x@r))
            if (is.null(gaps(x)))
                 gapsnew <- NULL
            else gapsnew <- exp(gaps(x))
            
            lower <- exp(getLow(x))
            upper <- exp(getUp(x))

            n <- getdistrOption("DefaultNrFFTGridPointsExponent")
            h <- (upper-lower)/2^n

            xx <- x
            x.g <- seq(from = lower, to = upper, by = h)

            dnew <- function(x, log = FALSE){
                    x1 <- ifelse (x <= 0, 1, x) 
                    if (.inArgs("log", d(xx)))
                        dx <- (x>0) * d(xx)(log(x1), log)
                    else{
                        dx <- (x>0) * d(xx)(log(x1))
                        if (log) dx <- log(dx)                    
                    } 
                    dx <- if (log) dx - (x>0) * log(abs(x1)) else  dx/abs(x1) 
            }
            
            pnew <- function(q, lower.tail = TRUE, log.p = FALSE){
                    q1 <- ifelse (q <= 0, 0, q) 
                    if (.inArgs("log.p", p(x)) && .inArgs("lower.tail", p(x))){
                              px <- p(x)(log(q1), log.p = log.p, 
                                                 lower.tail = lower.tail) 
                    }else{
                         if (.inArgs("lower.tail", p(x)))
                              px <- p(x)(log(q1), lower.tail = lower.tail) 
                         else{px <- p(x)(log(q1)) 
                              if (lower.tail) px <- 1 - px}                   
                         if (log.p) px <- log(px)
                    }
                    return(px)
            }

            px.l <- pnew(x.g + 0.5*h)
            px.u <- pnew(x.g + 0.5*h, lower.tail = FALSE)
            
            yL <- exp(q(x)(0))
            yR <- exp(q(x)(1))

            qnew <- .makeQNew(x.g + 0.5*h, px.l, px.u,
                            notwithLLarg = FALSE,  yL, yR)

            object <- new("AbscontDistribution", r = rnew, p = pnew,
                           q = qnew, d = dnew, gaps = gapsnew, 
                           .withSim = x@.withSim, .withArith = TRUE)
            object
          })

### preliminary to export special functions
if (getRversion()>='2.6.0'){

setMethod("log", "AbscontDistribution", function(x){
            if (p(x)(0)>0)
                stop("With positive probability log(x) is not well defined.")                
            rnew = function(n, ...){}
            body(rnew) <- substitute({ log(g(n, ...)) }, list(g = x@r))

            if (is.null(gaps(x)))
                 gapsnew <- NULL
            else gapsnew <- log(gaps(x))
            
            lower <- max(log(getLow(x)),
                         log(q(x)(2*getdistrOption("TruncQuantile"))), -7)
            upper <- log(getUp(x))

            n <- getdistrOption("DefaultNrFFTGridPointsExponent")
            h <- (upper-lower)/2^n

            xx <- x
            x.g <- seq(from = lower, to = upper, by = h)

            dnew <- function(x, log = FALSE){
                    if (.inArgs("log", d(xx)))
                        dx <- d(xx)(exp(x), log)  
                    else{
                        dx <- d(xx)(exp(x)) 
                        if (log) dx <- log(dx)                    
                    } 
                    dx <- if (log) dx + x else  dx*exp(x) 
                    return(dx)
            }
            
            pnew <- function(q, lower.tail = TRUE, log.p = FALSE){
                    if (.inArgs("log.p", p(x)) && .inArgs("lower.tail", p(x))){
                              px <- p(x)(exp(q), log.p = log.p, 
                                                 lower.tail = lower.tail) 
                    }else{
                         if (.inArgs("lower.tail", p(x)))
                              px <- p(x)(exp(q), lower.tail = lower.tail) 
                         else{px <- p(x)(exp(q)) 
                              if (lower.tail) px <- 1 - px}                   
                         if (log.p) px <- log(px)
                    }
                    return(px)
            }

            px.l <- pnew(x.g + 0.5*h)
            px.u <- pnew(x.g + 0.5*h, lower.tail = FALSE)
            
            yL <- log(q(x)(0))
            yR <- log(q(x)(1))

            qnew <- .makeQNew(x.g + 0.5*h, px.l, px.u,
                            notwithLLarg = FALSE,  yL, yR)
            
            object <- new("AbscontDistribution", r = rnew, p = pnew,
                           q = qnew, d = dnew, gaps = gapsnew, 
                           .withSim = x@.withSim, .withArith = TRUE)
            object
          })
                       
                       
setMethod("log10", "AbscontDistribution",
          function(x) log(x)/log(10))




setMethod("lgamma", "AbscontDistribution",
          function(x){
            rnew = function(n, ...){}
            body(rnew) <- substitute({ lgamma(g(n, ...)) }, list(g = x@r))
            object <- new("AbscontDistribution", r = rnew,
                           .withSim = TRUE, .withArith = TRUE)
            object
          })

setMethod("gamma", "AbscontDistribution",
          function(x){
            rnew = function(n, ...){}
            body(rnew) <- substitute({ gamma(g(n, ...)) }, list(g = x@r))
            object <- new("AbscontDistribution", r = rnew,
                           .withSim = TRUE, .withArith = TRUE)
            object
          })
}