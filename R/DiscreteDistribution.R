###############################################################################
# Methods for Discrete Distributions
###############################################################################
                          
## (c) Matthias Kohl: revised P.R. 030707

DiscreteDistribution <- function(supp, prob, .withArith = FALSE,
     .withSim = FALSE){
    if(!is.numeric(supp))
        stop("'supp' is no numeric vector")
    if(any(!is.finite(supp)))   # admit +/- Inf?
        stop("inifinite or missing values in supp")
    len <- length(supp)
    if(missing(prob)){
        prob <- rep(1/len, len)
    }else{
        if(len != length(prob))
            stop("'supp' and 'prob' must have equal lengths")
        if(any(!is.finite(prob)))
            stop("inifinite or missing values in prob")
        if(!identical(all.equal(sum(prob), 1,
                          tolerance = 2*getdistrOption("TruncQuantile")), TRUE))
            stop("sum of 'prob' has to be (approximately) 1")
        if(!all(prob >= 0))
            stop("'prob' contains values < 0")
    }
    if(length(usupp <- unique(supp)) < len){
        warning("collapsing to unique support values")
        prob <- as.vector(tapply(prob, supp, sum))
        supp <- sort(usupp)
        len <- length(supp)
        rm(usupp)
    }else{
        o <- order(supp)
        supp <- supp[o]
        prob <- prob[o]
        rm(o)
    }

    if(len > 1){
       if(min(diff(supp)) <
          getdistrOption("DistrResolution") )
        stop("grid too narrow --> change DistrResolution")
    }
    rm(len)

    rfun <- function(n){
        sample(x = supp, size = n, replace = TRUE, prob = prob)
    }

    dfun <- .makeDNew(supp, prob, Cont = FALSE)
    pfun <- .makePNew(supp, prob, .withSim, Cont = FALSE)
    qfun <- .makeQNew(supp, cumsum(prob), rev(cumsum(rev(prob))),
                      .withSim, min(supp), max(supp), Cont = FALSE)

    object <- new("DiscreteDistribution", r = rfun, d = dfun, q = qfun, p=pfun,
         support = supp, .withArith = .withArith, .withSim = .withSim)
}


setMethod("support", "DiscreteDistribution", function(object) object@support)

### left continuous cdf

setMethod("p.l", "DiscreteDistribution", function(object){
       if (.inArgs("lower.tail", p(object))){
           function(q, lower.tail = TRUE, log.p = FALSE){
                px <- p(object)(q, lower.tail = lower.tail)
                owarn <- getOption("warn"); options(warn = -2)
                dx <- d(object)(.setEqual(q, support(object)))
                options(warn = owarn)
                px0 <- pmax(px + if(lower.tail) -dx else  dx,0)
                if (log.p) px0 <- log(px0)
                return(px0)
                }
       }else{
           function(q, lower.tail = TRUE, log.p = FALSE){
                px <- p(object)(q)
                owarn <- getOption("warn"); options(warn = -2)
                dx <- d(object)(.setEqual(q, support(object)))
                options(warn = owarn)
                px0 <- pmax(if(lower.tail) px - dx else 1 - px + dx, 0)
                if (log.p) px0 <- log(px0)
                return(px0)
                }
       }
})

### right continuous quantile function

setMethod("q.r", "DiscreteDistribution", function(object){
    if (.inArgs("log.p", q(object))){
       if (.inArgs("lower.tail", q(object))){
           function(p, lower.tail = TRUE, log.p = FALSE){
                s <- support(object)
                psx <- p(object)(s, lower.tail = lower.tail,
                                 log.p = log.p)
                ps0 <- .setEqual(p, psx)

                owarn <- getOption("warn"); options(warn = -2)
                qx0 <- q(object)(ps0, lower.tail = lower.tail,
                                 log.p = log.p)
                options(warn = owarn)

                m <- match(ps0, psx)
                n.ina.m <- !is.na(m)
                if(any(n.ina.m))
                   { M.n.ina.m  <- m[n.ina.m]
                     qx0[n.ina.m] <- (support(object))[pmin(M.n.ina.m+1,
                                                       length(s))]
                   }
                if(any(is.nan(qx0)))
                   warning("NaN's produced")
                return(qx0)
                }
       }else{
           function(p, lower.tail = TRUE, log.p = FALSE){
                s <- support(object)
                psx <- p(object)(s, log.p = log.p)
                if (lower.tail) p <- 1 - p
                ps0 <- .setEqual(p, psx)

                owarn <- getOption("warn"); options(warn = -2)
                qx0 <- q(object)(ps0, lower.tail = lower.tail,
                                 log.p = log.p)
                options(warn = owarn)

                m <- match(ps0, psx)
                n.ina.m <- !is.na(m)
                if(any(n.ina.m))
                   { M.n.ina.m  <- m[n.ina.m]
                     qx0[n.ina.m] <- (support(object))[pmin(M.n.ina.m+1,
                                                       length(s))]
                   }
                if(any(is.nan(qx0)))
                   warning("NaN's produced")
                return(qx0)
                }
       }
    }else{
       if (.inArgs("lower.tail", q(object))){
           function(p, lower.tail = TRUE, log.p = FALSE){
                if (log.p) p <- exp(p)
                s <- support(object)
                psx <- p(object)(s, lower.tail = lower.tail)
                ps0 <- .setEqual(p, psx)

                owarn <- getOption("warn"); options(warn = -2)
                qx0 <- q(object)(ps0, lower.tail = lower.tail,
                                 log.p = log.p)
                options(warn = owarn)

                m <- match(ps0, psx)
                n.ina.m <- !is.na(m)
                if(any(n.ina.m))
                   { M.n.ina.m  <- m[n.ina.m]
                     qx0[n.ina.m] <- (support(object))[pmin(M.n.ina.m+1,
                                                       length(s))]
                   }
                if(any(is.nan(qx0)))
                   warning("NaN's produced")
                return(qx0)
                }
       }else{
           function(p, lower.tail = TRUE, log.p = FALSE){
                if (log.p) p <- exp(p)
                s <- support(object)
                psx <- p(object)(s)
                if (lower.tail) p <- 1 - p
                ps0 <- .setEqual(p, psx)

                owarn <- getOption("warn"); options(warn = -2)
                qx0 <- q(object)(ps0, lower.tail = lower.tail,
                                 log.p = log.p)
                options(warn = owarn)

                m <- match(ps0, psx)
                n.ina.m <- !is.na(m)
                if(any(n.ina.m))
                   { M.n.ina.m  <- m[n.ina.m]
                     qx0[n.ina.m] <- (support(object))[pmin(M.n.ina.m+1,
                                                       length(s))]
                   }
                if(any(is.nan(qx0)))
                   warning("NaN's produced")
                }
       }
    }
})



## Convolution Discrete Distributions

setMethod("+", c("DiscreteDistribution","DiscreteDistribution"),
function(e1,e2){
            e1 <- as(e1, "LatticeDistribution")
            e2 <- as(e2, "LatticeDistribution")
            if(is(e1, "LatticeDistribution") & is(e2, "LatticeDistribution"))
                return(e1 + e2)
            convolutedsupport <- rep(support(e1), each = length(support(e2))) +
                                 support(e2)

            gridvalues1 <- d(e1)(support(e1)); gridvalues2 <- d(e2)(support(e2))
            convolutedvalues <- rep(gridvalues1, each = length(support(e2))) *
                                gridvalues2
            rm(gridvalues1,gridvalues2)

            tmptable <- data.frame(x = convolutedsupport, dx = convolutedvalues)
            rm(convolutedsupport,convolutedvalues)
            tmp <- tapply(tmptable$dx, tmptable$x, sum)
            rm(tmptable)

            supp.u <- as.numeric(names(tmp))
            prob.u <- as.numeric(tmp)

            o <- order(supp.u)
            supp <- supp.u[o]
            prob <- prob.u[o]

            #supp.u <- unique(supp)

            len = length(supp)

            if(len > 1){
              if(min(abs(diff(supp))) < getdistrOption("DistrResolution"))
                {if(!getdistrOption("DistrCollapse"))
                    stop("grid too narrow --> change DistrResolution")
                 else
                    {supp1 <- 0*supp  
                     prob1 <- 0*prob
                     xo <- supp[1]-1
                     j <- 0
                     for(i in seq(along=supp))
                        {if (abs(supp[i]-xo) > getdistrOption("DistrResolution")) 
                             { j <- j+1
                               supp1[j] <- supp[i]
                               prob1[j] <- prob[i] 
                               xo <- supp1[j]
                             }
                        else { prob1[j] <- prob1[j]+prob[i] }
                        } 
                     prob <- prob1[1:j]
                     supp <- supp1[1:j]    
                     rm(prob1,supp1,i,j,xo)
                     }
                }
            }

            rm(tmp, len)

            .withSim <- e1@.withSim || e2@.withSim

            rfun <- function(n) {}
            body(rfun) <- substitute({ f(n) + g(n) },
                                         list(f = e1@r, g = e2@r))

            dfun <- .makeDNew(supp, prob, Cont = FALSE)
            pfun <- .makePNew(supp, prob, .withSim, Cont = FALSE)
            qfun <- .makeQNew(supp, cumsum(prob), rev(cumsum(rev(prob))),
                      .withSim, min(supp), max(supp), Cont = FALSE)

            object <- new("DiscreteDistribution", r = rfun, d = dfun, p = pfun,
                           q = qfun, support = supp,
                           .withSim = .withSim, .withArith = TRUE)
            rm(rfun, dfun, qfun, pfun)
            object

          })

## binary operators for discrete distributions

setMethod("*", c("DiscreteDistribution","numeric"),
           function(e1, e2) { Distr <- .multm(e1,e2, "DiscreteDistribution")
                              if(is(Distr, "AffLinDistribution"))
                                 Distr@X0 <- e1
                              Distr
                             })
setMethod("+", c("DiscreteDistribution","numeric"),
           function(e1, e2) { Distr <- .plusm(e1,e2, "DiscreteDistribution")
                              if(is(Distr, "AffLinDistribution"))
                                 Distr@X0 <- e1
                              Distr
                             })

setMethod("*", c("AffLinDiscreteDistribution","numeric"),
           function(e1, e2) .multm(e1,e2, "AffLinDiscreteDistribution"))
setMethod("+", c("AffLinDiscreteDistribution","numeric"),
           function(e1, e2) .plusm(e1,e2, "AffLinDiscreteDistribution"))

## Group Math for discrete distributions
setMethod("Math", "DiscreteDistribution",
          function(x){
            rnew <- function(n, ...){}
            body(rnew) <- substitute({ f(g(n, ...)) },
                                         list(f = as.name(.Generic), g = x@r))
            object <- new("DiscreteDistribution", r = rnew,
                           .withSim = TRUE, .withArith = TRUE)
            object
          })
setMethod("Math", "Dirac",
          function(x){ loc <- location(x)
                       lc <- callGeneric(loc)
                       Dirac(lc)})

## exact: abs for discrete distributions
setMethod("abs", "DiscreteDistribution",
          function(x){
            if (.isEqual(p.l(x)(0),0)) return(x)
            rnew <- function(n, ...){}
            body(rnew) <- substitute({ abs(g(n, ...)) },
                                         list(g = x@r))

            supportnew <- sort(unique(abs(support(x))))

            xx <- x
            dnew <- function(x, log = FALSE){
                    o.warn <- getOption("warn"); options(warn = -1)
                    dx <- (x>=0) * d(xx)(x) + (x>0) * d(xx)(-x) 
                    options(warn = o.warn)
                    if (log) dx <- log(dx)
                    return(dx)
            }
            
            pnew <- function(q, lower.tail = TRUE, log.p = FALSE){
                    px <- (q>=0) * (p(x)(q) - p.l(x)(-q))                    
                    if (!lower.tail) px <- 1 - px
                    if (log.p) px <- log(px)
                    return(px)
            }

            prob <- dnew(supportnew)
            
            qnew <- .makeQNew(supportnew, cumsum(prob), 
                            rev(cumsum(rev(prob))), notwithLLarg = x@.withSim, 
                            min(supportnew), max(supportnew), Cont = FALSE)

            object <- new("DiscreteDistribution", r = rnew, p = pnew,
                           q = qnew, d = dnew, support = supportnew, 
                           .withSim = x@.withSim, .withArith = TRUE)
            object
          })

## exact: abs for discrete distributions
setMethod("exp", "DiscreteDistribution",
           function(x) .expm.d(x))


### preliminary to export special functions
if (getRversion()>='2.6.0'){ 

setMethod("log", "DiscreteDistribution",
           function(x, base = exp(1)) {
           xs <- as.character(deparse(match.call(
                 call = sys.call(sys.parent(1)))$x))
           ep <- getdistrOption("TruncQuantile")
           basl <- log(base)
           if(p(x)(0)>ep) 
                stop(gettextf("log(%s) is not well-defined with positive probability ", xs))
           else return(.logm.d(x)/basl)})

setMethod("log", "Dirac",
          function(x, base = exp(1)){ 
                       xs <- as.character(deparse(match.call(
                             call = sys.call(sys.parent(1)))$x))
                       loc <- location(x) 
                       ep <- getdistrOption("TruncQuantile")
                       basl <- log(base)
                       if(loc < ep) 
                          stop(gettextf("log(%s) is not well-defined with positive probability ", xs))                       
                       Dirac(log(loc)/basl)})

setMethod("log10", "DiscreteDistribution",
          function(x) log(x = x, base = 10))

setMethod("sign", "DiscreteDistribution",
          function(x){ 
          d0 <- d(x)(0)
          DiscreteDistribution(supp=c(-1,0,1), 
              prob=c(p(x)(-getdistrOption("TruncQuantile")),
                     d0,
                     p(x)(getdistrOption("TruncQuantile"), lower=FALSE)))                     
          })


setMethod("lgamma", "DiscreteDistribution",
          function(x){
            rnew = function(n, ...){}
            body(rnew) <- substitute({ lgamma(g(n, ...)) }, list(g = x@r))
            object <- new("DiscreteDistribution", r = rnew,
                           .withSim = TRUE, .withArith = TRUE)
            object
          })

setMethod("gamma", "DiscreteDistribution",
          function(x){
            rnew = function(n, ...){}
            body(rnew) <- substitute({ gamma(g(n, ...)) }, list(g = x@r))
            object <- new("DiscreteDistribution", r = rnew,
                           .withSim = TRUE, .withArith = TRUE)
            object
          })
}          

