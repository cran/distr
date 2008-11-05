## Integration of functions
setMethod("E", signature(object = "UnivariateDistribution", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun, cond){
        return(mean(r(object)(.distrExOptions$MCIterations)))
    })
setMethod("E", signature(object = "AbscontDistribution", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun, cond){
        integrand <- function(x, dfun){ x * dfun(x) }
        return(distrExIntegrate(f = integrand, 
                    lower = q(object)(.distrExOptions$ElowerTruncQuantile),
                    upper = q(object)(1-.distrExOptions$EupperTruncQuantile), 
                    rel.tol = .distrExOptions$ErelativeTolerance, 
                    distr = object, dfun = d(object)))
    })
setMethod("E", signature(object = "DiscreteDistribution", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun, cond){
        supp <- support(object)
        dfun <- d(object)
        return(sum(supp * dfun(supp)))
    })

setMethod("E", signature(object = "AffLinDistribution", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun, cond){
             object@a * E(object@X0) + object@b
    })

setMethod("E", signature(object = "AffLinAbscontDistribution", 
                         fun = "missing", 
                         cond = "missing"),
           getMethod("E", signature(object = "AffLinDistribution", 
                         fun = "missing", 
                         cond = "missing")))    
setMethod("E", signature(object = "AffLinDiscreteDistribution", 
                         fun = "missing", 
                         cond = "missing"),
           getMethod("E", signature(object = "AffLinDistribution", 
                         fun = "missing", 
                         cond = "missing")))    
setMethod("E", signature(object = "AffLinLatticeDistribution", 
                         fun = "missing", 
                         cond = "missing"),
           getMethod("E", signature(object = "AffLinDistribution", 
                         fun = "missing", 
                         cond = "missing")))    




setMethod("E", signature(object = "MultivariateDistribution", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun, cond){
        return(colMeans(r(object)(.distrExOptions$MCIterations)))
    })
setMethod("E", signature(object = "DiscreteMVDistribution", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun, cond){
        supp <- support(object)
        integrand <- function(x, dfun){ x * dfun(t(x)) }
        erg <- apply(supp, 1, integrand, dfun = d(object))
        if(is.vector(erg))
            return(sum(erg))
        else
            return(rowSums(erg))
    })
setMethod("E", signature(object = "UnivariateDistribution", 
                         fun = "function", 
                         cond = "missing"),
    function(object, fun, cond, useApply = TRUE, ...){
        if(useApply)        
            return(mean(sapply(r(object)(.distrExOptions$MCIterations), fun, ...)))
        else
            return(mean(fun(r(object)(.distrExOptions$MCIterations), ...)))
    })
setMethod("E", signature(object = "AbscontDistribution", 
                         fun = "function", 
                         cond = "missing"),
    function(object, fun, cond, useApply = TRUE, ...){
        if(useApply){
            integrand <- function(x, dfun, fun, ...){ 
                sapply(x, fun, ...) * dfun(x) 
            }
        }else{
            integrand <- function(x, dfun, fun, ...){ 
                fun(x, ...) * dfun(x) 
            }
        }
        return(distrExIntegrate(f = integrand, 
                    lower = q(object)(.distrExOptions$ElowerTruncQuantile), 
                    upper = q(object)(1-.distrExOptions$EupperTruncQuantile), 
                    rel.tol = .distrExOptions$ErelativeTolerance, 
                    distr = object, fun = fun, dfun = d(object), ...))
    })
setMethod("E", signature(object = "DiscreteDistribution", 
                         fun = "function", 
                         cond = "missing"),
    function(object, fun, cond, useApply = TRUE, ...){
        supp <- support(object)
        if(useApply){
            integrand <- function(x, dfun, fun, ...){
                sapply(x, fun, ...) * dfun(x)
            }
        }else{
            integrand <- function(x, dfun, fun, ...){
                fun(x, ...) * dfun(x)
            }
        }
        return(sum(integrand(x = supp, dfun = d(object), fun = fun, ...)))
    })
setMethod("E", signature(object = "MultivariateDistribution", 
                         fun = "function", 
                         cond = "missing"),
    function(object, fun, cond, useApply = TRUE, ...){
        x <- r(object)(.distrExOptions$MCIterations)
        if(useApply)
            erg <- apply(x, 1, fun, ...)
        else
            erg <- t(fun(x, ...))
        if(is.vector(erg))
            return(mean(erg))
        else{
            res <- fun(x[1,], ...)
            res[] <- rowMeans(erg)
            return(res)
        }
    })
setMethod("E", signature(object = "DiscreteMVDistribution", 
                         fun = "function", 
                         cond = "missing"),
    function(object, fun, cond, useApply = TRUE, ...){
        supp <- support(object)
        if(useApply){
            integrand <- function(x, fun, dfun, ...){ fun(x, ...) * dfun(t(x)) }
            erg <- apply(supp, 1, integrand, fun = fun, dfun = d(object), ...)
        }else{
            erg <- t(fun(supp, ...) * d(object)(supp))
        }
        if(is.vector(erg))
            return(sum(erg))
        else{
            res <- fun(supp[1,], ...)
            res[] <- rowSums(erg)
            return(res)
        }
    })
## Conditional expectation of functions
setMethod("E", signature(object = "UnivariateCondDistribution", 
                         fun = "missing", 
                         cond = "numeric"),
    function(object, fun, cond){
        return(mean(r(object)(.distrExOptions$MCIterations, cond)))
    })
setMethod("E", signature(object = "AbscontCondDistribution", 
                         fun = "missing", 
                         cond = "numeric"),
    function(object, fun, cond, useApply = TRUE){
        fct <- function(x, dfun, cond){ x * dfun(x, cond) }
        if(useApply){
            integrand <- function(x, dfun, cond){ 
                return(sapply(x, fct, dfun = dfun, cond = cond))
            }
        }else{
            integrand <- fct
        }
        return(distrExIntegrate(integrand, 
               lower = q(object)(.distrExOptions$ElowerTruncQuantile, cond), 
                upper = q(object)(1-.distrExOptions$EupperTruncQuantile, cond), 
                rel.tol = .distrExOptions$ErelativeTolerance, distr = object, 
                dfun = d(object), cond = cond))
    })
setMethod("E", signature(object = "DiscreteCondDistribution", 
                         fun = "missing",
                         cond = "numeric"),
    function(object, fun, cond, useApply = TRUE){
        supp <- support(object)(cond)
        fct <- function(x, dfun, cond){ x * dfun(x, cond) }
        if(useApply)
            return(sum(sapply(supp, fct, dfun = d(object), cond = cond)))
        else
            return(sum(fct(x = supp, dfun = d(object), cond = cond)))            
    })
setMethod("E", signature(object = "UnivariateCondDistribution",
                         fun = "function", 
                         cond = "numeric"),
    function(object, fun, cond, withCond = FALSE, useApply = TRUE, ...){
        if(withCond){
            if(useApply)
                res <- mean(sapply(r(object)(.distrExOptions$MCIterations, 
                                              cond), fun, cond, ...))
            else
                res <- mean(fun(r(object)(.distrExOptions$MCIterations, 
                                           cond), ...))
        }else{
            if(useApply)
                res <- mean(sapply(r(object)(.distrExOptions$MCIterations, 
                                              cond), fun, ...))
            else
                res <- mean(fun(r(object)(.distrExOptions$MCIterations, 
                                           cond), cond, ...))                
        }

        return(res)
    })
setMethod("E", signature(object = "AbscontCondDistribution", 
                         fun = "function", 
                         cond = "numeric"),
    function(object, fun, cond, withCond = FALSE, useApply = TRUE, ...){
        if(withCond)
            if(useApply){
                integrand <- function(x, dfun, fun, cond, ...){ 
                    sapply(x, fun, cond, ...) * dfun(x, cond) 
                }        
            }else{
                integrand <- function(x, dfun, fun, cond, ...){ 
                    fun(x, cond, ...) * dfun(x, cond) 
                }        
            }
        else
            if(useApply){
                integrand <- function(x, dfun, fun, cond, ...){ 
                    sapply(x, fun, ...) * dfun(x, cond) 
                }
            }else{
                integrand <- function(x, dfun, fun, cond, ...){ 
                    fun(x, ...) * dfun(x, cond) 
                }
            }
        
        return(distrExIntegrate(integrand, 
                lower = q(object)(.distrExOptions$ElowerTruncQuantile, cond), 
                upper = q(object)(1-.distrExOptions$EupperTruncQuantile, cond), 
                rel.tol = .distrExOptions$ErelativeTolerance, distr = object, 
                dfun = d(object), fun = fun, cond = cond, ...))
    })
setMethod("E", signature(object = "DiscreteCondDistribution", 
                         fun = "function",
                         cond = "numeric"),
    function(object, fun, cond, withCond = FALSE, useApply = TRUE, ...){
        supp <- support(object)(cond)
        if(withCond){
            if(useApply){
                fct <- function(x, dfun, fun, cond, ...){ 
                    sapply(x, fun, cond, ...) * dfun(x, cond) 
                }
            }else{
                fct <- function(x, dfun, fun, cond, ...){ 
                    fun(x, cond, ...) * dfun(x, cond) 
                }
            }
        }else{
            if(useApply){
                fct <- function(x, dfun, fun, cond, ...){ 
                    sapply(x, fun, ...) * dfun(x, cond) 
                }
            }else{
                fct <- function(x, dfun, fun, cond, ...){ 
                    fun(x, ...) * dfun(x, cond) 
                }
            }
        }
        return(sum(fct(x = supp, dfun = d(object), fun = fun, 
                       cond = cond, ...)))
    })

### added 29-03-06 P.R. 
# some exact expectations:
setMethod("E", signature(object = "Norm", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun, cond){
        return(mean(object))
    })

setMethod("E", signature(object = "Beta", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun, cond){
        if(!isTRUE(all.equal(ncp(object),0)))
          return(E(as(object,"AbscontDistribution"),...))
        else
          return(shape1(object)/(shape1(object)+shape2(object)))
    })

setMethod("E", signature(object = "Binom", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun, cond){
        return(size(object)*prob(object))
    })

setMethod("E", signature(object = "Cauchy", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun, cond){
        return(NA)
    })

setMethod("E", signature(object = "Chisq", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun, cond){
        return(df(object)+ncp(object))
    })

setMethod("E", signature(object = "Dirac", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun, cond){
        return(location(object))
    })


setMethod("E", signature(object = "DExp", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun, cond){
        return(0)
    })

setMethod("E", signature(object = "Exp", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun, cond){
        return(1/rate(object))
    })


setMethod("E", signature(object = "Fd", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun, cond){ 
        df1 <- df1(object)
        df2 <- df2(object)
        d <- ncp(object)
        return(ifelse(df2>2,df2/(df2-2)*(df1+d)/df1,Inf))
    })

setMethod("E", signature(object = "Gammad", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun, cond){
        return(shape(object)*scale(object))
    })

setMethod("E", signature(object = "Geom", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun, cond){
        return(1/ prob(object) -1)
    })

setMethod("E", signature(object = "Hyper", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun, cond){
        return(k(object)*n(object)/(m(object)+n(object)))
    })

setMethod("E", signature(object = "Logis", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun, cond){
        return(location(object))
    })

setMethod("E", signature(object = "Lnorm", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun, cond){
        return(exp(meanlog(object)+sdlog(object)^2/2))
    })

setMethod("E", signature(object = "Nbinom", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun, cond){
        return(size(object)*(1-prob(object))/prob(object))
    })

setMethod("E", signature(object = "Pois", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun, cond){
        return(lambda(object))
    })

setMethod("E", signature(object = "Td", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun, cond){
        ## correction thanks to G.Jay Kerns
        return(ifelse( df(object)>1, 
                       ncp(object)*sqrt(df(object)/2)*
                         exp(lgamma((df(object)-1)/2)-lgamma(df(object)/2)), 
                       NA))
    })

setMethod("E", signature(object = "Unif", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun, cond){
        return((Max(object)+Min(object))/2)
    })

setMethod("E", signature(object = "Weibull", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun, cond){
        return(scale(object)*gamma(1+1/shape(object)))
    })
setMethod("E", signature(object = "Arcsine", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun, cond){
        return(0)
    })
