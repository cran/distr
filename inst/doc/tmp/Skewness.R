###################################################################################
#skewness --- code due to G. Jay Kerns, gkerns@ysu.edu
###################################################################################
setMethod("skewness", signature(x = "UnivariateDistribution"),
    function(x, fun = function(t) {t}, cond, withCond = FALSE, useApply = TRUE, ...){
        f2 <- function(t) {fun(t)^2}
        f3 <- function(t) {fun(t)^3}
        if(missing(cond))
            {
            m <- E(x, fun = fun, useApply = useApply, ...)
            m2 <- E(x, fun = f2, useApply = useApply, ...)
            m3 <- E(x, fun = f3, useApply = useApply, ...)

            return( (m3-3*m2*m+2*m^3)/(var(x, fun = fun, useApply = TRUE, ...))^1.5 )
            }
        else{
            m <- E(x, cond = cond, fun = fun, withCond  = withCond, useApply = useApply, ...)
            m2 <- E(x, cond = cond, fun = f2, withCond  = withCond, useApply = useApply, ...)
            m3 <- E(x, cond = cond, fun = f3, withCond  = withCond, useApply = useApply, ...)

            return( (m3-3*m2*m+2*m^3)/(var(x, fun = fun, cond = cond, withCond = FALSE, useApply = TRUE,...))^1.5  )

            }

    })

  
setMethod("skewness", signature(x = "AffLinDistribution"),
    function(x, fun = function(t) {t}, cond, withCond = FALSE, useApply = TRUE, ...){
        if (missing(fun) && missing(cond)){

            return( skewness(x@X0, withCond = withCond, useApply = useApply, 
                             ...))

            }
        else return(skewness( x = as(x, sub("AffLin","",class(x))), 
                    fun = fun, cond = cond, withCond = withCond, 
                    useApply = useApply, ... ))
    })

setMethod("skewness", signature(x = "AffLinAbscontDistribution"),
           getMethod("skewness", signature(x = "AffLinDistribution")))    
setMethod("skewness", signature(x = "AffLinDiscreteDistribution"),
           getMethod("skewness", signature(x = "AffLinDistribution")))    
setMethod("skewness", signature(x = "AffLinLatticeDistribution"),
           getMethod("skewness", signature(x = "AffLinDistribution")))    
###
# some exact skewnesses:
###
#
setMethod("skewness", signature(x = "Norm"),
    function(x,...){ 
    if((hasArg(fun))||(hasArg(cond)))
       return(skewness(as(x,"AbscontDistribution"),...))
    else
        return(0)
    })
#
setMethod("skewness", signature(x = "Binom"),
    function(x,  ...){
    if((hasArg(fun))||(hasArg(cond)))
       return(skewness(as(x,"DiscreteDistribution"),...))
    else
        return((1-2*prob(x))/sqrt(size(x)*prob(x)*(1-prob(x))))
    })

#
setMethod("skewness", signature(x = "Cauchy"),
    function(x,...){    
    if((hasArg(fun))||(hasArg(cond)))
      return(skewness(as(x,"AbscontDistribution"),...))
    else
        return(NA)
    })
#
setMethod("skewness", signature(x = "Chisq"),
    function(x,...){    
    if((hasArg(fun))||(hasArg(cond)))
       return(skewness(as(x,"AbscontDistribution"),...))
    else
        return( sqrt(8)*(df(x)+3*ncp(x))/(df(x)+2*ncp(x))^1.5)
    })
#
setMethod("skewness", signature(x = "Dirac"),
    function(x, ...){return(0)})

#
setMethod("skewness", signature(x = "DExp"),
    function(x, ...){    
    if((hasArg(fun))||(hasArg(cond))) 
         return(skewness(as(x,"AbscontDistribution"),...))
    else
        return(0)
    })
#
setMethod("skewness", signature(x = "Exp"),
    function(x, ...){    
    if((hasArg(fun))||(hasArg(cond))) 
         return(skewness(as(x,"AbscontDistribution"),...))
    else
        return(2)
    })

#
setMethod("skewness", signature(x = "Fd"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))){
         return(skewness(as(x,"AbscontDistribution"),...))
    }else {
        if (df2(x)>6){
          m <- df1(x)
          n <- df2(x)
          d <- ncp(x)
          L <- d/m
          m2 <- 2*n^2*(m+n-2)/m/(n-2)^2/(n-4)*(1+2*L+m*L^2/(m+n-2))
          a <-  8*n^3*(m+n-2)*(2*m+n-2)/m^2/(n-2)^3/(n-4)/(n-6)
          b <-  1+3*L+6*m*L^2/(2*m+n-2)+2*m^2*L^3/(m+n-2)/(2*m+n-2)
          m3 <- a*b
          return(m3/m2^1.5)
        } else {
          return(NA)
        }
    }
    })
#
setMethod("skewness", signature(x = "Gammad"),
    function(x, ...){    
    if((hasArg(fun))||(hasArg(cond))) 
         return(skewness(as(x,"AbscontDistribution"),...))
    else
        return(2/sqrt(shape(x)))
    })
#
setMethod("skewness", signature(x = "Geom"),
    function(x, ...){    
    if((hasArg(fun))||(hasArg(cond))) 
         return(skewness(as(x,"DiscreteDistribution"),...))
    else
        return((2-prob(x))/sqrt(1-prob(x)))
    })
#
setMethod("skewness", signature(x = "Hyper"),
    function(x, ...){    
    if((hasArg(fun))||(hasArg(cond))) 
         return(skewness(as(x,"DiscreteDistribution"),...))
    else
       {k <- k(x);
        m <- m(x); 
        n <- n(x);
        return( sqrt((m+n-1)/(k*m*n)/(m+n-k))*(n-m)*(m+n-2*k)/(m+n-2) )
        }
    })
#
setMethod("skewness", signature(x = "Logis"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))) 
        return(skewness(as(x,"AbscontDistribution"),...))
    else
        return(0)
    })
#
setMethod("skewness", signature(x = "Lnorm"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))) {
        return(skewness(as(x,"AbscontDistribution"),...))
    } else {
        w <- exp(sdlog(x)^2)
        return( sqrt(w-1)*(w+2) )
    }
    })
#
setMethod("skewness", signature(x = "Nbinom"),
    function(x, ...){    
    if((hasArg(fun))||(hasArg(cond))) 
         return(skewness(as(x,"DiscreteDistribution"),...))
    else
        return((2-prob(x))/sqrt(size(x)*(1-prob(x))))
    })
#
setMethod("skewness", signature(x = "Pois"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))) 
        return(skewness(as(x,"DiscreteDistribution"),...))
    else
        return(1/sqrt(lambda(x)))
    })
#
setMethod("skewness", signature(x = "Td"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))) {
        return(skewness(as(x,"AbscontDistribution"),...))
    } else {
        if (df(x)>3){
        n <- df(x); d<- ncp(x)
        m1 <- sqrt(0.5*n)*gamma(0.5*(n-1))*d/gamma(0.5*n)
        m2 <- n*(1+d^2)/(n-2)-m1^2
        m3 <- m1*(n*(2*n-3+d^2)/(n-2)/(n-3)-2*m2)
         return(m3/m2^1.5)
        } else {
         return(NA)
        }
    }
    })

#
setMethod("skewness", signature(x = "Unif"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))) 
        return(skewness(as(x,"AbscontDistribution"),...))
    else
        return(0)
    })
#
setMethod("skewness", signature(x = "Weibull"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))) 
        return(skewness(as(x,"AbscontDistribution"),...))
    else
        g1 <- gamma(1+1/shape(x))
        g2 <- gamma(1+2/shape(x))
        g3 <- gamma(1+3/shape(x))
        return( (g3-3*g1*(g2-g1^2)-g1^3)/(g2-g1^2)^1.5 )
    })
#    
setMethod("skewness", signature(x = "Beta"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))||(!isTRUE(all.equal(ncp(x),0)))) 
        return(skewness(as(x,"AbscontDistribution"),...))
    else
        {a<-shape1(x); b<- shape2(x)
        return( 2*(b-a)*sqrt(a+b+1)/(a+b+2)/sqrt(a*b) ) }
    })

###################################################################################
#skewness --- code P.R.:
###################################################################################

setMethod("skewness", signature(x = "Arcsine"),
    function(x, ...)return(0))




