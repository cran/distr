###################################################################################
#kurtosis  --- code due to G. Jay Kerns, gkerns@ysu.edu
###################################################################################


###################################################################################
#kurtosis
###################################################################################
setMethod("kurtosis", signature(x = "UnivariateDistribution"),
    function(x, fun = function(t) {t}, cond, withCond = FALSE, useApply = TRUE, ...){
        f2 <- function(t) {fun(t)^2}
        f3 <- function(t) {fun(t)^3}
        f4 <- function(t) {fun(t)^4}
        if(missing(cond))
            {
            m <- E(x, fun = fun, useApply = useApply, ...)
            m2 <- E(x, fun = f2, useApply = useApply, ...)
            m3 <- E(x, fun = f3, useApply = useApply, ...)
            m4 <- E(x, fun = f4, useApply = useApply, ...)

            return( (m4-4*m3*m+6*m2*m^2-3*m^4)/(var(x, fun = fun, 
                                                    useApply = TRUE, ...))^2 -3)
            }
        else{
            m <- E(x, cond = cond, fun = fun, withCond  = withCond, useApply = useApply, ...)
            m2 <- E(x, cond = cond, fun = f2, withCond  = withCond, useApply = useApply, ...)
            m3 <- E(x, cond = cond, fun = f3, withCond  = withCond, useApply = useApply, ...)
            m4 <- E(x, cond = cond, fun = f4, withCond  = withCond, useApply = useApply, ...)

            return( (m4-4*m3*m+6*m2*m^2-3*m^4)/(var(x, fun = fun, cond = cond, 
                                  withCond = FALSE, useApply = TRUE,...))^2  -3)

            }

    })

setMethod("kurtosis", signature(x = "AffLinDistribution"),
    function(x, fun = function(t) {t}, cond, withCond = FALSE, useApply = TRUE, ...){
        if (missing(fun) && missing(cond)){

            return( kurtosis(x@X0, withCond = withCond, useApply = useApply, 
                             ...))

            }
        else return(kurtosis( x = as(x, sub("AffLin","",class(x))), 
                    fun = fun, cond = cond, withCond = withCond, 
                    useApply = useApply, ... ))
    })

setMethod("kurtosis", signature(x = "AffLinAbscontDistribution"),
           getMethod("kurtosis", signature(x = "AffLinDistribution")))    
setMethod("kurtosis", signature(x = "AffLinDiscreteDistribution"),
           getMethod("kurtosis", signature(x = "AffLinDistribution")))    
setMethod("kurtosis", signature(x = "AffLinLatticeDistribution"),
           getMethod("kurtosis", signature(x = "AffLinDistribution")))    
  
    
###
# some exact kurtoses:
###
setMethod("kurtosis", signature(x = "Norm"),
    function(x,...){ 
    if((hasArg(fun))||(hasArg(cond)))
       return(kurtosis(as(x,"AbscontDistribution"),...))
    else
        return(0)
    })
#
setMethod("kurtosis", signature(x = "Binom"),
    function(x,  ...){
    if((hasArg(fun))||(hasArg(cond)))
       return(kurtosis(as(x,"DiscreteDistribution"),...))
    else
        p <- prob(x)
        return((1-6*p*(1-p))/(size(x)*p*(1-p)))
    })

#
setMethod("kurtosis", signature(x = "Cauchy"),
    function(x,...){    
    if((hasArg(fun))||(hasArg(cond)))
      return(kurtosis(as(x,"AbscontDistribution"),...))
    else
        return(NA)
    })
#
setMethod("kurtosis", signature(x = "Chisq"),
    function(x,...){    
    if((hasArg(fun))||(hasArg(cond)))
       return(kurtosis(as(x,"AbscontDistribution"),...))
    else
        return(12*(df(x)+4*ncp(x))/(df(x)+2*ncp(x))^2)
    })
#
setMethod("kurtosis", signature(x = "Dirac"),
    function(x, ...){return(0)})

#
setMethod("kurtosis", signature(x = "DExp"),
    function(x, ...){    
    if((hasArg(fun))||(hasArg(cond))) 
         return(kurtosis(as(x,"AbscontDistribution"),...))
    else
        return(3)
    })
#
setMethod("kurtosis", signature(x = "Exp"),
    function(x, ...){    
    if((hasArg(fun))||(hasArg(cond))) 
         return(kurtosis(as(x,"AbscontDistribution"),...))
    else
        return(2)
    })

#
setMethod("kurtosis", signature(x = "Fd"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))) {
         return(kurtosis(as(x,"AbscontDistribution"),...))
    }else {
        if (df2(x)>8){
          m <- df1(x)
          n <- df2(x)
          d <- ncp(x)
          L <- d/m
          m2 <- 2*n^2*(m+n-2)/m/(n-2)^2/(n-4)*(1+2*L+m*L^2/(m+n-2))
          a <-  12*n^4*(m+n-2)/m^3/(n-2)^4/(n-4)/(n-6)/(n-8)
          b <-  (1+4*L)*(2*(3*m+n-2)*(2*m+n-2)+(m+n-2)*(n-2)*(m+2))
          c <-  2*m*(3*m+2*n-4)*(n+10)*L^2
          d <-  4*m^2*(n+10)*L^3
          e <-  m^3*(n+10)*L^4/(m+n-2)
          m4 <- a*(b+c+d+e)
          return(m4/m2^2-3)
        } else {
          return(NA)
        }
    }
    })
#
setMethod("kurtosis", signature(x = "Gammad"),
    function(x, ...){    
    if((hasArg(fun))||(hasArg(cond))) 
         return(kurtosis(as(x,"AbscontDistribution"),...))
    else
        return(6/shape(x))
    })
#
setMethod("kurtosis", signature(x = "Geom"),
    function(x, ...){    
    if((hasArg(fun))||(hasArg(cond))) 
         return(kurtosis(as(x,"DiscreteDistribution"),...))
    else
        return(6+ prob(x)^2/(1-prob(x)))
    })
#
setMethod("kurtosis", signature(x = "Hyper"),
    function(x, ...){    
    if((hasArg(fun))||(hasArg(cond))) 
         return(kurtosis(as(x,"DiscreteDistribution"),...))
    else
       {k <- k(x);
        m <- m(x); 
        n <- n(x);
        a <- (m+n)^2*(m+n-1)/(k*m*n*(m+n-k)*(m+n-2)*(m+n-3));
        return(
                a*((m+n)*(m+n+1-6*k)+3*m*n*(k-2)+6*k^2+3*m*n*k*(6-k)/(m+n)
                -18*m*n*k^2/(m+n)^2)
              )
        }
    })
#
setMethod("kurtosis", signature(x = "Logis"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))) 
        return(kurtosis(as(x,"AbscontDistribution"),...))
    else
        return(6/5)
    })
#
setMethod("kurtosis", signature(x = "Lnorm"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))) {
        return(kurtosis(as(x,"AbscontDistribution"),...))
    } else {
        w <- exp(sdlog(x)^2)
        return( w^4+2*w^3+3*w^2-3 )
    }
    })
#
setMethod("kurtosis", signature(x = "Nbinom"),
    function(x, ...){    
    if((hasArg(fun))||(hasArg(cond))) 
         return(kurtosis(as(x,"DiscreteDistribution"),...))
    else
        return(6/size(x)+prob(x)^2/(size(x)*(1-prob(x))))
    })
#
setMethod("kurtosis", signature(x = "Pois"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))) 
        return(kurtosis(as(x,"DiscreteDistribution"),...))
    else
        return(1/lambda(x))
    })
#
setMethod("kurtosis", signature(x = "Td"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))){ 
        return(kurtosis(as(x,"AbscontDistribution"),...))
    } else {
        if (df(x)>4){
          n <- df(x); d<- ncp(x)
          m1 <- sqrt(0.5*n)*gamma(0.5*(n-1))*d/gamma(0.5*n)
          m2 <- n*(1+d^2)/(n-2)-m1^2
          m3 <- m1*(n*(2*n-3+d^2)/(n-2)/(n-3)-2*m2)
          m4 <- n^2*(3+6*d^2+d^4)/(n-2)/(n-4)-m1^2*(n*((n+1)*d^2+3*(3*n-5))/(n-2)/(n-3)-3*m2)
          return(m4/m2^2-3)
        } else {
          return(NA)
        }
    }
    })

#
setMethod("kurtosis", signature(x = "Unif"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))) 
        return(kurtosis(as(x,"AbscontDistribution"),...))
    else
        return(-6/5)
    })
#
setMethod("kurtosis", signature(x = "Weibull"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))) 
        return(kurtosis(as(x,"AbscontDistribution"),...))
    else
        g1 <- gamma(1+1/shape(x))
        g2 <- gamma(1+2/shape(x))
        g3 <- gamma(1+3/shape(x))
        g4 <- gamma(1+4/shape(x))
        v <- (g2-g1^2)^2
        return( (g4-4*g3*g1+6*g2*g1^2-3*g1^4)/v - 3 )
    })
#    
setMethod("kurtosis", signature(x = "Beta"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))||(!isTRUE(all.equal(ncp(x),0)))) 
        return(kurtosis(as(x,"AbscontDistribution"),...))
    else
        {a<-shape1(x); b<- shape2(x)
        return(6*(a^3-a^2*(2*b-1)+b^2*(b+1)-2*a*b*(b+2))/(a*b*(a+b+2)*(a+b+3)) )}
    })

###################################################################################
#kurtosis --- code P.R.:
###################################################################################

setMethod("kurtosis", signature(x = "Arcsine"),
    function(x, ...)return(-3/2))



