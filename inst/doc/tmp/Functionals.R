################################################################################
# some worked out functionals
################################################################################

################################################################################
#Var
################################################################################
setMethod("var", signature(x = "UnivariateDistribution"),
    function(x, fun = function(t) {t}, cond, withCond = FALSE, useApply = TRUE, 
             ...){
        f2 <- function(t) {fun(t)^2}
        if(missing(cond))
            {
            m <- E(x, fun = fun, useApply = useApply, ...) 
            m2 <- E(x, fun = f2, useApply = useApply, ...)
            }
        else{
            m <- E(x, cond = cond, fun = fun, withCond  = withCond, 
                   useApply = useApply, ...) 
            m2 <- E(x, cond = cond, fun = f2, withCond  = withCond, useApply = 
                    useApply, ...)
            }
        return(m2-m^2)
    })

setMethod("var", signature(x = "AffLinDistribution"),
    function(x, fun = function(t) {t}, cond, withCond = FALSE, useApply = TRUE, 
             ...){
        if (missing(fun) && missing(cond)){

            return( x@a^2* var(x@X0, withCond = withCond, useApply = useApply, 
                             ...))

        }else return(var( x = as(x, sub("AffLin","",class(x))), 
                    fun = fun, cond = cond, withCond = withCond, 
                    useApply = useApply, ... ))
    })

setMethod("var", signature(x = "AffLinAbscontDistribution"),
           getMethod("var", signature(x = "AffLinDistribution")))    
setMethod("var", signature(x = "AffLinDiscreteDistribution"),
           getMethod("var", signature(x = "AffLinDistribution")))    
setMethod("var", signature(x = "AffLinLatticeDistribution"),
           getMethod("var", signature(x = "AffLinDistribution")))    

################################################################################
#sd
################################################################################
setMethod("sd", signature(x = "UnivariateDistribution"), 
    function(x, fun, cond, withCond = FALSE, useApply = TRUE, ...){
      if(missing(fun))
        {if(missing(cond))
           return(sqrt(var(x, useApply = TRUE, ...)))
        else
           return(sqrt(var(x, cond = cond, withCond = FALSE, useApply = TRUE, 
                  ...)))
      }else{
        if(missing(cond))
           return(sqrt(var(x, fun = fun, useApply = TRUE, ...)))
        else
           return(sqrt(var(x, fun = fun, cond = cond, withCond = FALSE, 
                  useApply = TRUE,...)))
           }           
    })

### overload "sd" method for "Norm" ...
setMethod("sd", signature(x = "Norm"), 
    function(x, fun, cond, withCond = FALSE, useApply = TRUE, ...){
      if(missing(fun))
        {if(missing(cond))
           return(sd(param(x)))
        else
           return(sqrt(var(x, cond = cond, withCond = FALSE, useApply = TRUE, 
                  ...)))}
      else
        {if(missing(cond))
           return(sqrt(var(x, fun = fun, useApply = TRUE, ...)))
        else
           return(sqrt(var(x, fun = fun, cond = cond, withCond = FALSE, 
                  useApply = TRUE,...)))}           
    }) 
    


################################################################################
#median, mad, IQR
################################################################################
setMethod("median", signature(x = "UnivariateDistribution"),
    function(x){
        return(q(x)(1/2))
    })

setMethod("median", signature(x = "AffLinDistribution"),
    function(x) x@a * median(x@X0) + x@b) 

setMethod("median", signature(x = "AffLinAbscontDistribution"),
           getMethod("median", signature(x = "AffLinDistribution")))    
setMethod("median", signature(x = "AffLinDiscreteDistribution"),
           getMethod("median", signature(x = "AffLinDistribution")))    
setMethod("median", signature(x = "AffLinLatticeDistribution"),
           getMethod("median", signature(x = "AffLinDistribution")))    

setMethod("mad", signature(x = "UnivariateDistribution"),
    function(x){
        m <- median(x)
        y <- abs(x-m) 
        return(q(y)(1/2))
    })

setMethod("mad", signature(x = "AffLinDistribution"),
    function(x) abs(x@a) * mad(x@X0)) 

setMethod("mad", signature(x = "AffLinAbscontDistribution"),
           getMethod("mad", signature(x = "AffLinDistribution")))    
setMethod("mad", signature(x = "AffLinDiscreteDistribution"),
           getMethod("mad", signature(x = "AffLinDistribution")))    
setMethod("mad", signature(x = "AffLinLatticeDistribution"),
           getMethod("mad", signature(x = "AffLinDistribution")))    

setMethod("IQR", signature(x = "UnivariateDistribution"),
    function(x){
        return(q(x)(3/4)-q(x)(1/4))
    })

setMethod("IQR", signature(x = "DiscreteDistribution"),
    function(x) q.r(x)(3/4)-q(x)(1/4)
)

setMethod("IQR", signature(x = "AffLinDistribution"),
    function(x) abs(x@a) * IQR(x@X0)) 

setMethod("IQR", signature(x = "AffLinAbscontDistribution"),
           getMethod("IQR", signature(x = "AffLinDistribution")))    
setMethod("IQR", signature(x = "AffLinDiscreteDistribution"),
           getMethod("IQR", signature(x = "AffLinDistribution")))    
setMethod("IQR", signature(x = "AffLinLatticeDistribution"),
           getMethod("IQR", signature(x = "AffLinDistribution")))    

##standardization
make01 <- function(x){
    if (!is(x, "UnivariateDistribution"))
        stop("This function is for univariate distribution objects.")
    return((x-E(x))/sd(x))
    }


#################################################################
# some exact variances:
#################################################################
setMethod("var", signature(x = "Norm"),
    function(x,...){ 
    if((hasArg(fun))||(hasArg(cond)))
       return(var(as(x,"AbscontDistribution"),...))
    else
        return(sd(x)^2)
    })

setMethod("var", signature(x = "Binom"),
    function(x,...){
    if((hasArg(fun))||(hasArg(cond)))
        return(var(as(x,"DiscreteDistribution"),...))
    else
        return(size(x)*prob(x)*(1-prob(x)))
    })

setMethod("var", signature(x = "Cauchy"),
    function(x,...){    
    if((hasArg(fun))||(hasArg(cond)))
      return(var(as(x,"AbscontDistribution"),...))
    else
        return(NA)
    })

setMethod("var", signature(x = "Chisq"),
    function(x,...){    
    if((hasArg(fun))||(hasArg(cond)))
       return(var(as(x,"AbscontDistribution"),...))
    else
        return(2*(df(x)+2*ncp(x)))
    })

setMethod("var", signature(x = "Dirac"),
    function(x, ...){return(0)})


setMethod("var", signature(x = "DExp"),
    function(x, ...){    
    if((hasArg(fun))||(hasArg(cond))) 
         return(var(as(x,"AbscontDistribution"),...))
    else
        return(2/rate(x)^2)
    })

setMethod("var", signature(x = "Exp"),
    function(x, ...){    
    if((hasArg(fun))||(hasArg(cond))) 
         return(var(as(x,"AbscontDistribution"),...))
    else
        return(1/rate(x)^2)
    })


setMethod("var", signature(x = "Fd"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))) 
         return(var(as(x,"AbscontDistribution"),...))
    else
        {df1 <- df1(x)
         df2 <- df2(x)
         d <- ncp(x)
         Ex2 <- (E(x))^2 
         Exx <- df2^2/(df2-2)/(df2-4)*((df1+d)^2+2*df1+4*d)/df1^2
        return(ifelse(df2>4,Exx-Ex2, NA ))}
    })

setMethod("var", signature(x = "Gammad"),
    function(x, ...){    
    if((hasArg(fun))||(hasArg(cond))) 
         return(var(as(x,"AbscontDistribution"),...))
    else
        return(shape(x)*scale(x)^2)
    })

setMethod("var", signature(x = "Geom"),
    function(x, ...){    
    if((hasArg(fun))||(hasArg(cond))) 
         return(var(as(x,"DiscreteDistribution"),...))
    else {p <- prob(x); e <- 1/p-1; return(e+e^2)}
    })

setMethod("var", signature(x = "Hyper"),
    function(x, ...){    
    if((hasArg(fun))||(hasArg(cond))) 
         return(var(as(x,"DiscreteDistribution"),...))
    else
       {k <- k(x);
        m <- m(x); 
        n <- n(x);
        return(k*n/(m+n)*m/(m+n)*(m+n-k)/(m+n-1))}
    })

setMethod("var", signature(x = "Logis"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))) 
        return(var(as(x,"AbscontDistribution"),...))
    else
        return(pi^2/3*scale(x)^2)
    })

setMethod("var", signature(x = "Lnorm"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))) 
        return(var(as(x,"AbscontDistribution"),...))
    else
        return(exp(2*meanlog(x)+sdlog(x)^2)*(exp(sdlog(x)^2)-1))
    })

setMethod("var", signature(x = "Nbinom"),
    function(x, ...){    
    if((hasArg(fun))||(hasArg(cond))) 
         return(var(as(x,"DiscreteDistribution"),...))
    else {p <- prob(x); e <- 1/p-1; return(size(x)*(e+e^2))}
    })

setMethod("var", signature(x = "Pois"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))) 
        return(var(as(x,"DiscreteDistribution"),...))
    else
        return(lambda(x))
    })

setMethod("var", signature(x = "Td"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))) 
        return(var(as(x,"AbscontDistribution"),...))
    else
        {n <- df(x); d<- ncp(x)
        ## correction thanks to G.Jay Kerns
        return(ifelse( n>2, n/(n-2)+
               d^2*(n/(n-2)-n/2*exp(lgamma((n-1)/2)-lgamma(n/2))^2), NA))
       }
    })


setMethod("var", signature(x = "Unif"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))) 
        return(var(as(x,"AbscontDistribution"),...))
    else
        return((Max(x)-Min(x))^2/12)
    })

setMethod("var", signature(x = "Weibull"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))) 
        return(var(as(x,"AbscontDistribution"),...))
    else
        return(scale(x)^2*(gamma(1+2/shape(x))- (gamma(1 + 1/shape(x)))^2))
    })
    
setMethod("var", signature(x = "Beta"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))||(!isTRUE(all.equal(ncp(x),0)))) 
        return(var(as(x,"AbscontDistribution"),...))
    else
        {a<-shape1(x); b<- shape2(x)
        return(a*b/(a+b)^2/(a+b+1))}
    })

setMethod("var", signature(x = "Arcsine"),
    function(x, ...)return(1/2))

#################################################################
# some exact medians
#################################################################

setMethod("median", signature(x = "Norm"),
    function(x) mean(x))

setMethod("median", signature(x = "Cauchy"),
    function(x) location(x))

setMethod("median", signature(x = "Dirac"),
    function(x) location(x))

setMethod("median", signature(x = "DExp"),
    function(x) 0)

setMethod("median", signature(x = "Exp"),
    function(x) log(2)/rate(x))

setMethod("median", signature(x = "Geom"),
    function(x) ceiling(-log(2)/log(1-prob(x))-1))

setMethod("median", signature(x = "Logis"),
    function(x) location(x))

setMethod("median", signature(x = "Lnorm"),
    function(x) exp(meanlog(x)))

setMethod("median", signature(x = "Unif"),
    function(x) (Min(x)+Max(x))/2)

setMethod("median", signature(x = "Arcsine"),
    function(x) 0)

#################################################################
# some exact IQRs
#################################################################

setMethod("IQR", signature(x = "Norm"),
    function(x) 2*qnorm(3/4)*sd(x))

setMethod("IQR", signature(x = "Cauchy"),
    function(x) 2*scale(x))

setMethod("IQR", signature(x = "Dirac"),
    function(x) 0)

setMethod("IQR", signature(x = "DExp"),
    function(x) 2*log(2)/rate(DExp))

setMethod("IQR", signature(x = "Exp"),
    function(x) (log(4)-log(4/3))/rate(x))

setMethod("IQR", signature(x = "Geom"),
    function(x) ceiling(log(1/4)/log(1-prob(x)))-
                max(floor(log(3/4)/log(1-prob(x))),0))

setMethod("IQR", signature(x = "Logis"),
    function(x) 2*log(3)*scale(x))

setMethod("IQR", signature(x = "Unif"),
    function(x) (Max(x)-Min(x))/2)

setMethod("IQR", signature(x = "Arcsine"),
    function(x) sqrt(2))

#################################################################
# some exact mads
#################################################################

setMethod("mad", signature(x = "Norm"),
    function(x) qnorm(3/4)*sd(x))

setMethod("mad", signature(x = "Cauchy"),
    function(x)  scale(x))

setMethod("mad", signature(x = "Dirac"),
    function(x) 0)

setMethod("mad", signature(x = "DExp"),
    function(x) log(2)/rate(DExp))

setMethod("mad", signature(x = "Exp"),
    function(x) log((1+sqrt(5))/2)/rate(x))

setMethod("mad", signature(x = "Geom"),
    function(x) {p <- prob(x); pq <-  1-p
                 m <- median(x); rho <- 1/2*pq^(-m)
                 max(ceiling(-log(rho/2+sqrt(pq+rho^2/4))/log(pq)),0)
                 })

setMethod("mad", signature(x = "Logis"),
    function(x) log(3)*scale(x))

setMethod("mad", signature(x = "Unif"),
    function(x) (Max(x)-Min(x))/4)

setMethod("mad", signature(x = "Arcsine"),
    function(x) sqrt(1/2))
