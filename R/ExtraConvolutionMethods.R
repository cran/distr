##setMethod("+", c("AbscontDistribution","DiscreteDistribution"),
##          function(e1,e2){
##            rnew = function(n) r(e1)(n) + r(e2)(n)
##            new("AbscontDistribution", r = rnew)
##          })


##setMethod("+", c("DiscreteDistribution","AbscontDistribution"),
##          function(e1,e2){
##            rnew = function(n) r(e1)(n) + r(e2)(n)
##            new("AbscontDistribution", r = rnew)
##          })




setMethod("+", c("numeric","UnivariateDistribution"),
          function(e1, e2){
            e2 + e1
          })

setMethod("-", c("numeric","UnivariateDistribution"),
          function(e1, e2){
            -1*e2 + e1
          })

setMethod("*", c("numeric","UnivariateDistribution"),
          function(e1, e2){
            e2 * e1
          })

setMethod("-", c("UnivariateDistribution","UnivariateDistribution"),
          function(e1,e2){
            e1+(-e2)
          })
setMethod("-", "UnivariateDistribution",
          function(e1, e2){
            e1*(-1)
          })


setMethod("-", c("UnivariateDistribution","numeric"),
          function(e1, e2){
            return(e1 + (-e2))
          })

setMethod("/", c("UnivariateDistribution","numeric"),
          function(e1, e2){
            if(e2 == 0) stop("division by zero")
            return(e1 * (1/e2))
          })


setMethod("+", c("AbscontDistribution","DiscreteDistribution"),
          function(e1,e2){
            return(e2 + e1)
          })


setMethod("+", c("DiscreteDistribution","AbscontDistribution"),
          function(e1,e2){
            rnew <- function(n) r(e1)(n) + r(e2)(n)
            
            grid <- support(e1)
            probab <- d(e1)(grid)

            lower1 <- min(grid)
            upper1 <- max(grid)
            lower2 <- ifelse((q(e2)(0) > - Inf), q(e2)(0), q(e2)(TruncQuantile))
            upper2 <- ifelse((q(e2)(1) < Inf), q(e2)(1), q(e2)(1 - TruncQuantile))
            
            lower = lower1 + lower2
            upper = upper1 + upper2
            approxgrid = seq(from = lower, to = upper, length = DefaultNrGridPoints)
                        
            dnew <- function(x) sum(probab * d(e2)(x - grid))
            dnew2 <- function(x) sapply(x, dnew) / sum(probab)
            newd <- dnew2(approxgrid)
            dnew3 <- approxfun(approxgrid, newd, yleft = 0, yright = 0)
            ## integrate the piecewise linear function dnew3 
            standardizer <- sum(newd[1:(length(newd) - 1)] + newd[2:length(newd)]) / 2 * (approxgrid[2] - approxgrid[1])
            dnew4 <- function(x) dnew3(x) / standardizer

            pnew <- function(x) sum(probab * p(e2)(x - grid))
            pnew2 <- function(x) sapply(x, pnew) / sum(probab)

            pvalues <- pnew2(approxgrid)
            pnew3 <- approxfun(approxgrid, pvalues/max(pvalues), yleft = 0, yright = 1)            
            
            w0 <- options("warn")
            options(warn = -1)

            qfun1 <- approxfun(x = pnew2(approxgrid), y = approxgrid, yleft = lower, yright = upper)
            qfun2 <- function(x){ 
              ind1 <- (x == 0)*(1:length(x))
              ind2 <- (x == 1)*(1:length(x))
              y <- qfun1(x)
              y <- replace(y, ind1[ind1 != 0], lower)
              y <- replace(y, ind2[ind2 != 0], upper)
              return(y)
            }

            options(w0)

            return(new("AbscontDistribution", r = rnew, d = dnew4, p = pnew3, q = qfun2))
          }) 
