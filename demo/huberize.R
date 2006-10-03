require(distr)

if(!isGeneric("Huberize")) 
    setGeneric("Huberize", 
       function(object, lower, upper) 
                standardGeneric("Huberize")
                )

setMethod("Huberize",
          signature(object = "AbscontDistribution", 
          lower = "numeric", upper = "numeric"),
          function(object, lower, upper){
            ## new random number function
            rnew = function(n){
              rn = r(object)(n)
              ifelse(rn < lower, lower, 
                     ifelse(rn >= upper, upper, rn))
            }

            ## new cdf 
            pnew = function(x)
              ifelse(x < lower, 0, 
                     ifelse(x >= upper, 1, p(object)(x)))

            ## new quantile function
            plower = p(object)(lower)
            pupper = p(object)(upper)
            qnew = function(x)
              ifelse(x < plower,
                     ifelse(x < 0, NA, -Inf),
                     ifelse(x >= pupper,
                            ifelse(x > 1, NA, upper),
                            q(object)(x)))

            new("UnivariateDistribution",r=rnew,p=pnew,q=qnew,d=NULL)
          })

# Example
# Normal(0,1)-Distribution huberized at -0.5 and 1
N = Norm()
HN = Huberize(N, -0.5, 1)

# some huberized randomnumbers
r(HN)(10)

## plot is not (yet) available for UnivariateDistributions
## which are neither a.c. nor discrete (here HN is a mixture
## of a.c. and discrete distributions)

# cdf of huberized Normal-Distribution
# and of Normal-Distribution
oldpar = par()
par(mfrow = c(1,2))

x = seq(-1.5, 1.5, length = 1000)
plot(x, p(HN)(x),
     type = "l",
     lwd = 5,
     ylab = "CDF")
lines(x, p(N)(x),
      lwd = 2,
      col = "red")
legend(-1.5,1,
       legend = c("N(0,1)", "N(0,1) huberized"),
       fill = c("red", "black"))

# quantile functions
x = seq(0, 1, length = 1000)
plot(x, q(HN)(x),
     type = "l",
     lwd = 5,
     ylab = "Quantiles",
     ylim = c(-2.5,3))
lines(x, q(N)(x),
      lwd = 2,
      col = "red")
legend(0,3,
       legend = c("N(0,1)", "N(0,1) huberized"),
       fill = c("red", "black"))

par(oldpar)