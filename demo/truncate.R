require(distr)

if(!isGeneric("Truncate")) 
    setGeneric("Truncate", 
       function(object, lower, upper) 
                standardGeneric("Truncate")
                )

setMethod("Truncate",
          signature(object = "AbscontDistribution", 
          lower = "numeric", upper = "numeric"),
          function(object, lower, upper){
            ## new random number function
            rnew = function(n){
              rn = r(object)(n)
              while(TRUE){
                rn[rn < lower] = NA
                rn[rn > upper] = NA
                index = is.na(rn)
                if(!(any(index))) break
                rn[index] = r(object)(sum(index))
              }
              rn
            }

            ## new cdf 
            plower = p(object)(lower)
            pupper = p(object)(upper)
            pnew = function(x)
              ifelse(x < lower, 0, 
                     ifelse(x >= upper, 1, 
                     (p(object)(x) - plower)/(pupper - plower)))

            ## new density 
            lostmass = plower + 1 - pupper

            dnew = function(x) 
              ifelse(x < lower, 0, 
                     ifelse(x >= upper, 0, 
                         d(object)(x)/(1-lostmass)))

            # new quantile
            qfun1 <- function(x){
              if(x == 0) return(lower)
              if(x == 1) return(upper)
              fun <- function(t) pnew(t) - x
              uniroot(f = fun, interval = c(lower, upper))$root
            }
            qfun2 <- function(x)
              sapply(x, qfun1)

            return(new("AbscontDistribution", r = rnew, 
                   d = dnew, p = pnew, q = qfun2))            
          })

# Example
# Normal(0,1)-Distribution truncated at -0.5 and 1

N = Norm()
Z = Truncate(N, -0.5, 1)

# the truncated Distribution

plot(Z)

# some truncated randomnumbers

r(Z)(10)

oldpar = par()
par(mfrow = c(1,2))


# cdf of truncated Normal-Distribution
# and of Normal-Distribution

x = seq(-1.5, 1.5, length = 1000)
plot(x, p(Z)(x),
     type = "l",
     lwd = 5,
     xlab = "",
     ylab = "CDF")
lines(x, p(N)(x),
      lwd = 2,
      col = "red")
legend("topleft",
       legend = c("N(0,1)", "N(0,1) truncated"),
       fill = c("red", "black"))

# density of truncated Normal-Distribution
# and of Normal-Distribution

x = seq(-1.5, 1.5, length = 1000)
plot(x, d(Z)(x),
     type = "l",
     lwd = 5,
     xlab = "",
     ylab = "density")
lines("topleft",
      lwd = 2,
      col = "red")
legend(-1.5,0.7, legend = c("N(0,1)", "N(0,1) truncated"),
       fill = c("red", "black"))


par(oldpar)
