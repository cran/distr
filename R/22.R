## extra methods
## binary operators




if(!isGeneric("simplifyr")) setGeneric("simplifyr", function(object, 
                 size = 10^getdistrOption("RtoDPQ.e")) standardGeneric("simplifyr"))

setMethod("simplifyr", "UnivariateDistribution", 
          function(object, size = 10^getdistrOption("RtoDPQ.e")){
            Sample <- r(object)(size)       
            rneu <- function(n) sample(x = Sample, size = n, replace = TRUE)
            eval.parent(substitute(object@r<-rneu))           
          })


## function to automatically generate, starting from simulations, density, 
## quantile function and cdf
## first version for absolutely continuous, second for discrete distributions

## we use 10^RtoDPQExponent random numbers to generate new distr
## density should use DefaultNrGridPoints equally spaced points for evaluation

RtoDPQ <- function(r, e = getdistrOption("RtoDPQ.e"), n = getdistrOption("DefaultNrGridPoints")){
  zz <- r(10^e)
  dfun <- approxfun(density(zz, n = n), yleft = 0, yright = 0)
  
  pfun0 <- ecdf(zz)
  pfun <- function(x) pfun0(x)
  pfunx <- seq(from = min(zz), to = max(zz), length = n)
  pfuny <- pfun(pfunx)

  pfunx <- tapply(pfunx, pfuny, min)
  pfuny <- unique(pfuny)
  
  qfun <- approxfun(x = pfuny, y = pfunx, rule = 2)
  
  qfun1 <- function(x) ifelse(x > 1, NA,
                              ifelse(x < 0, NA, qfun(x))
                              )
  list(dfun = dfun, pfun = pfun, qfun = qfun1)}

RtoDPQ.d <- function(r, e = getdistrOption("RtoDPQ.e")){
  zz <- r(10^e)
  X <- table(zz)
  dim.X <- length(X)
  gridpoints <- as.numeric(names(X))
  gridmasses <- X/(10^e)
  
  len = length(gridpoints)
  
  if(len > 1){
    if(min(gridpoints[2:len] - gridpoints[1:(len - 1)]) < getdistrOption("DistrResolution"))
       stop("grid too narrow --> change DistrResolution")
  }
  
  intervall <- getdistrOption("DistrResolution") / 2  
  
  grid.xx <- as.numeric(matrix(rbind(gridpoints - intervall, gridpoints + intervall), nrow = 1))
  grid.yy <- c(as.numeric(matrix(rbind(0, gridmasses), nrow = 1)), 0)
  d <- stepfun(x = grid.xx, y = grid.yy)
  dfun <- function(x) d(x)
  
  pfun0 <- ecdf(zz)
  pfun <- function(x) pfun0(x)
  
  cumprob <- cumsum(gridmasses)
  qnew <- function(q) gridpoints[sum(cumprob<q)+1]
  qfun <- function(x) sapply(x, qnew)
  list(dfun = dfun, pfun = pfun, qfun = qfun)
}

###########################################################



###Functions for AbscontDistribution 


#determines slot d from p
P2D <- function(p, x, ngrid = getdistrOption("DefaultNrGridPoints"))
{xx <- seq(x[1],x[2],length = ngrid)
 px <- p(xx)
 dx <- c(px[1],diff(px))/(xx[2]-xx[1])
# later with sfsmisc dx <- D1ss(xx,px)
 approxfun(xx,dx, yleft = 0, yright = 0)
}


#determines slot q from p

P2Q <- function(p,x,ngrid=getdistrOption("DefaultNrGridPoints")){
xx0 <- seq(x[1],x[2],length=ngrid)
px0 <- p(xx0)
px <- unique(px0)
xx <- tapply(xx0, px0, min)
approxfun(x = px, y = xx, rule = 2)
}
