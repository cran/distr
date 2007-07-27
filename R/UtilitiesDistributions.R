## extra methods
## binary operators


if(!isGeneric("simplifyr")) 
    setGeneric("simplifyr", 
                function(object, 
                         size = 10^getdistrOption("RtoDPQ.e")) 
                         standardGeneric("simplifyr")
              )

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

RtoDPQ <- function(r, e = getdistrOption("RtoDPQ.e"),
                      n = getdistrOption("DefaultNrGridPoints")){
  zz <- r(10^e)

  dxy <-  xy.coords(density(zz, n = n))
  dfun <- .makeDNew(dxy$x, dxy$y, standM = "int")

  pf0 <- function(x, y, yleft, yright) ecdf(x)
  pfun <- .makePNew(x=zz, dx=0, notwithLLarg=TRUE, myPf = pf0)
            ## quantile function

  yL <-  min(zz);   yR <-  max(zz); rm(zz)
  px.l <- pfun(dxy$x);   px.u <- pfun(dxy$x, lower.tail = FALSE)
  qfun <- .makeQNew(dxy$x, px.l, px.u, TRUE, yL, yR)

  rm(px.l, px.u, dxy, pf0)
  list(dfun = dfun, pfun = pfun, qfun = qfun)}


RtoDPQ.d <- function(r, e = getdistrOption("RtoDPQ.e")){
  zz <- r(10^e)
  X <- table(zz)
  rm(zz)

  supp <- as.numeric(names(X))
  prob <- X/(10^e)
  rm(X)

  len = length(supp)

  if(len > 1){
    if(min(diff(supp)) <
           getdistrOption("DistrResolution"))
       stop("grid too narrow --> change DistrResolution")
  }

  dfun <- .makeDNew(supp, prob, Cont = FALSE)
  pfun <- .makePNew(supp, prob, TRUE, Cont = FALSE)
  qfun <- .makeQNew(supp, cumsum(prob), rev(cumsum(rev(prob))),
                      TRUE, min(supp), max(supp), Cont = FALSE)

  list(dfun = dfun, pfun = pfun, qfun = qfun)
}

###########################################################



###Functions for AbscontDistribution 


#determines slot d from p
P2D <- function(p, x, ngrid = getdistrOption("DefaultNrGridPoints"))
{xx <- seq(x[1], x[2], length = ngrid)
 px <- p(xx)
 dx <- c(px[1], diff(px))/(xx[2]-xx[1]) 
# later with sfsmisc dx <- D1ss(xx,px)
 approxfun(xx, dx, yleft = 0, yright = 0)
}


#determines slot q from p

P2Q <- function(p, x, ngrid = getdistrOption("DefaultNrGridPoints")){
xx0 <- seq(x[1], x[2], length = ngrid)
px0 <- p(xx0)
px <- unique(px0)
xx <- tapply(xx0, px0, min)
approxfun(x = px, y = xx, rule = 2)
}

#determines slot p from d
D2P <- function(d, x, ngrid = getdistrOption("DefaultNrGridPoints"))
{xx <- seq(x[1], x[2], length = ngrid)
 px <- sapply(xx, function(y) {
           tr <- try(integrate(d, lower = x[1], upper = y), silent = TRUE) 
           if (!is(tr, "try-error")) return(tr$value)
           else return((xx[2]-xx[1]) * sum(d(xx[xx<y])))})
 approxfun(xx, px, yleft = 0, yright = 1)
}
