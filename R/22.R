## extra methods
## binare operators

## geklippte erste und zweite Momente einer Verteilung

###if(!isGeneric("m1df")) setGeneric("m1df", function(object) standardGeneric("m1df"))
###if(!isGeneric("m2df")) setGeneric("m2df", function(object) standardGeneric("m2df"))



if(!isGeneric("simplifyr")) setGeneric("simplifyr", function(object, size = 10^5) standardGeneric("simplifyr"))

setMethod("simplifyr", "UnivariateDistribution", 
          function(object, size = 10^5){
            Sample <- r(object)(size)       
            rneu <- function(n) sample(x = Sample, size = n, replace = TRUE)
            eval.parent(substitute(object@r<-rneu))           
          })


## Funktion um aus den Zufallszahlen einer Verteilung die Dichte, die Quantilsfunktion und die Vtlg zu bestimmen
## erste Version für absolutstetige, zweite für diskrete

## we use 10^RtoDPQ.e random numbers to generate new distr
# RtoDPQ.e <- 5 (GlobalVariable)
## density should use DefaultNrGridPoints equally spaced points for evaluation

RtoDPQ <- function(r, e = RtoDPQ.e, n = DefaultNrGridPoints){
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

RtoDPQ.d <- function(r, e = RtoDPQ.e){
  zz <- r(10^e)
  X <- table(zz)
  dim.X <- length(X)
  gitterpunkte <- as.numeric(names(X))
  gittermassen <- X/(10^e)

  
  len = length(gitterpunkte)

  if(len > 1){
    if(min(gitterpunkte[2:len] - gitterpunkte[1:(len - 1)]) < DistrResolution)
       stop("grid too narrow --> change DistrResolution")
  }
  
  intervall <- DistrResolution / 2  
  
  gitter.xx <- as.numeric(matrix(rbind(gitterpunkte - intervall, gitterpunkte + intervall), nrow = 1))
  gitter.yy <- c(as.numeric(matrix(rbind(0, gittermassen), nrow = 1)), 0)
  d <- stepfun(x = gitter.xx, y = gitter.yy)
  dfun <- function(x) d(x)
  
  pfun0 <- ecdf(zz)
  pfun <- function(x) pfun0(x)
  
  cumprob <- cumsum(gittermassen)
  qnew <- function(q) gitterpunkte[sum(cumprob<q)+1]
  qfun <- function(x) sapply(x, qnew)
  list(dfun = dfun, pfun = pfun, qfun = qfun)
}

