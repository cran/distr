setClass("AbscontDistribution", representation(), contains = "UnivariateDistribution")

## Initialize method
setMethod("initialize", "AbscontDistribution",
          function(.Object, r = NULL, d = NULL, p = NULL, q = NULL) {
            if(is.null(r)) {
              stop("You have at least to give the slot r.")
              return(invisible())}
            
            ## TOBEDONE Errorkanal
            
            dpq.approx <- 0
            
            dfun <- d
            pfun <- p
            qfun <- q
            
            if(is.null(d)) {
              dpq <- RtoDPQ(r)
              dpq.approx <- 1
              dfun <- dpq$dfun}
            
            if(is.null(p)) {
              if(dpq.approx == 0) {dpq <- RtoDPQ(r)}
              dpq.approx <- 1
              pfun <- dpq$pfun}
            
            if(is.null(q)) {
              if(dpq.approx == 0) {dpq <- RtoDPQ(r)}
              qfun <- dpq$qfun}
            
            .Object@img <- new("Reals")
            .Object@param <- NULL
            .Object@d <- dfun
            .Object@p <- pfun
            .Object@q <- qfun
            .Object@r <- r
            .Object })


setMethod("+", c("AbscontDistribution","AbscontDistribution"),
          function(e1,e2){
            n <- DefaultNrFFTGridPointsExponent

            lower1 <- ifelse((q(e1)(0) > - Inf), q(e1)(0), q(e1)(TruncQuantile))
            lower2 <- ifelse((q(e2)(0) > - Inf), q(e2)(0), q(e2)(TruncQuantile))
            upper1 <- ifelse((q(e1)(1) < Inf), q(e1)(1), q(e1)(1 - TruncQuantile))
            upper2 <- ifelse((q(e2)(1) < Inf), q(e2)(1), q(e2)(1 - TruncQuantile))
            
            lower <- min(lower1, lower2)
            upper <- max(upper1, upper2)

            h <- (upper-lower)/2^n
            if(h > 0.01)
              warning("Grid for approxfun too wide, increase DefaultNrFFTGridPointsExponent")
            x <- seq(from = lower, to = upper, by = h)
            pe1 <- p(e1)(x)
            pe2 <- p(e2)(x)
            pe1 <- pe1[2:(2^n + 1)] - pe1[1:2^n]
            pe2 <- pe2[2:(2^n + 1)] - pe2[1:2^n]
            
            x <- seq(from = 2*lower, to = 2*upper, by = h)
            pe1 <- c(pe1, numeric(2^n))
            pe2 <- c(pe2, numeric(2^n))
            ## computation of DFT
            ftpe1 <- fft(pe1)
            ftpe2 <- fft(pe2)
            
            ## convolution theorem for DFTs
            newd <- Re(fft(ftpe1*ftpe2, inverse = TRUE)) / length(ftpe1)
            newd <- (abs(newd) >= .Machine$double.eps)*newd
            newd <- c(0,newd)
            newd1 <- newd / h
            
            ## density
            dfun1 <- approxfun(x = x, y = newd1, yleft = 0, yright = 0)
            standardizer <- sum(newd[2:2^(n+1)]) + (newd[1]+newd[2^(n+1)+1])/2
            dfun2 <- function(x) dfun1(x) / standardizer
            
            ## cdf
            newp <- cumsum(newd)
            ## continuity correction by h/2
            pfun1 <- approxfun(x = x+0.5*h, y = newp, yleft = 0, yright = newp[2^(n+1)+1])
            pfun2 <- function(x) pfun1(x) / newp[2^(n+1)+1]

            ## quantile
            yleft <- ifelse(((q(e1)(0) == -Inf)|(q(e2)(0) == -Inf)), -Inf, lower1+lower2)
            yright <- ifelse(((q(e1)(1) == Inf)|(q(e2)(1) == Inf)), Inf, upper1+upper2)
            w0 <- options("warn")
            options(warn = -1)
            ## continuity correction by h/2
            qfun1 <- approxfun(x = pfun2(x+0.5*h), y = x+0.5*h, yleft = yleft, yright = yright)
            qfun2 <- function(x){ 
              ind1 <- (x == 0)*(1:length(x))
              ind2 <- (x == 1)*(1:length(x))
              y <- qfun1(x)
              y <- replace(y, ind1[ind1 != 0], yleft)
              y <- replace(y, ind2[ind2 != 0], yright)
              return(y)
            }
            options(w0)
            
            rnew <- function(n) r(e1)(n) + r(e2)(n)
            
            object <- new("AbscontDistribution", r = rnew, d = dfun2, p = pfun2, q = qfun2)
            body(object@r) <- substitute({ f(n) + g(n) },
                                         list(f = e1@r, g = e2@r))
            object
          })




###setMethod("m1df", "AbscontDistribution",
###          function(object){
###            lower <- q(object)(TruncQuantile)
###            upper <- q(object)(1 - TruncQuantile)
###            
###            gitter.x <- seq(from = lower, to = upper, length = DefaultNrGridPoints)
###            
###           integrand <- function(x) x * d(object)(x)
###            
###            tmp <- function(t) integrate(integrand, lower = lower, upper = t)$value
###            
###            gitter.y <- sapply(gitter.x, tmp)
###            
###            approxfun(gitter.x, gitter.y, rule = 2)
###          })


###setMethod("m2df", "AbscontDistribution", 
###          function(object){
###            lower <- q(object)(TruncQuantile)
###            upper <- q(object)(1 - TruncQuantile)
###            
###            gitter.x <- seq(from = lower, to = upper, length = DefaultNrGridPoints)
###            
###            integrand <- function(x) x^2 * d(object)(x)
###            
###            tmp <- function(t) integrate(integrand, lower = lower, upper = t)$value
###            
###            gitter.y <- sapply(gitter.x, tmp)
###            
###            approxfun(gitter.x, gitter.y, rule = 2)
###          })

## binary operators for absolut continuous distributions

## extra methods
## binare operators

setMethod("+", c("AbscontDistribution","numeric"),
          function(e1, e2){
            rnew <- function(n){ e1@r(n) + e2 }
            dnew <- function(x){ e1@d(x - e2) }
            pnew <- function(x){ e1@p(x - e2) }
            qnew <- function(x){ e1@q(x) + e2 }
            
            object <- new("AbscontDistribution", r = rnew, d = dnew, p = pnew, q = qnew)            
            body(object@r) <- substitute({ f(n) + g },
                                         list(f = e1@r, g = e2))
            object
          })


setMethod("*", c("AbscontDistribution","numeric"),
          function(e1, e2){
            if(e2 == 0) return(new("Dirac", location = 0))
            rnew <- function(n){ e1@r(n) * e2 }
            dnew <- function(x){ e1@d(x / e2) / abs(e2) }
            pnew <- function(x){ e1@p(x / e2) * sign(e2) + (1-sign(e2))/2}
            qnew <- function(x){ e1@q((1-sign(e2))/2+sign(e2)*x) * e2 }
            
            object <- new("AbscontDistribution", r = rnew, d = dnew, p = pnew, q = qnew)
            body(object@r) <- substitute({ f(n) * g },
                                         list(f = e1@r, g = e2))
            object
          })

## Gruppe Math für absolutstetige
setMethod("Math", "AbscontDistribution",
          function(x){
            expr <- parse(text=sys.call(),n=1)
            fun <- eval(expr)
            rnew <- function(n){ fun(x@r(n)) }
            object <- new("AbscontDistribution", r = rnew)
            body(object@r) <- substitute({ f(g(n)) },
                                         list(f = as.name(.Generic), g = x@r))
            object
          })


###Plot

setMethod("plot","AbscontDistribution",
          function(x,y=NULL,...){
            opar <- par(mfrow = c(1,3))

            slots = slotNames(param(x))
            slots = slots[slots != "name"]
            nrvalues = length(slots)
            if(nrvalues > 0){
              values = numeric(nrvalues)
              for(i in 1:nrvalues)
                values[i] = attributes(attributes(x)$param)[[slots[i]]]
              paramstring = paste("(", paste(values, collapse = ", "), ")", sep = "")
            }
            else paramstring = ""

            if(is.finite(q(x)(0))) lower <- q(x)(0)
            else lower <- q(x)(TruncQuantile)
   
            if(is.finite(q(x)(1))) upper <- q(x)(1)
            else upper <- q(x)(1 - TruncQuantile)

            dist = upper - lower
            grid <- seq(from = lower - 0.1 * dist, to = upper + 0.1 * dist, length = 1000)
            plot(grid, d(x)(grid), type = "l", main = paste("Density of ", class(x)[1], paramstring), ...)
            plot(grid, p(x)(grid), type = "l", main = paste("CDF of ", class(x)[1], paramstring), ...)
            plot(p(x)(grid), grid, type = "l", main = paste("Quantile of ", class(x)[1], paramstring), ...)
            par(opar)
          }
          )
