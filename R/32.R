################################
###
### Class: DiscreteParameter
###
################################

setClass("DiscreteParameter", representation(), contains = "UnivariateParameter")

## Initialisierungsmethode
setMethod("initialize", "DiscreteParameter",
          function(.Object) {
            .Object@name <- "Parameter of a Discrete Distribution" 
            .Object
          })

setClass("DiscreteDistribution", representation(support = "numeric"), contains = "UnivariateDistribution")

## Initialize method
setMethod("initialize", "DiscreteDistribution",
          function(.Object, r = NULL, d = NULL, p = NULL, q = NULL, support = NULL) {
            if(is.null(r)) {
              stop("You have at least to give the slot r.")
              return(invisible())}
            
            if(is.null(support)) .Object@support <- as.numeric(names(table(r(10^6))))
            else .Object@support <- support
            
            len = length(support)

            if(len > 1){
              if(min(support[2:len] - support[1:(len - 1)]) < DistrResolution)
                stop("grid too narrow --> change DistrResolution")
            }
            
            dpq.approx <- 0
            
            dfun <- d
            pfun <- p
            qfun <- q
            
            if(is.null(d)) {
              dpq <- RtoDPQ.d(r)
              dpq.approx <- 1
              dfun <- dpq$dfun
            }
            
            if(is.null(p)) {
              if(dpq.approx==0) dpq <- RtoDPQ.d(r)
              dpq.approx <- 1
              pfun <- dpq$pfun
            }
            
            if(is.null(q)) {
              if(dpq.approx==0) dpq <- RtoDPQ.d(r)
              qfun <- dpq$qfun
            }
            
            .Object@img <- new("Reals")
            .Object@param <- new("DiscreteParameter")
            .Object@d <- dfun
            .Object@p <- pfun
            .Object@q <- qfun
            .Object@r <- r
            .Object
          })


if(!isGeneric("support")) setGeneric("support", function(object) standardGeneric("support"))
setMethod("support", "DiscreteDistribution", function(object) object@support)


## Faltung Discrete Verteilungen

setMethod("+", c("DiscreteDistribution","DiscreteDistribution"),
          function(e1,e2){
            convolutedsupport <- rep(support(e1), each = length(support(e2))) + support(e2)          
            
            gridvalues1 <- d(e1)(support(e1))
            gridvalues2 <- d(e2)(support(e2))
            
            convolutedvalues <- rep(gridvalues1, each = length(support(e2))) * gridvalues2
            
            tmptable <- data.frame(x = convolutedsupport, dx = convolutedvalues)
            
            tmp <- tapply(tmptable$dx, tmptable$x, sum)
            
            supportnew <- as.numeric(names(tmp))


            len = length(supportnew)

            if(len > 1){
              if(min(supportnew[2:len] - supportnew[1:(len - 1)]) < DistrResolution)
                stop("grid too narrow --> change DistrResolution")
            }

            valuesnew <- as.numeric(tmp)
            
            intervall <- DistrResolution / 2              
            
            grid.xx <- as.numeric(matrix(rbind(supportnew - intervall, supportnew + intervall), nrow = 1))
            grid.yy <- c(as.numeric(matrix(rbind(0,valuesnew), nrow = 1)),0)
            dnew <- stepfun(x = grid.xx, y = grid.yy)
            dfun <- function(x) dnew(x)
            
            cumprob <- cumsum(valuesnew)
            
            pnew <- stepfun(x = supportnew, y = c(0,cumprob))
            pfun <- function(x) pnew(x)
            
            
            qnew <- function(q) supportnew[sum(cumprob<q)+1]
            qfun <- function(x) sapply(x, qnew)       
            
            rfun <- function(n) r(e1)(n) + r(e2)(n)
            
            new("DiscreteDistribution", r = rfun, d = dfun, p = pfun, q = qfun, support = supportnew)
          })

## extra methods
## binare operators

setMethod("+", c("DiscreteDistribution","numeric"),
          function(e1, e2){
            supportnew <- support(e1) + e2
            rnew <- function(n){ e1@r(n) + e2 }
            dnew <- function(x){ e1@d(x - e2) }
            pnew <- function(x){ e1@p(x - e2) }
            qnew <- function(x){ e1@q(x) + e2 }
            
            new("DiscreteDistribution", r = rnew, d = dnew, p = pnew, q = qnew, support = supportnew) 
          })

setMethod("*", c("DiscreteDistribution","numeric"),
          function(e1, e2){
            if(e2 == 0) return(new("Dirac", location = 0))
            supportnew <- support(e1) * e2
            if(e2 < 0) supportnew <- supportnew[length(supportnew):1]
            rnew <- function(n){ e1@r(n) * e2 }
            dnew <- function(x){ e1@d(x / e2) }
            pnew <- function(x){ e1@p(x / e2) * sign(e2) + (1-sign(e2)) * (1+e1@d(x / e2))/2}
            qnew1 <- function(x){ e1@q((1-sign(e2))/2+sign(e2)*x) * e2 }
            qnew2 <- function(x){  if(e2<0){
              if(is.nan(qnew1(x))==FALSE){
                if(p(e1)(qnew1(x))==x)
                  {if(x!=0) {x=x-DistrResolution}}}}
                                  qnew1(x)}    
            new("DiscreteDistribution", r = rnew, d = dnew, p = pnew, q = qnew2, support = supportnew)
          })

## Gruppe Math für diskrete Verteilungen
setMethod("Math", "DiscreteDistribution",
          function(x){
            expr = parse(text=sys.call(),n=1)
            fun = eval(expr)
            rnew = function(n){fun(x@r(n)) }
            new("DiscreteDistribution", r = rnew)
          })


###Plot

setMethod("plot","DiscreteDistribution",
          function(x,y=NULL,...){
            w0 <- options("warn")
            options(warn = -1)
            opar <- par()
            par(mfrow = c(1,3))

            slots = names(getSlots(class(param(x))))
            slots = slots[slots != "name"]
            nrvalues = length(slots)
            if(nrvalues > 0){
              values = 0
              for(i in 1:nrvalues)
                values[i] = attributes(attributes(x)$param)[[slots[i]]]
              paramstring = paste("(", paste(values, collapse = ", "), ")", sep = "")
            }
            else paramstring = ""

            plot(support(x), d(x)(support(x)), type = "h", main = paste("Density of ", class(x)[1], paramstring), ...)
            points(support(x), d(x)(support(x)), pch = 16)
            lower <- min(support(x))
            upper <- max(support(x))
            dist = upper - lower
            grid <- seq(from = lower - 0.1 * dist, to = upper + 0.1 * dist, length = 1000)
            plot(grid, p(x)(grid), type = "l", main = paste("CDF of ", class(x)[1], paramstring), ...)
            plot(p(x)(grid), grid, type = "l", main = paste("Quantile of ", class(x)[1], paramstring), ...)
            par(opar)
            options(w0)
          }
          )
