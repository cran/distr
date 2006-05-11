setClass("DiscreteDistribution", representation(support = "numeric"), 
          prototype= prototype(r = function(n){ rbinom(n, size=1, prob=0.5) },
                     d = function(x, ...){ dbinom(x, size=1, prob=0.5) },
                     p = function(x, ...){ pbinom(x, size=1, prob=0.5) },
                     q = function(x, ...){ qbinom(x, size=1, prob=0.5) },
                     img = new("Naturals"),
                     support = c(0,1),
                     param = NULL,
                     .withArith = FALSE,
                     .withSim = FALSE), contains = "UnivariateDistribution")

## Initialize method
setMethod("initialize", "DiscreteDistribution",
          function(.Object, r = NULL, d = NULL, p = NULL, q = NULL, support = NULL, 
                    param = NULL, img = new("Reals"), .withSim = FALSE, .withArith = FALSE) {
            if(is.null(r)) {
              stop("you have to specify slot r at least")
              return(invisible())}
            
            if(is.null(support)) .Object@support <- as.numeric(names(table(r(10^6))))
            else .Object@support <- support
            
            len = length(support)

            if(len > 1){
              if(min(support[2:len] - support[1:(len - 1)]) < getdistrOption("DistrResolution"))
                stop("grid too narrow --> change DistrResolution")
            }
            
            dpq.approx <- 0
            
            dfun <- d
            pfun <- p
            qfun <- q
            
            if(is.null(d)) {
              .withSim <- TRUE
              dpq <- RtoDPQ.d(r)
              dpq.approx <- 1
              dfun <- dpq$dfun
            }
            
            if(is.null(p)) {
              .withSim <- TRUE
              if(dpq.approx==0) dpq <- RtoDPQ.d(r)
              dpq.approx <- 1
              pfun <- dpq$pfun
            }
            
            if(is.null(q)) {
              .withSim <- TRUE
              if(dpq.approx==0) dpq <- RtoDPQ.d(r)
              qfun <- dpq$qfun
            }
            
            .Object@img <- img
            .Object@param <- param
            .Object@d <- dfun
            .Object@p <- pfun
            .Object@q <- qfun
            .Object@r <- r
            .Object@.withSim <- .withSim
            .Object@.withArith <- .withArith
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
              if(min(supportnew[2:len] - supportnew[1:(len - 1)]) < .distroptions$DistrResolution)
                stop("grid too narrow --> change DistrResolution")
            }

            valuesnew <- as.numeric(tmp)
            
            intervall <- getdistrOption("DistrResolution") / 2              
            
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
            
            object <- new("DiscreteDistribution", r = rfun, d = dfun, p = pfun, q = qfun, support = supportnew,
                           .withSim = FALSE, .withArith = TRUE)
            body(object@r) <- substitute({ f(n) + g(n) },
                                         list(f = e1@r, g = e2@r))
            object

          })

## extra methods
## binare operators

setMethod("+", c("DiscreteDistribution","numeric"),
          function(e1, e2){
            if (length(e2)>1) stop("length of operator must be 1")
            supportnew <- support(e1) + e2
            rnew <- function(n){ e1@r(n) + e2 }
            dnew <- function(x){ e1@d(x - e2) }
            pnew <- function(x){ e1@p(x - e2) }
            qnew <- function(x){ e1@q(x) + e2 }
            
            object <- new("DiscreteDistribution", r = rnew, d = dnew, p = pnew, q = qnew, support = supportnew,
                           .withSim = FALSE, .withArith = TRUE)
            body(object@r) <- substitute({ f(n) + g },
                                         list(f = e1@r, g = e2))
            object       
          })

setMethod("*", c("DiscreteDistribution","numeric"),
          function(e1, e2){
            if (length(e2)>1) stop("length of operator must be 1")
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
                  {if(x!=0) {x=x-getdistrOption("DistrResolution")}}}}
                                  qnew1(x)}    
            object <- new("DiscreteDistribution", r = rnew, d = dnew, p = pnew, q = qnew2, support = supportnew,
                           .withSim = FALSE, .withArith = TRUE)
            body(object@r) <- substitute({ f(n) * g },
                                         list(f = e1@r, g = e2))
            object            
          })

## Gruppe Math für diskrete Verteilungen
setMethod("Math", "DiscreteDistribution",
          function(x){
            expr = parse(text=sys.call(),n=1)
            fun = eval(expr)
            rnew = function(n){fun(x@r(n)) }
            object <- new("DiscreteDistribution", r = rnew,
                           .withSim = TRUE, .withArith = TRUE)
            body(object@r) <- substitute({ f(g(n)) },
                                         list(f = as.name(.Generic), g = x@r))
            object
          })


###Plot

setMethod("plot","DiscreteDistribution",
          function(x,y=NULL,xlim=NULL,ylim=NULL,...){
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


            lower <- min(support(x))
            upper <- max(support(x))
            dist = upper - lower
 
            supp<-support(x); 
            
            if(hasArg(xlim)) {if(length(xlim)!=2) stop("Wrong length of Argument xlim");
                              grid<- seq(xlim[1],xlim[2],length = 1000)
                              supp<-supp[(supp>=xlim[1])&(supp<=xlim[2])]         
                              }

            else {grid <- seq(from = lower - 0.1 * dist, to = upper + 0.1 * dist, length = 1000)}
  
            dx <- d(x)(supp)
     
            if(hasArg(ylim)){if(length(ylim)!=2) stop("Wrong length of Argument ylim") 
                             ylim1 <- ylim; ylim2 <- ylim}
            else {ylim1 <- c(0, max(dx)); ylim2 <- c(-0.05,1.05)}

           
            plot(supp,
                 dx,
                 type = "h",
                 main = gettextf("Density of %s%s", class(x)[1], paramstring),
                 ylim = ylim1, xlim=xlim,
                 ...)
            
            points(supp, dx, pch = 16)
            
            plot(grid, p(x)(grid), type = "l", main = gettextf("CDF of %s%s", class(x)[1], paramstring), xlim=xlim, ylim=ylim2, ...)
            plot(p(x)(grid), grid, type = "l", main = gettextf("Quantile of %s%s", class(x)[1], paramstring), xlim=ylim2, ylim=xlim,...)
            par(opar)
          }
          )
