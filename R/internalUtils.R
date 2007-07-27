#------------------------------------------------------------------------------
### internal help function to check if a vector can be made a lattice
#------------------------------------------------------------------------------

.is.vector.lattice <- function(x)
  {    ### is x equally spaced?
    all( sapply(diff(x), function(y)
         isTRUE(all.equal(y, diff(x)[1],
                tolerance = getdistrOption("DistrResolution")))))
  }

### internal help function to check consistency of lattice with support:
.is.consistent <- function(lattice, support, eq.space = TRUE)
  {
   p <- pivot(lattice); l <- Length(lattice); w <- width(lattice)
   ds <- diff(support); ms <- min(support); Ms <- max(support)
   ### is support equally spaced?
   if (! .is.vector.lattice(support)  && eq.space)
      return(FALSE)
   ### are width of lattice and support consistent
   if (! isTRUE(all.equal(min(ds),abs(w))))
      return(FALSE)
   ### pivot is left or right endpoint of support
   if ( isTRUE(all.equal(ms,p)) || isTRUE(all.equal(Ms,p)) )
      return(TRUE)

   if (isTRUE(all.equal(min((support[1]-p)%%w,w-(support[1]-p)%%w),0,
                         tolerance = getdistrOption("TruncQuantile"))))
      return(TRUE)
  return(FALSE)
  }

 ## generate a lattice from equally spaced vector
.make.lattice.es.vector <- function(x){
  new("Lattice", pivot = x[1], width = x[2]-x[1],
       Length = length(x))
}

#------------------------------------------------------------------------------
### .fm, .fM, .fM2 functions
#------------------------------------------------------------------------------

.fM <- function(x,f){
   xo <- x
   x1 <- (1+xo)/2
   i <- 1
   while( i < 30)
   { i <- i+1
     while(f(x1) < Inf)
        {xo <- x1
         x1 <- (1+x1)/2
         }
     x1 <- (x1+xo)/2}
   xo}
   
.fM2 <- function(x,f){
   xo <- x
   x1 <- xo/2
   i <- 1
   while( i < 30)
   { i <- i+1
     while(f(x1, lower.tail = FALSE) < Inf)
        {xo <- x1
         x1 <- x1/2
         }
     x1 <- (x1+xo)/2}
   xo}

.fm <- function(x,f){
   xo <- x
   x1 <- xo/2
   i <- 1
   while( i < 30)
   { i <- i+1
     while(f(x1) > -Inf)
        {xo <- x1
         x1 <- x1/2
         }
     x1 <- (x1+xo)/2}
   xo}
   
#------------------------------------------------------------------------------
# .presubs : for titles etc
#------------------------------------------------------------------------------

.presubs <- function(inp, frompat, topat){
### replaces in an expression or a string all frompat patterns to topat patterns

logic <- FALSE
inCx <- sapply(inp,
   function(inpx){
      inC <- deparse(inpx)
      l <- length(frompat)
      for(i in 1:l)
         { if (is.language(topat[[i]])){
               totxt <- deparse(topat[[i]])
               totxt <- gsub("expression\\(", "\", ", gsub("\\)$",", \"",totxt))
               if (length(grep(frompat[i],inC))) logic <<- TRUE
               inC <- gsub(frompat[i],totxt,inC)
           }else inC <- gsub(frompat[i], topat[[i]], inC)
         }
    })
if (length(inCx) > 1) {
   inCx <- paste(inCx, c(rep(",", length(inCx)-1), ""),
                 sep = "", collapse = "\"\\n\",")
   if ( any(c(lapply(inp,is.language))) | logic )
      inCx <- paste("expression(paste(", gsub("\\\\n"," ", inCx), "))", sep ="")
   else
      inCx <- paste("paste(",inCx,")", sep ="")
}else inCx <- paste("expression(paste(",inCx,"))",sep="")
outC <- eval(parse(text = eval(inCx)))
return(outC)
}

#------------------------------------------------------------------------------
# help check functions
#------------------------------------------------------------------------------

.inArgs <- function(arg, fct)
          {as.character(arg) %in% names(formals(fct))}

.isEqual <- function(p0, p1, tol = min( getdistrOption("TruncQuantile")/2,
                                          .Machine$double.eps^.7
                                          ))
                abs(p0-p1)< tol

.isIn <- function(p0, pmat, tol = min( getdistrOption("TruncQuantile")/2,
                                          .Machine$double.eps^.7
                                          ))
                  {list1 <- lapply(1:nrow(pmat), function(x){ 
                            (p0+tol > pmat[x,1]) & (p0-tol < pmat[x,2]) })
                   apply(matrix(unlist(list1), ncol = nrow(pmat)), 1, any)}           


.isEqual01<- function(x) .isEqual(x,0)|.isEqual(x,1)

.setEqual <- function(x, y, tol = 1e-7){ 
### all elements of x equal to some element of y up tol are set to exactly
###     the respective element of y
   x1 <- round(2*x/tol,0)
   y1 <- round(2*y/tol,0)
   z  <- x
   m  <- match(x1,y1)
   n.ina.m <- !is.na(m)
   z[n.ina.m] <- y[m[n.ina.m]]
   z
}


.notwithLArg <- function(D)  D@.withSim||!.inArgs("lower.tail",p(D))

#------------------------------------------------------------------------------
# other helpers
#------------------------------------------------------------------------------

.getObjName <- function(i = 1)
     {Ca <- sys.call(-4);  as.character(as.list(Ca[[1]]))[i+1]}

.discretizeP <- function(D, lower, upper, h){
   h0 <- 40*(getUp(D)-getLow(D)) /
         2^getdistrOption("DefaultNrFFTGridPointsExponent")
   if(h > h0 )
      warning(paste("Grid for approxfun too wide, ",
                     "increase DefaultNrFFTGridPointsExponent", sep =""))

   x <- seq(from = lower, to = upper, by = h)
   if(TRUE){#.notwithLArg(D)){
      return(diff(p(D)(x)))
   }else{
      M <- q(D)(0.5);   L <- length(x)
      x.l <- x [ x <= M ];  x.u <- x [ x >= M ]
      L.l <- length(x.l);   L.u <- length(x.u)
      if(L.u+L.l-L>0)
         return(c(diff(p(D)(x.l, lower.tail = TRUE)),
               -diff(p(D)(x.u, lower.tail = FALSE))))
      else
         return(c(diff(p(D)(x.l, lower.tail = TRUE)),
                p(D)(x.u[1])-p(D)(x.l[L.l]),
               -diff(p(D)(x.u, lower.tail = FALSE))))
            }
   }



#------------------------------------------------------------------------------
# .makeD, .makeP, .makeQ
#------------------------------------------------------------------------------

.makeD <- function(object, argList, stand = NULL)
         { d <- function(x, log = FALSE, ...){}
           if(inA <- .inArgs("log", object@d))
                     argList <- substitute(c(AL, log = log, ...),
                                           list(AL = argList))
           else      argList <- substitute(c(AL, ...),
                                           list(AL = argList))
           myCall <- substitute(d0 <- do.call("@"(objC,d),AL) ,
                                list(objC = quote(object),
                                AL = argList))
           myBod0 <- myCall
           if (!inA) myBod0 <- substitute({myCall
                                           if (log) d0 <- log(d0)})
           myBod <- myBod0
           if (!is.null(stand))
                myBod <- substitute(
                     {myBod0
                      d0 <- if (log) d0 - log(stand) else d0 / stand },
                      list(stand = stand, myBod0 = myBod0))
           body(d) <- substitute({myBod
                                  return(d0)})
           return(d)
         }

.makeP <- function(object, argList, sign = TRUE, correct = NULL)
         {
           p <- function(q, lower.tail = TRUE, log.p = FALSE, ...){}
           siY <- substitute(lower.tail)
           siN <- substitute(!lower.tail)
           lowS <-  if( sign) siY else siN
           lowS1 <- if(!sign) siY else siN
           if(inA1 <- .inArgs("lower.tail", object@p))
                      argList <- substitute(c(aL, lower.tail = lowT),
                                list( lowT = lowS,
                                      aL = argList )
                                      )
           else  argList <- substitute(aL, list(aL = argList ))
           if(inA2 <- (.inArgs("log.p", object@p) && inA1))
                     argList <- substitute(c(argList, log.p = log.p, ...))
           else      argList <- substitute(c(argList, ...))

           myCall <- substitute(p0 <- do.call("@"(objC,p),AL),
                                list(objC = quote(object),
                                     AL = argList))
           if (!inA1)
               myBod0 <- substitute({myC
                                     if (lowT) p0 <- 1 - p0},
                         list(myC=myCall, lowT = lowS1))
           else
               myBod0 <- substitute(myCall)

           if (!sign && !is.null(correct))
               myBod1 <- substitute({myC
                                     corC},
                         list(myC = myBod0, corC = correct))
           else myBod1 <- substitute(myBod0)

           if (!inA2) myBod <- substitute({myBod1
                                           if (log.p) p0 <- log(p0)
                                           return(p0)}  )

           else myBod <- substitute({myBod1
                                     return(p0)})
           body(p) <- substitute(myBod)
           return(p)
         }


.makeQ <- function(object, lastCall, sign = TRUE, Cont = TRUE)
     ## lastCall of form e1 %op% e2
         {
           siY <- substitute(lower.tail)
           siN <- substitute(!lower.tail)
           lowS <-  if( sign) siY else siN
           lowS1 <- if(!sign) siY else siN

           q <- function(p, lower.tail = TRUE, log.p = FALSE, ...){}
           argList <- alist(p)
           if(inA1 <- .inArgs("lower.tail", object@q))
                     argList1 <- substitute(alist(lower.tail = lowT),
                                list( lowT = lowS))
           else argList1 <- NULL

           if(inA2 <- (.inArgs("log.p", object@q) && inA1))
                      argList1 <- substitute(c(argList1,
                                               alist(log.p = log.p), ...))
           else       argList1 <- substitute(c(argList1, alist(...)))


           argList <- substitute(c(AL,AL1),list(AL=argList,AL1=argList1))
           myCall <- substitute({q0 <- do.call("@"(objC,q),AL)
                                 q0 <- lasC
                                 return(q0)},
                                list(objC = quote(object), AL=argList,
                                     lasC = lastCall))

           if (!Cont && !sign){
               if (inA1){
                     dqfS <- substitute({
                             dq <- getdistrOption("DistrResolution")
                             if (lower.tail) dq <- -dq
                                      })
               }else{
                     dqfS <- substitute({
                             dq <- getdistrOption("DistrResolution")
                                        })
               }
               if (inA2){ indS <- substitute(if(log.p){ ind <- (p>-Inf)&(p<0)
                                               p0[!ind] <- +1 }
                                             else { ind <- (p>0)&(p<1)
                                               p0[!ind] <- -1 } )
               }else    { indS <- substitute({ ind <- (p>0)&(p<1)
                                               p0[!ind] <- -1 })
               }
               myBod1 <- substitute({
                   q01 <- do.call("@"(object,q),AL)
                   p0 <- do.call("@"(object,p), c(alist(q=q01),AL1))
                   indC
                   ind2 <- .isEqual(p,p0)
                   dqfC # dq * if (with lower.tail-arg & lower.tail == FALSE)
                        # -1 else 1
                   p[ind2] <- p[ind2] + dq
                   my0C
               }, list(my0C = substitute(myCall),
                       AL = substitute(argList), AL1 = substitute(argList1),
                       indC = substitute(indS), dqfC = substitute(dqfS) )
                       )
                 ## discrete correction
           }else{
               myBod1 <- substitute(myCall)
           }

           if (!inA1) myBod2 <- substitute({if (lowT) p <- 1 - p
                                            myC},
                                    list(lowT = lowS1, myC = myBod1 ))
           else myBod2 <- substitute(myBod1)

           if (!inA2){
               myBod <- substitute({if (log.p) p <- exp(p)
                                    myC   },
                         list(myC = myBod2))
               }else{ myBod <- substitute(myBod2) }

           body(q) <- substitute(myBod)
           return(q)
         }

#------------------------------------------------------------------------------
# .plusm, .multm
#------------------------------------------------------------------------------

.plusm <- function(e1, e2, Dclass = "DiscreteDistribution"){
            if (length(e2)>1) stop("length of operator must be 1")
            if (isTRUE(all.equal(e2,0))) return(e1)

            if ((Dclass == "DiscreteDistribution")||
                (Dclass == "AffLinDiscreteDistribution"))
                supportnew <- e1@support + e2

            if ((Dclass == "AbscontDistribution")||
                (Dclass == "AffLinAbscontDistribution"))
                 gapsnew <- if(is.null(e1@gaps)) NULL else e1@gaps + e2

            rnew <- function(n, ...){}
            body(rnew) <- substitute({ f(n, ...) + g },
                                         list(f = e1@r, g = e2))
            dnew <- .makeD(e1, substitute(alist(x = x - e2), list(e2 = e2)))
            pnew <- .makeP(e1, substitute(alist(q = q - e2), list(e2 = e2)))
            qnew <- .makeQ(e1, substitute(q0 + e2, list(e2 = e2)))

            if (Dclass == "AffLinDiscreteDistribution"){
                object <- new("AffLinDiscreteDistribution", 
                              r = rnew, d = dnew, p = pnew,
                              q = qnew, support = supportnew,
                              a = e1@a, b = e1@b + e2, X0 = e1@X0,
                             .withSim = FALSE, .withArith = TRUE)
                rm(supportnew)

            }else if (Dclass == "DiscreteDistribution"){
                object <- new("AffLinDiscreteDistribution", 
                              r = rnew, d = dnew, p = pnew,
                              q = qnew, support = supportnew,
                              a = 1, b = e2, X0 = e1,
                             .withSim = FALSE, .withArith = TRUE)
                rm(supportnew)

            }else if (Dclass == "AffLinAbscontDistribution"){
                object <- new("AffLinAbscontDistribution", 
                              r = rnew, d = dnew, p = pnew, q = qnew, 
                              gaps = gapsnew, a = e1@a, b = e1@b + e2, 
                              X0 = e1@X0, .withSim = FALSE, .withArith = TRUE)

            }else if (Dclass == "AbscontDistribution"){  
                object <- new("AffLinAbscontDistribution", r = rnew, 
                              d = dnew, p = pnew, q = qnew, gaps = gapsnew, 
                              a = 1, b = e2, X0 = e1, .withSim = FALSE, 
                              .withArith = TRUE)
            }
            rm(pnew, qnew, dnew, rnew)
            object
          }

.multm <- function(e1, e2, Dclass = "DiscreteDistribution"){
            if (length(e2)>1) stop("length of operator must be 1")

            if (isTRUE(all.equal(e2,1))) return(e1)
            if (isTRUE(all.equal(e2,0)))
               return(new("Dirac", location = 0))

            rnew <- function(n, ...){}
            body(rnew) <- substitute({ f(n, ...) * g },
                                         list(f = e1@r, g = e2))

            if (Dclass == "AffLinDiscreteDistribution"){
                 supportnew <- e1@support * e2
                 if (e2 < 0) supportnew <- rev(supportnew)

                 coR <- substitute({
                             owarn <- getOption("warn"); options(warn = -1)
                             d0 <- object@d(x = q / e2C)
                             options(warn = owarn)
                             if (!lower.tail) d0 <- -d0
                             p0 <- p0 + d0},
                             list(e2C = e2)
                             )

                 dnew <- .makeD(substitute(e1, list(e1 = e1)),
                                substitute(alist(x = x / e2), list(e2 = e2)))
                 pnew <- .makeP(substitute(e1, list(e1 = e1)),
                                substitute(alist(q = q / e2), list(e2 = e2)),
                                sign = e2>0, correct = coR)
                 qnew <- .makeQ(substitute(e1, list(e1 = e1)),
                                substitute(q0 * e2, list(e2 = e2)),
                                sign = e2>0, Cont = FALSE)
                 object <- new(Dclass, r = rnew, d = dnew, p = pnew,
                               q = qnew, support = supportnew,
                               a = e1@a * e2, b = e2 * e1@b, X0 = e1@X0,                              
                              .withSim = FALSE, .withArith = TRUE)
                 rm(supportnew)

            }else if (Dclass == "DiscreteDistribution"){
                 supportnew <- e1@support * e2
                 if (e2 < 0) supportnew <- rev(supportnew)

                 coR <- substitute({
                             owarn <- getOption("warn"); options(warn = -1)
                             d0 <- object@d(x = q / e2C)
                             options(warn = owarn)
                             if (!lower.tail) d0 <- -d0
                             p0 <- p0 + d0},
                             list(e2C = e2)
                             )

                 dnew <- .makeD(substitute(e1, list(e1 = e1)),
                                substitute(alist(x = x / e2), list(e2 = e2)))
                 pnew <- .makeP(substitute(e1, list(e1 = e1)),
                                substitute(alist(q = q / e2), list(e2 = e2)),
                                sign = e2>0, correct = coR)
                 qnew <- .makeQ(substitute(e1, list(e1 = e1)),
                                substitute(q0 * e2, list(e2 = e2)),
                                sign = e2>0, Cont = FALSE)
                 object <- new("AffLinDiscreteDistribution", r = rnew, d = dnew, 
                               p = pnew, q = qnew, support = supportnew,
                               a = e2, b = 0, X0 = e1,                              
                              .withSim = FALSE, .withArith = TRUE)
                 rm(supportnew)

            }else if (Dclass == "AffLinAbscontDistribution"){
                 if(is.null(e1@gaps)) 
                    gapsnew <- NULL
                 else {gapsnew <- e1@gaps * e2
                       if (e2 < 0) gapsnew <- gapsnew[,c(2,1)] }
                 
                 dnew <- .makeD(substitute(e1, list(e1 = e1)),
                                substitute(alist(x = x / e2), list(e2 = e2)),
                                stand = abs(e2))
                 pnew <- .makeP(substitute(e1, list(e1 = e1)),
                                substitute(alist(q = q / e2), list(e2 = e2)),
                                sign = e2>0)
                 qnew <- .makeQ(substitute(e1, list(e1 = e1)),
                                substitute(q0 * e2, list(e2 = e2)),
                                sign = e2>0)
                 object <- new(Dclass, r = rnew, d = dnew, p = pnew, q = qnew, 
                               gaps = gapsnew, a = e1@a * e2, b = e2 * e1@b, 
                               X0 = e1@X0, .withSim = FALSE, .withArith = TRUE)
 
            }else if (Dclass == "AbscontDistribution"){
                 if(is.null(e1@gaps)) 
                    gapsnew <- NULL
                 else {gapsnew <- e1@gaps * e2
                       if (e2 < 0) gapsnew <- gapsnew[,c(2,1)] }

                 dnew <- .makeD(substitute(e1, list(e1 = e1)),
                                substitute(alist(x = x / e2), list(e2 = e2)),
                                stand = abs(e2))
                 pnew <- .makeP(substitute(e1, list(e1 = e1)),
                                substitute(alist(q = q / e2), list(e2 = e2)),
                                sign = e2>0)
                 qnew <- .makeQ(substitute(e1, list(e1 = e1)),
                                substitute(q0 * e2, list(e2 = e2)),
                                sign = e2>0)
                 object <- new("AffLinAbscontDistribution", r = rnew, d = dnew, 
                               p = pnew, q = qnew, gaps = gapsnew, a = e2, 
                               b = 0, X0 = e1, .withSim = FALSE, 
                               .withArith = TRUE)
            }
            rm(pnew, qnew, dnew, rnew)
            object
          }
          

#------------------------------------------------------------------------------
# .makeDd, .makePd, .makeQd
#------------------------------------------------------------------------------



.makeDd <- function(x,y, yleft, yright){
   intervall <- getdistrOption("DistrResolution") / 2
   supp.grid <- c(matrix(rbind(x - intervall, x + intervall), nrow = 1))
   prob.grid <- c(matrix(rbind(0, y), nrow = 1), 0)
   df0 <- stepfun(x = supp.grid, y = prob.grid)
   rm(intervall, supp.grid, prob.grid)
   return(df0)
}

.makePd <- function(x,y, yleft, yright){
   stepfun(x = x, y = c(yleft, y))
}

.makeQd <- function(x,y, yleft, yright){
force(y)
force(x)
f <- function(u) {
               q0 <- sapply(u, 
                       function(z) y[min(sum(x < z-.Machine$double.eps) + 1,
                                         length(y)) ] )
               q0[.isEqual(u,0)] <- yleft
               q0[.isEqual(u,1)] <- yright
               return(q0)}
### individualize local variables
#environment(f) <- new.env()
#assign("y", y, environment(f))
#assign("x", x, environment(f))
#assign("yright", yright, environment(f))
#assign("yleft", yleft, environment(f))
return(f)
}

.makeQc <- function(x,y, yleft, yright){
yl <- if(is.finite(yleft)) yleft  else y[1]
yr <- if(is.finite(yright)) yright else y[length(y)]
#f0 <- function(u) {
#               q0 <- sapply(u, 
#                       function(z) y[min(sum(x < z-.Machine$double.eps) + 1,
#                                         length(y)) ] )
#               return(q0)}
#eps <- getdistrOption("TruncQuantile")
#x00 <- seq(eps, 1-eps, length = getdistrOption("DefaultNrGridPoints"))
#y00 <- f0(x00)
#idx <- cumsum(rle(y00)[[1]]) ### use only unique y's and corresponding 
#x0 <- x00[idx]               ### maximal x's
#y0 <- y00[idx]
#f1 <- approxfun(x = x0, y = y0, yleft = y0[1], yright = y0[length(y0)])
f1 <- approxfun(x = x, y = y, yleft = yl, yright = yr)
f <- function(x) 
   {y1 <- f1(x)
    y1[.isEqual(x,0)] <- yleft
    y1[.isEqual(x,1)] <- yright
    return(y1)
    }
return(f)
}


#------------------------------------------------------------------------------
# .makeDNew, .makePNew, .makeQNew
#------------------------------------------------------------------------------


.makeDNew <- function(x, dx, h = NULL, Cont = TRUE, standM = "sum"){
            dx <- (dx >= .Machine$double.eps)*dx
            if( length(dx) < length(x) ) dx <- c(0,dx)

            if (is.null(h)) h <- 1

            dx1 <- dx / h
            mfun <- if (Cont) approxfun else .makeDd

            ## density
            df1 <- mfun(x = x, y = dx1, yleft = 0, yright = 0)

            if (standM == "sum")
                   stand <- sum(dx)
            else   {
            stand <- try(integrate(df1, -Inf, Inf)$value, TRUE)
            if (is(stand,"try-error")){
               warning("'integrate()' threw an error ---result may be inaccurate.")
               stand <- sum(df1(x))*h*(x[2]-x[1])
               }
            }
            dfun <- function(x, log = FALSE)
                    {if (log)
                          d0 <-    log(df1(x))-log(stand)
                     else d0 <- df1(x) / stand
                     return (d0)}
            rm(x,dx1,h)
            return(dfun)
}



.makePNew <- function(x, dx, h = NULL, notwithLLarg = FALSE,
                      Cont = TRUE, myPf = NULL, pxl = NULL, pxu = NULL){

  p.l <- if(!is.null(pxl)) pxl else cumsum(dx)
  if (is.null (h)) h <- 0

  if (Cont){
         mfun <- if (is.null (myPf)) approxfun else myPf
  }else  mfun <- .makePd

  ## continuity correction by h/2
  nm <- max(p.l)
  p1.l <- mfun(x = x + 0.5*h, y = p.l, yleft = 0, yright = nm)
  nm <- p1.l(max(x))

  if(notwithLLarg){
      ifElsePS <- substitute(if (lower.tail) p1.l(q) else 1 - p1.l(q))
  }else{
      p.u <- if(!is.null(pxu)) pxu else rev(cumsum(rev(dx)))
      ## continuity correction by h/2
      if (!Cont) p.u <- c(p.u[-1],0)
      p1.u <- mfun(x = x + 0.5*h, y = p.u, yright = 0, yleft = nm)
      rm(p.u)
      ifElsePS <- substitute(if (lower.tail) p1.l(q) else p1.u(q))
  }
  pfun <- function(q, lower.tail = TRUE, log.p = FALSE){}
  body(pfun) <- substitute(
              { p0 <- ifElsePC
                p0 <- if (log.p) log(p0)-log(nm) else p0/nm
                return(p0)
              }, list(ifElsePC = ifElsePS))
  rm(dx, p.l, notwithLLarg)
  return(pfun)
}


.makeQNew <- function(x, px.l, px.u, notwithLLarg = FALSE, yL , yR,
                      Cont = TRUE){
  owarn <- getOption("warn"); options(warn = -1)
  mfun <- if (Cont) .makeQc else
          .makeQd
  ix <- .isEqual01(px.l)
  xx <- px.l[!ix]; yy <- x[!ix]
  q.l <- mfun(x = xx, y = yy, yleft = yL, yright = yR)
  rm(xx,yy)
  if(notwithLLarg){
     ifElseQS <- quote(if (lower.tail) q.l(p01) else q.l(1-p01))
  }else{
#         px.u <- rev(px.u);
         x <- rev(x)
         if (Cont) px.u <- rev(px.u)
         ix <- .isEqual01(px.u)
         xx <- px.u[!ix]
         yy <- if (Cont) x[!ix] else x[rev(!ix)]
         q.u <- mfun(x = xx, y = yy, yleft = yR, yright = yL)
         rm(xx,yy)
     ifElseQS <- quote(if (lower.tail) q.l(p01) else q.u(p01))
  }
  options(warn = owarn)
  qfun <- function(p, lower.tail = TRUE, log.p = FALSE){}
  body(qfun) <- substitute({
          if (log.p) p <- exp(p)
          if (any((p < -.Machine$double.eps)|(p > 1+.Machine$double.eps)))
              warning(gettextf("q method of %s produced NaN's ", objN))
              i01 <- (-.Machine$double.eps<=p)&(p<=1+.Machine$double.eps)
              p01 <- p[i01] ## only values in [0,1] are used
              q0  <- p*0
              q0[!i01] <- NaN
              q0[ i01] <- ifElseQC
              return(as.numeric(q0))
              }, list(ifElseQC = ifElseQS, objN = quote(.getObjName(1))))
  return(qfun)
}




#setMethod("+", c("LatticeDistribution","AbscontDistribution"),plusDAC)
