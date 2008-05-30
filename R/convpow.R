##########################################################
## Function for n-fold convolution          
## -- absolute continuous distribution --
##########################################################

##implentation of Algorithm 3.4. of
# Kohl, M., Ruckdeschel, P., Stabla, T. (2005):
#   General purpose convolution algorithm for distributions
#   in S4-Classes by means of FFT.
# Technical report, Feb. 2005. Also available in
# http://www.uni-bayreuth.de/departments/math/org/mathe7/
#       /RUCKDESCHEL/pubs/comp.pdf


setMethod("convpow",
          signature(D1 = "AbscontDistribution"),
          function(D1, N){
            if( !.isNatural0(N))
              stop("N has to be a natural (or 0)")
            if (N==0) return(Dirac(0))
            if (N==1) return(D1)
    ##STEP 1

            lower <- getLow(D1);  upper <- getUp(D1);

    ##STEP 2

    ## binary logarithm of the effective number of gridpoints
            m <- max(getdistrOption("DefaultNrFFTGridPointsExponent") -
                 floor(log(N)/log(2)),5)
            M <- 2^m
            Nl <-2^ceiling(log(N)/log(2))

            h <- (upper-lower)/M
            dp1 <- .discretizeP(D1, lower, upper, h)

    ##STEP 3

            dpn0 <- c(dp1, numeric((Nl-1)*M))
    ##STEP 4

            ## computation of DFT
            fftdpn <- fft(dpn0)

    ##STEP 5

            ## convolution theorem for DFTs
            dpn <- c(0,(Re(fft(fftdpn^N, inverse = TRUE)) / (Nl*M))[1:(N*M-N+2)])

            x <- seq(from = N*lower+N/2*h, to = N*upper-N/2*h, by = h)
            x <- c(x[1]-h, x[1], x+h)

            ## density  (steps 5--7)

            dfun <- .makeDNew(x, dpn, h)

            ## cdf (steps 5--7)
            pfun <- .makePNew(x, dpn, h, .notwithLArg(D1))

            ## continuity correction by h/2

            ## quantile function
            yL <-  if  (q(D1)(0) == -Inf) -Inf  else  N*lower
            yR <-  if  (q(D1)(1) ==  Inf)  Inf  else  N*upper
            px.l <- pfun(x + 0.5*h)
            px.u <- pfun(x + 0.5*h, lower.tail = FALSE)
            qfun <- .makeQNew(x + 0.5*h, px.l, px.u, .notwithLArg(D1), yL, yR)

            rfun = function(N) colSums(matrix(r(D1)(n*N), ncol=N))

            object <- new("AbscontDistribution", r = rfun, d = dfun, p = pfun,
                       q = qfun, .withArith = TRUE, .withSim = FALSE)
            rm(m, dpn, dp1, dpn0, fftdpn)
            rm(h, px.u, px.l, rfun, dfun, qfun, pfun, upper, lower)
           return(object)
})

setMethod("convpow",
          signature(D1 = "LatticeDistribution"),
          function(D1, N){
            if( !.isNatural0(N))
              stop("N has to be a natural (or 0)")
            if (N==0) return(Dirac(0))

            if (N==1) return(D1)

            w <- width(lattice(D1))

            supp0 <- support(D1)
            supp1 <- seq(by=abs(w),from=N*min(supp0),to=N*max(supp0))

            d1 <- d(D1)(supp0); d1 <- c(d1,numeric((length(supp0)-1)*(N-1)))

            ## computation of DFT
            ftde1 <- fft(d1)

            ## convolution theorem for DFTs
            newd <- Re(fft(ftde1^N, inverse = TRUE)) / length(ftde1)
            newd <- (abs(newd) >= .Machine$double.eps)*newd

            rsum.u <- min( sum( rev(cumsum(rev(newd))) <=
                                getdistrOption("TruncQuantile")/2)+1,
                           length(supp1))
            rsum.l <- max( sum( cumsum(newd) <
                                getdistrOption("TruncQuantile")/2),
                          1)

            newd <- newd[rsum.l:rsum.u]
            newd <- newd/sum(newd)
            supp1 <- supp1[rsum.l:rsum.u]

            return(LatticeDistribution(supp=supp1,prob=newd))
})

###############################################################################
#
# new from 2.0: convpov for  AcDcLcDistribution
#
###############################################################################
#
setMethod("convpow",
          signature(D1 = "AcDcLcDistribution"),
          function(D1, N){
            if( !.isNatural0(N))
              stop("N has to be a natural (or 0)")
            if (N==0) return(Dirac(0))
            if (N==1) return(D1)
        e1 <- as(D1, "UnivarLebDecDistribution")

        aw1 <- acWeight(e1)
        dw1 <- 1-aw1
        dD1 <- discretePart(e1)
        aD1 <- acPart(e1)
        dD1 <- discretePart(e1)
        if(is(dD1,"LatticeDistribution"))
           dD1 <- as(dD1,"LatticeDistribution")
        dDm <- max(d.discrete(e1)(support(e1)))*dw1

        ep <- getdistrOption("TruncQuantile")

        if(aw1<ep) return(convpow(dD1,N))
        if(1-aw1<ep) return(convpow(aD1,N))

        maxN <- ceiling(2*log(ep)/log(dDm))
        Nm <- min(maxN,N)
        Mm <- N%/%Nm
        Rm <- N-Mm*Nm
        print(maxN)

        sumM <- function(mm){
                DList <- lapply(seq(mm+1)-1,
                               function(x) {
                                   S.a <- convpow(aD1, x)
                                   S.d <- convpow(dD1, mm-x) #as(dD1,
                                          #  "DiscreteDistribution"), mm-x)
                                   as(S.a+S.d,"UnivarLebDecDistribution")
                               })
                erg <- do.call(flat.LCD, c(DList,
                        alist(mixCoeff = dbinom(0:mm, size = mm, prob = aw1))))
                erg}
        erg <- sumM(Nm)
        if(Mm>1) erg <- convpow(erg,Mm)
        if(Rm>0) erg <- sumM(Rm)+ as(erg,"UnivarLebDecDistribution")
        if(is(erg,"UnivarLebDecDistribution")) erg <- simplifyD(erg)
        return(erg)
})
#
###############################################################################

setMethod("convpow",
          signature(D1 = "Norm"),
          function(D1, N) 
             {if( !.isNatural0(N))
                  stop("N has to be a natural (or 0)")
              if (N==0) return(Dirac(0))
              if(N==1)  D1 else Norm(mean = N*mean(D1), sd = sqrt(N)*sd(D1))}
           )

setMethod("convpow",
          signature(D1 = "Pois"),
          function(D1, N) 
             {if( !.isNatural0(N))
                  stop("N has to be a natural (or 0)")
              if (N==0) return(Dirac(0))
              if(N==1) D1 else  Pois(lambda=N*lambda(D1))
             }
          )

setMethod("convpow",
          signature(D1 = "Binom"),
          function(D1, N) 
             {if( !.isNatural0(N))
                  stop("N has to be a natural (or 0)")
              if (N==0) return(Dirac(0))
              if(N==1) D1 else  Binom(size=N*size(D1),prob=prob(D1))}
          )

setMethod("convpow",
          signature(D1 = "Nbinom"),
          function(D1, N) 
             {if( !.isNatural0(N))
                  stop("N has to be a natural (or 0)")
              if (N==0) return(Dirac(0))
              if(N==1) D1 else  Nbinom(size=N*size(D1),prob=prob(D1))}
          )

#setMethod("convpow",
#          signature(D1 = "Gammad"),
#          function(D1, N) 
#            {if((N<1)||(abs(floor(N)-N)>.Machine$double.eps))
#               stop("N has to be a natural greater than or equal to  1")
#              if(N==1) D1 else  Gammad(shape=N*shape(D1),scale=scale(D1))}
#          )

setMethod("convpow",
          signature(D1 = "Dirac"),
          function(D1, N) 
             {if( !.isNatural0(N))
                  stop("N has to be a natural (or 0)")
              if (N==0) return(Dirac(0))
              Dirac(shape=N*location(D1))}
          )

setMethod("convpow",
          signature(D1 = "ExpOrGammaOrChisq"),
          function(D1, N) 
             {if( !.isNatural0(N))
                  stop("N has to be a natural (or 0)")
              if (N==0) return(Dirac(0))
              if(N==1) return(D1) 
                 else  if(is(D1,"Gammad")) 
                          {D1 <- as(D1,"Gammad")
                           return(Gammad(shape = N*shape(D1),
                                   scale = scale(D1))) }
                 else convpow(as(D1, "AbscontDistribution"),N)}
          )

 setMethod("convpow",
          signature(D1 = "Cauchy"),
          function(D1, N) 
             {if( !.isNatural0(N))
                  stop("N has to be a natural (or 0)")
              if (N==0) return(Dirac(0))
              if(N==1)  D1 else Cauchy(location = N*location(D1), 
                                       scale = N*scale(D1))}
           )
