################################################################################
#
#                  LatticeDistribution
#
################################################################################

LatticeDistribution <- function(lattice = NULL, supp = NULL, prob = NULL, 
                       .withArith = FALSE, .withSim = FALSE, 
                       DiscreteDistribution = NULL, check = TRUE){
    if (is(DiscreteDistribution, "AffLinDiscreteDistribution"))
        {  D <- DiscreteDistribution
           if (is(lattice, "Lattice")) 
             { ### check consistency with support of DiscreteDistribution} 
              if( !.is.consistent(lattice, support(D), eq.space = FALSE)){         
                 if (check)
                     stop(paste("Argument 'lattice' is inconsistent to",
                            " the support of argument 'DiscreteDistribution'." , 
                            sep = ""))
                 else return(D)}           
              return(new("AffLinLatticeDistribution", r = D@r, d = D@d, 
                          q = D@q, p = D@p, support = D@support, 
                          a = D@a, b = D@b, X0 = D@X0,
                          lattice = lattice, .withArith = .withArith, 
                          .withSim = .withSim, img = D@img,
                          param = D@param))
              }else{
               if( !.is.vector.lattice(support(D))){ 
                   if (check)
                       stop(paste("Support of argument 'DiscreteDistribution' ",
                              "is not a lattice.", sep = ""))
                   else return(D)}           
               return(new("AffLinLatticeDistribution", r = D@r, d = D@d, 
                          q = D@q, p = D@p, support = D@support, 
                          lattice = .make.lattice.es.vector(D@support), 
                          a = D@a, b = D@b, X0 = D@X0,
                          .withArith = .withArith, 
                          .withSim = .withSim, img = D@img,
                          param = D@param))                           
              }                 
        }

    if (is(DiscreteDistribution, "DiscreteDistribution"))
        {  D <- DiscreteDistribution
           if (is(lattice, "Lattice")) 
             { ### check consistency with support of DiscreteDistribution} 
              if( !.is.consistent(lattice, support(D), eq.space = FALSE)){         
                 if (check)
                     stop(paste("Argument 'lattice' is inconsistent to the",
                            " support of argument 'DiscreteDistribution'." , 
                            sep = ""))
                 else return(D)}           
              return(new("LatticeDistribution", r = D@r, d = D@d, 
                          q = D@q, p = D@p, support = D@support, 
                          lattice = lattice, .withArith = .withArith, 
                          .withSim = .withSim, img = D@img,
                          param = D@param))
              }else{
               if( !.is.vector.lattice(support(D))){ 
                 if (check)
                     stop(paste("Support of argument 'DiscreteDistribution' is",
                              "not a lattice.", sep = " "))
                 else return(D)}           
 
               return(new("LatticeDistribution", r = D@r, d = D@d, 
                          q = D@q, p = D@p, support = D@support, 
                          lattice = .make.lattice.es.vector(D@support), 
                          .withArith = .withArith, 
                          .withSim = .withSim, img = D@img,
                          param = D@param))                           
              }                 
        }

    if (is(lattice, "Lattice") && ! is.null(supp))
       {D <- DiscreteDistribution(supp = supp, prob = prob, 
                                   .withArith = .withArith, 
                                   .withSim = .withSim )
        
        if( !.is.consistent(lattice, supp, eq.space = FALSE)){         
            if (check)     
                stop("Argument 'lattice' is inconsistent to argument 'supp'.")
            else return(D)}
        
        return(new("LatticeDistribution", r = r(D), d = d(D), 
                    q = q(D), p = p(D), support = supp, 
                    lattice = lattice, .withArith = .withArith, 
                    .withSim = .withSim))
       }

    if (is(lattice, "Lattice"))
       {if (is.finite(Length(lattice)))
             {if (is.null(prob))
                  prob <- rep(1/Length(lattice), Length(lattice))
              if (Length(lattice) == length(prob))
                 {supp <- seq( pivot(lattice), length = Length(lattice), 
                               by = width(lattice))
                  D <- DiscreteDistribution(supp = supp, prob = prob, 
                                             .withArith = .withArith, 
                                             .withSim = .withSim )
                  return(new("LatticeDistribution", r = r(D), d = d(D), 
                          q = q(D), p = p(D), support = supp, 
                          lattice = lattice, .withArith = .withArith, 
                          .withSim = .withSim))
                  }else{ 
                   if (check)
                       stop("Lengths of lattice and probabilities differ.")
                   else return(D)}    
              }else {if (is.null(prob))
                        stop(paste("Insufficient information given to ",
                                   "determine distribution.", sep = ""))
                     else{
                         supp <- seq( pivot(lattice), length = length(prob), 
                                     by = width(lattice))
                         D <- DiscreteDistribution(supp = supp, prob = prob, 
                                                   .withArith = .withArith, 
                                                   .withSim = .withSim )
                         return(new("LatticeDistribution", r = r(D), d = d(D), 
                                q = q(D), p = p(D), support = supp, 
                                lattice = lattice, .withArith = .withArith, 
                                .withSim = .withSim))
                        }                  
             }
       }else if (!is.null(supp))
            {if (is.null(prob)) prob <- supp*0+1/length(supp)
             D <- DiscreteDistribution(supp, prob, .withArith = .withArith, 
                                       .withSim = .withSim )
             if (!.is.vector.lattice (supp)){
                 if (check)
                     stop("Argument 'supp' given is not a lattice.")
                 else return (D)    
             }else{ 
                  return(new("LatticeDistribution", r = D@r, d = D@d, 
                             q = D@q, p = D@p, support = D@support, 
                             lattice = .make.lattice.es.vector(D@support), 
                             .withArith = D@.withArith, 
                             .withSim = D@.withSim, img = D@img,
                             param = D@param))                           
                 }
            }else 
             stop("Insufficient information given to determine distribution.")
}


setMethod("lattice", "LatticeDistribution", function(object) object@lattice)


## canceling out of lattice points with mass 0
#setAs("LatticeDistribution", "DiscreteDistribution", 
#       def = function(from){
#    cF <- class(from)[1]
#    value <- if (cF!="LatticeDistribution") 
#                 new(cF) else new("DiscreteDistribution")
#    for (what in slotNames("DiscreteDistribution")) 
#         slot(value, what) <- slot(from, what)
#    supp.old <- from@support
#    o.warn <- getOption("warn"); options(warn = -2)
#    d.old <- from@d(from@support)
#    options(warn = o.warn)
#    supp.new <- supp.old[d.old > 0]
#    value@support <- supp.new
#    value
#       }
#)


setAs("AffLinLatticeDistribution","AffLinDiscreteDistribution", 
       def = function(from){
    value <- new("AffLinDiscreteDistribution")
    for (what in slotNames("AffLinDiscreteDistribution")) 
         slot(value, what) <- slot(from, what)
    supp.old <- from@support
    o.warn <- getOption("warn"); options(warn = -2)
    d.old <- from@d(from@support)
    supp.new <- supp.old[d.old > 0]
    options(warn = o.warn)
    value@support <- supp.new
    value
       }
)

setMethod("+", c("LatticeDistribution", "LatticeDistribution"),
function(e1,e2){
            ### Step 1

#            e1 <- as(e1, "LatticeDistribution")
#            e2 <- as(e2, "LatticeDistribution")
#                  ### casting necessary due to setIs

            ### Lattice Calculations:
            w1 <- width(lattice(e1))
            w2 <- width(lattice(e2))

            if (abs(abs(w1)-abs(w2)) < getdistrOption("DistrResolution")){
               w <- w1
               ###  else need common lattice
            }else{
                   W <- sort(abs(c(w1,w2)))
                   if (W[2] %% W[1] > getdistrOption("DistrResolution"))
                       return(as(e1, "DiscreteDistribution") +
                              as(e2, "DiscreteDistribution"))
                   else
                       w <- W[1] #generate common lattice / support
                  }

            newlat <- NULL
            ### if any lattice is infinite: see if we can keep this in mind:
            if( ! (is.finite(Length(lattice(e1))) &&
                   is.finite(Length(lattice(e2)))   )  &&
                width(lattice(e1)) * width(lattice(e2)) > 0 )
                {p1 <- pivot(lattice(e1))
                 p2 <- pivot(lattice(e2))
                 p <- p1 + p2
                 w0 <- if (width(lattice(e1))>0) w else -w
                 newlat <- Lattice(pivot = p, width = w0, Length = Inf)
                }
            ### end Lattice Calculations

            ### Step 2
            supp0 <- seq(by = abs(w),
                         from = min(support(e1), support(e2)),
                         to   = max(support(e1), support(e2)))

            d1 <- d(e1)(supp0)
            d2 <- d(e2)(supp0)

            L <- length(supp0)
            Ln <- 2^(ceiling(log(L)/log(2))+1)

            ### Step 3
            d1 <- c(d1, numeric(Ln-L))
            d2 <- c(d2, numeric(Ln-L))

            ##STEP 4
            ## computation of DFT
            ftde1 <- fft(d1)
            ftde2 <- fft(d2)

            ## convolution theorem for DFTs
            newd <- (Re(fft(ftde1*ftde2, inverse = TRUE)) / Ln)[1:(2*L+1)]
            newd <- (newd >= .Machine$double.eps)*newd


            ## reduction to relevant support
            supp1 <- seq(by = abs(w),
                         from = 2 * min(support(e1), support(e2)),
                         to   = 2 * max(support(e1), support(e2)))
            L1 <- length(supp1)
            newd <- newd[1:L1]

            if (L1 > getdistrOption("DefaultNrGridPoints")){
                rsum.u <- min( sum( rev(cumsum(rev(newd))) >=
                                    getdistrOption("TruncQuantile")/2)+1,
                               length(supp1)
                           )
                rsum.l <- 1 + sum( cumsum(newd) < 
                                   getdistrOption("TruncQuantile")/2)
                newd <- newd[rsum.l:rsum.u]
                newd <- newd/sum(newd)
                supp1 <- supp1[rsum.l:rsum.u]
            }else{
                rsum.u <- min( sum( rev(cumsum(rev(newd))) >=
                                    .Machine$double.eps),
                               length(supp1)
                           )
                rsum.l <- 1 + sum( cumsum(newd) < .Machine$double.eps)
                newd <- newd[rsum.l:rsum.u]
                newd <- newd/sum(newd)
                supp1 <- supp1[rsum.l:rsum.u]}

            return(LatticeDistribution(supp = supp1, prob = newd,
                                       lattice = newlat, .withArith = TRUE))
          })

## extra methods
## binary operators

setMethod("+", c("LatticeDistribution", "numeric"),
          function(e1, e2) 
             {L <- lattice(e1)
              pivot(L) <- pivot(L) + e2
              Distr <- as(e1, "DiscreteDistribution") + e2 
              if(is(Distr, "AffLinDistribution"))
                    Distr@X0 <- e1
              LatticeDistribution(lattice = L, 
                     DiscreteDistribution = Distr)                                        
              })       

setMethod("*", c("LatticeDistribution", "numeric"),
          function(e1, e2) 
             {if (isTRUE(all.equal(e2,0)))
                  return(Dirac( location = 0 ))
              else     
                { L <- lattice(e1)
                  pivot(L) <- pivot(L) * e2
                  width(L) <- width(L) * e2
                  Distr <- as(e1, "DiscreteDistribution") * e2 
                  if(is(Distr, "AffLinDistribution"))
                        Distr@X0 <- e1
                  return(LatticeDistribution(lattice = L, 
                          DiscreteDistribution = Distr))
                }
             }
          )              

setMethod("+", c("AffLinLatticeDistribution", "numeric"),
          function(e1, e2) 
             {L <- lattice(e1)
              pivot(L) <- pivot(L) + e2
              LatticeDistribution(lattice = L, 
                     DiscreteDistribution = 
                        as(e1, "AffLinDiscreteDistribution") + e2)                     
              })       

setMethod("*", c("AffLinLatticeDistribution", "numeric"),
          function(e1, e2) 
             {if (isTRUE(all.equal(e2,0)))
                  return(Dirac( location = 0 ))
              else     
                { L <- lattice(e1)
                  pivot(L) <- pivot(L) * e2
                  width(L) <- width(L) * e2
                  return(LatticeDistribution(lattice = L, 
                          DiscreteDistribution = 
                             as(e1, "AffLinDiscreteDistribution") * 
                             e2))
                }
             }
          )              

