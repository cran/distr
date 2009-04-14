
setMethod("NumbOfSummandsDistr", signature="CompoundDistribution",
           function(object) object@NumbOfSummandsDistr)
setMethod("SummandsDistr", signature="CompoundDistribution",
           function(object) object@SummandsDistr)

CompoundDistribution<- function( NumbOfSummandsDistr, SummandsDistr, .withSim = FALSE,
                                 withSimplify = FALSE){
  if(!is(NumbOfSummandsDistr,"DiscreteDistribution"))
    stop("Argument 'NumbOfSummandsDistr' must be of class 'DiscreteDistribution'")
  supp <- support(NumbOfSummandsDistr)
  if(!(all(.isInteger(supp))&&all(supp >=0)))
    stop("Support of 'NumbOfSummandsDistr' must be non neg. integers")

  if(!is(SummandsDistr,"UnivDistrListOrDistribution"))
    stop("Argument 'SummandsDistr' must be of class 'UnivDistrListOrDistribution'")
  supp <- support(NumbOfSummandsDistr)
  supp <- as(supp,"integer")
  suppNot0 <- supp[supp!=0L]
  is0 <- 0 %in% supp
  lI <- vector("list", length(supp))
  if(is0) lI[[1]] <- Dirac(0)
  if(length(suppNot0)){
     if(is(SummandsDistr,"UnivariateDistribution")){
        dsuppNot0 <- c(suppNot0,diff(suppNot0))
        S <- 0
        for (i in 1:length(suppNot0)){
             x0 <- convpow(SummandsDistr,dsuppNot0[i])
             S <- S + x0
             lI[[i+is0]] <- S
        }
     }else{
       supp <- min(supp):max(supp)
       if( (length(supp)!=length(SummandsDistr)) &&
           !(is0 && length(supp)==1+length(SummandsDistr)))
          stop("Lengths of support of 'NumbOfSummandDistr' and list in 'SummandDistr' do not match")
       if(is0 && length(supp)==length(SummandsDistr))
          SummandsDistr <- SummandsDistr[2:length(SummandsDistr)]
       S <- 0
       for(i in 1:(length(supp)-is0)){
           S <- S + SummandsDistr[[i]]
           lI[[i+is0]] <- S
       }
     }
  UV <- do.call("UnivarMixingDistribution",
                 args = c(list(mixCoeff = d(NumbOfSummandsDistr)(supp),
                               withSimplify = FALSE),
                               lI)
                 )
  obj <- new("CompoundDistribution",
              NumbOfSummandsDistr = NumbOfSummandsDistr,
              SummandsDistr = SummandsDistr,
              p = UV@p, r = UV@r, d = UV@d, q = UV@q,
              mixCoeff = UV@mixCoeff, mixDistr = UV@mixDistr,
              .withSim = .withSim, .withArith = TRUE)

   if(withSimplify) return(simplifyD(obj))
   else return(obj)

  }
}

setMethod("+", c("CompoundDistribution","numeric"),
          function(e1, e2) simplifyD(e1)+e2)
setMethod("*", c("CompoundDistribution","numeric"),
          function(e1, e2) simplifyD(e1)*e2)


setMethod("Math", "AcDcLcDistribution",
          function(x){
            callGeneric(simplifyD(.ULC.cast(x)))
          })

setAs("CompoundDistribution", "UnivarLebDecDistribution",
       function(from)simplifyD(from))







####################################
