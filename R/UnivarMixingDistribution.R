UnivarMixingDistribution <- function(..., Dlist, mixCoeff,
                                     withSimplify = getdistrOption("simplifyD"))
   {
    ldots <- list(...)
    if(!missing(Dlist)){
        Dlist.L <- as(Dlist, "list")
        if(!is(try(do.call(UnivarDistrList,args=Dlist.L),silent=TRUE),"try-error"))
            ldots <- c(ldots, Dlist.L)
       }
    l <- length(ldots)
    mixDistr <- do.call(UnivarDistrList,args=ldots)
    if(missing(mixCoeff))
       mixCoeff <- rep(1,l)/l
    else{ if (l!=length(mixCoeff))
          stop("argument 'mixCoeff' and the mixing distributions must have the same length")
          if(any(mixCoeff < 0) || sum(mixCoeff)>1)
             stop("mixing coefficients are no probabilities")
        }
    mixDistr <- new("UnivarDistrList", ldots)
    rnew <- .rmixfun(mixDistr = mixDistr, mixCoeff = mixCoeff)

    pnew <- .pmixfun(mixDistr = mixDistr, mixCoeff = mixCoeff)


    .withArith <- any(as.logical(lapply(mixDistr, function(x) x@".withArith")))
    .withSim   <- any(as.logical(lapply(mixDistr, function(x) x@".withSim")))

    dnew <- NULL
    if (all( as.logical(lapply(mixDistr, function(x) is(x,"AbscontDistribution")))) ||
        all( as.logical(lapply(mixDistr, function(x) is(x,"DiscreteDistribution")))))
        dnew <- .dmixfun(mixDistr = mixDistr, mixCoeff = mixCoeff)


    qnew <- .qmixfun(mixDistr = mixDistr, mixCoeff = mixCoeff,
                     Cont = TRUE, pnew = pnew)

    obj <- new("UnivarMixingDistribution", p = pnew, r = rnew, d = dnew, q = qnew,
         mixCoeff = mixCoeff, mixDistr = mixDistr, .withSim = .withSim,
         .withArith = .withArith)

    if (withSimplify)
        obj <- simplifyD(obj)

    return(obj)
}


setMethod("mixCoeff", "UnivarMixingDistribution", function(object)object@mixCoeff)
setReplaceMethod("mixCoeff", "UnivarMixingDistribution", function(object,value){
   object@mixCoeff<- value; object})


setMethod("mixDistr", "UnivarMixingDistribution", function(object)object@mixDistr)
setReplaceMethod("mixDistr", "UnivarMixingDistribution", function(object,value){
   object@mixDistr<- value; object})

