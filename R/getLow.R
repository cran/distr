#------------------------------------------------------------------------------
### getLow / getUp methods
#------------------------------------------------------------------------------

 setMethod("getLow", "AbscontDistribution",
            function(object, eps = getdistrOption("TruncQuantile")) {
                 q0 <- q(object)(0)
                 if (q0 > - Inf){
                   return(q0)
                }else{
                 qe <- q(object)(eps)
                 if (qe > -Inf)
                    return(qe)
                 else return(q(object)(p = fm(x=.5, f=q(object)))) }
                 })
 setMethod("getUp", "AbscontDistribution",
            function(object, eps = getdistrOption("TruncQuantile")) {
                 q1 <- q(object)(1)
                 if (q1 < Inf){
                   return(q1)
                 }else{
                      if (.inArgs("lower.tail", q(object))){
                          qe <- q(object)(eps, lower.tail = FALSE)
                          if (qe < Inf)
                               return(qe)
                          else return(q(object)(p = .fM2(x=.5,f=q(object))))
                      }else{
                          qe <- q(object)(1-eps)
                          if (qe < Inf)
                               return(qe)
                          else return(q(object)(p = .fM(x=.5,f=q(object))))
                      }
                 }
            })
 setMethod("getLow", "DiscreteDistribution",
            function(object) min(support(object)) )
 setMethod("getUp", "DiscreteDistribution",
            function(object) max(support(object)) )

