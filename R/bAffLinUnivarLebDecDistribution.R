############################## Accessor / Replacement functions

############################## Arithmetics

setMethod("*", c("AffLinUnivarLebDecDistribution","numeric"),
          function(e1, e2) {

          if (length(e2)>1) stop("length of operator must be 1")

          if (isTRUE(all.equal(e2,1))) return(e1)
          if (isTRUE(all.equal(e2,0)))
               return(new("Dirac", location = 0))

          Distr <- UnivarLebDecDistribution(
                     discretePart = discretePart(e1)*e2,
                     acPart = acPart(e1)*e2,
                     discreteWeight = discreteWeight(e1),
                     acWeight = acWeight(e1))

          object <- new("AffLinUnivarLebDecDistribution",
                    r = Distr@r, d = Distr@d, p = Distr@p,
                    q = Distr@q, X0 = e1, mixDistr = Distr@mixDistr,
                    mixCoeff = Distr@mixCoeff,
                    a = e1@a*e2, b = e1@b, .withSim  = e1@.withSim,
                    .withArith = TRUE)
          object})

setMethod("+", c("AffLinUnivarLebDecDistribution","numeric"),
          function(e1, e2) {
          if (length(e2)>1) stop("length of operator must be 1")
          if (isTRUE(all.equal(e2,0))) return(e1)

          Distr <- UnivarLebDecDistribution(
                     discretePart = discretePart(e1)+e2,
                     acPart = acPart(e1)+e2,
                     discreteWeight = discreteWeight(e1),
                     acWeight = acWeight(e1))

          object <- new("AffLinUnivarLebDecDistribution",
                    r = Distr@r, d = Distr@d, p = Distr@p,
                    q = Distr@q, X0 = e1, mixDistr = Distr@mixDistr,
                    mixCoeff = Distr@mixCoeff,
                    a = e1@a, b = e1@b+e2, .withSim  = e1@.withSim,
                    .withArith = TRUE)
          object})


