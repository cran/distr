
setMethod("Huberize", "AcDcLcDistribution",
          function(object, lower, upper, withSimplify = getdistrOption("simplifyD"))
                {Mi <- Minimum( object, Dirac(upper),
                                withSimplify = withSimplify)
                 Maximum(Dirac(lower),Mi,
                        withSimplify = withSimplify)})
