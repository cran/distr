
#------ UnivariateDistribution  ---------- #

setMethod("print", "UnivariateDistribution",
          function(x, ...){
            cat(gettextf("Distribution Object of Class: %s\n", class(x)[1]))
            if(x@.withArith && getdistrOption("WarningArith")) 
            {msga <- gettext(
     "arithmetics on distributions are understood as operations on r.v.'s\n")
             msgb <- gettext(
     "see 'distrARITH()'; for switching off this warning see '?distroptions'")
             warning(msga,msgb)}
            if(x@.withSim && getdistrOption("WarningSim")) 
            {msga <- gettext(
     "slots d,p,q have been filled using simulations; ")
             msgb <- gettext(
     "for switching off this warning see '?distroptions'")
             warning(msga,msgb)}
            parameter = param(x)
            Names = slotNames(parameter)
            if(length(Names) > 1){
              for(i in Names[Names != "name"])
                cat(i, ": ", slot(parameter, i), "\n")
            }
          })

setMethod("show", "UnivariateDistribution",
          function(object)print(object))

#------ DistributionList  ---------- #
## M.Kohl

setMethod("show", "DistrList", 
    function(object){
        cat(gettextf("An object of class \"%s\"\n", class(object)))
        for(i in 1:length(object)){
            cat("[[", i, "]]\n", sep = "")
            print(object[[i]])
        }
    })
