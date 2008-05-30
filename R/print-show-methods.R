#------  UnivariateDistribution ---------- #

setMethod("show", "UnivariateDistribution",
          function(object){cat(showobj(object))
          if(object@.withArith && getdistrOption("WarningArith")) 
            {msga <- gettext(
     "arithmetics on distributions are understood as operations on r.v.'s\n")
             msgb <- gettext(
     "see 'distrARITH()'; for switching off this warning see '?distroptions'")
             warning(msga,msgb)}
           if(object@.withSim && getdistrOption("WarningSim")) 
            {msga <- gettext(
     "slots d,p,q have been filled using simulations; ")
             msgb <- gettext(
     "for switching off this warning see '?distroptions'")
             warning(msga,msgb)}}
          )

setMethod("showobj", "UnivariateDistribution",
          function(object){
            x <- object
            txt <- gettextf("Distribution Object of Class: %s\n", class(x)[1])
            parameter = param(x)
            Names = slotNames(parameter)
            if(length(Names) > 1){
              for(i in Names[Names != "name"])
                txt <- c(txt,
                gettextf("%s: %s\n", i, slot(parameter, i)))                
            }
            #cat(txt)
            return(txt)
          })

setMethod("print", "UnivariateDistribution",
          function(x, ...)show(x))

#------ DistributionList  ---------- #

setMethod("show", "DistrList", 
    function(object){cat(showobj(object))
    
    if(any(unlist(lapply(object,function(x)x@.withArith))) && 
            getdistrOption("WarningArith")) 
            {msga <- gettext(
     "arithmetics on distributions are understood as operations on r.v.'s\n")
             msgb <- gettext(
     "see 'distrARITH()'; for switching off this warning see '?distroptions'")
             warning(msga,msgb)}
     if(any(unlist(lapply(object,function(x)x@.withSim))) && 
             getdistrOption("WarningSim")) 
            {msga <- gettext(
     "slots d,p,q have been filled using simulations; ")
             msgb <- gettext(
     "for switching off this warning see '?distroptions'")
             warning(msga,msgb)}
    
    }
    )

setMethod("showobj", "DistrList", 
    function(object){
        txt <- gettextf("An object of class \"%s\"\n", class(object))
        for(i in 1:length(object)){
            s <- showobj(object[[i]])
            les <- length(s)
            if(les >1)
                 st <- c(gettextf("[[%i]] ",i), rep("      :",les-1))
            else st <- gettextf("[[%i]] ",i)
            sts <- paste(st, gettextf("%s", s), sep = "")
            txt <- c(txt, sts)
        }
        return(txt)
    })

#------ UnivarMixingDistribution ---------- #


setMethod("show", "UnivarMixingDistribution",
          function(object){
          cat(showobj(object))
           if(object@.withArith && getdistrOption("WarningArith")) 
            {msga <- gettext(
     "arithmetics on distributions are understood as operations on r.v.'s\n")
             msgb <- gettext(
     "see 'distrARITH()'; for switching off this warning see '?distroptions'")
             warning(msga,msgb)}
           if(object@.withSim && getdistrOption("WarningSim")) 
            {msga <- gettext(
     "slots d,p,q have been filled using simulations; ")
             msgb <- gettext(
     "for switching off this warning see '?distroptions'")
             warning(msga,msgb)}
          }
         )


setMethod("showobj", "UnivarMixingDistribution",
          function(object){
              txt <- gettextf("An object of class \"%s\"\n", class(object))
              l <- length(mixCoeff(object))
              txt <- c(txt,
                     "---------------------------------------------\n")
              txt <- c(txt,
                  gettextf("It consists of  %i components \n", l))
              txt <- c(txt,
                     gettextf("Components: \n"))
 
              for(i in 1:l){
                 s <- showobj(mixDistr(object)[[i]])
                 les <- length(s)
                 st <- if (les>1) 
                     c(gettextf("[[%i]]", i), rep("      :",les-1))
                 else  gettextf("[[%i]]", i)
                 txt <- c(txt,
                          paste(st, gettextf("%s",s),sep=""))}
              txt <- c(txt,
                     "---------------------------------------------\n")
              txt <- c(txt, 
                          gettextf("Weights: \n"))
              txt <- c(txt, gettextf("%f",
                          round(mixCoeff(object),3)))
            txt<-c(txt,"\n ---------------------------------------------\n")
            return(txt)
            }
          )

#------ UnivarLebDecDistribution ---------- #

setMethod("show", "UnivarLebDecDistribution",
          function(object){
           objs <- as.character(deparse(           
           match.call(
                   call = sys.call(sys.parent(1)))$object))
           cat(showobj(object = object, objs = objs))
          if(object@.withArith && getdistrOption("WarningArith")) 
            {msga <- gettext(
     "arithmetics on distributions are understood as operations on r.v.'s\n")
             msgb <- gettext(
     "see 'distrARITH()'; for switching off this warning see '?distroptions'")
             warning(msga,msgb)}
           if(object@.withSim && getdistrOption("WarningSim")) 
            {msga <- gettext(
     "slots d,p,q have been filled using simulations; ")
             msgb <- gettext(
     "for switching off this warning see '?distroptions'")
             warning(msga,msgb)}          
          })

setMethod("showobj", "UnivarLebDecDistribution",
          function(object, objs){
              if(missing(objs)) objs <- "" 
              else if(length(grep("<S4 object ", objs))) objs <- ""
              txt <- gettextf("An object of class \"%s\"\n", class(object))
              txt <- c(txt,
                       gettextf("--- a Lebesgue decomposed distribution:\n\n"))
              l <- length(mixCoeff(object))
              txt <- c(txt,
                gettextf("   Its discrete part (with weight %f) is a\n", 
                  round(discreteWeight(object),3)))
              txt <- c(txt, showobj(discretePart(object)))
              txt <- c(txt,
              gettextf("This part is accessible with 'discretePart(%s)'.\n\n", 
                  objs))
              txt <- c(txt,
              gettextf("   Its absolutely continuous part (with weight %f) is a\n", 
                  round(acWeight(object),3)))
              txt <- c(txt, showobj(acPart(object)))
              txt <- c(txt,
              gettextf("This part is accessible with 'acPart(%s)'.\n", 
                  objs))
             return(txt)
          }          
          )

