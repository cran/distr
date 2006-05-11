.distroptions <- list(
                      DefaultNrGridPoints = 2^12,
                      DistrResolution = 1e-6,
                      TruncQuantile = 1e-5,
                      DefaultNrFFTGridPointsExponent = 12,
                      RtoDPQ.e = 5, # new Warning-items P.R. 28.03.06
                      WarningArith = TRUE,
                      WarningSim = TRUE)
  



.onAttach <- function(library, pkg)
{
  unlockBinding(".distroptions", asNamespace("distr"))
# next lines are taken from Valentin Todorov's package "rrcov"
#    ver <- read.dcf(file.path(library, pkg, "DESCRIPTION"), "Version")
#    ver <- as.character(ver)
#    title <- read.dcf(file.path(library, pkg, "DESCRIPTION"), "Title")
#    title <- as.character(title)
#    if((!getOption("StartupBanner")=="off")||is.null(getOption("StartupBanner"))) 
#       message(paste(title, " (version ", ver, ")\n", sep = ""))
#    msga <- gettext("For more information see ?\"distrTEst\", NEWS(\"distrTEst\"), and \n")
#    msgb <- gettext("    http://www.uni-bayreuth.de/departments/math/org/mathe7/DISTR/distr.pdf .\n")
#    if((getOption("StartupBanner")=="complete")||is.null(getOption("StartupBanner"))) 
#       message(msga,msgb,sep=""); 
    msga <- gettext("Attention: Arithmetics on distribution objects are understood as\n")
    msgb <- gettext("operations on corresponding random variables (r.v.s); see distrARITH().\n")
    msgc <- gettext("Note that global options are controlled by distroptions()\n---c.f. ?\"distroptions\".\n")
buildStartupMessage(pkg="distr", msga,msgb,msgc, library=library, 
                    packageHelp=TRUE, MANUAL="http://www.uni-bayreuth.de/departments/math/org/mathe7/DISTR/distr.pdf")
#    msgd <- gettext("For more information see ?\"distr\", NEWS(\"distr\"), and \n")
#    msge <- gettext("    http://www.uni-bayreuth.de/departments/math/org/mathe7/DISTR/distr.pdf .\n")
#    if((getOption("StartupBanner")=="complete")||is.null(getOption("StartupBanner"))) 
#        message(msga,msgb,msgc,msgd,msge,sep=""); 
  invisible()
} 
