.distroptions <- list(
                      DefaultNrGridPoints = 2^12,
                      DistrResolution = 1e-6,
                      TruncQuantile = 1e-5,
                      DefaultNrFFTGridPointsExponent = 12,
                      RtoDPQ.e = 5, 
                      # new Warning-items P.R. 28.03.06
                      WarningArith = TRUE,
                      WarningSim = TRUE
                      )
  



.onAttach <- function(library, pkg)
{
  unlockBinding(".distroptions", asNamespace("distr"))
    msga <- gettext(
    "Attention: Arithmetics on distribution objects are understood as\n"
                   )
    msgb <- gettext(
    "operations on corresponding random variables (r.v.s); see distrARITH().\n"
                   )
    msgc <- gettext(
    "Some functions from package 'stats' are intentionally masked\n---see distrMASK().\n"
                   )
    msgd <- gettext(
    "Note that global options are controlled by distroptions()\n---c.f. ?\"distroptions\"."
                   )
buildStartupMessage(pkg = "distr", msga, msgb, msgc, msgd, library = library, 
                    packageHelp = TRUE, 
# MANUAL = "http://www.uni-bayreuth.de/departments/math/org/mathe7/DISTR/distr.pdf",
                    VIGNETTE = gettext(
"Package \"distrDoc\" provides a vignette to this package as well as\nto several extension packages; try vignette(\"distr\")."
                                      )
                   )
  invisible()
} 
