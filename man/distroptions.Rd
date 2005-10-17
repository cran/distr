\name{distroptions}
\alias{distroptions}
\alias{DefaultNrGridPoints}
\alias{DistrResolution}
\alias{TruncQuantile}
\alias{DefaultNrFFTGridPointsExponent}
\alias{RtoDPQ.e}

\title{function to change the global variables of the package `distr' }
\description{With \code{distroptions} you may inspect and change the global variables used by the package \pkg{`distr'} }
\usage{
distroptions(arg = "missing", value = -1)
}
\arguments{
  \item{arg}{ the global variable to be shown or changed }
  \item{value}{ the new value of the global variable }
}
\value{
\code{distroptions()} returns a list of the global variables.
\code{distroptions(arg=x)} returns  the global variable \var{x}.
\code{distroptions(arg=x,value=y)} sets the value of the global variable \var{x} to \var{y}.

}
\author{Thomas Stabla \email{Thomas.Stabla@uni-bayreuth.de},\cr Florian Camphausen \email{Florian.Camphausen@uni-bayreuth.de},\cr
 Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr Matthias Kohl \email{Matthias.Kohl@stamats.de}}

\examples{
distroptions("RtoDPQ.e") # returns the value of RtoDPQ.e, by default = 5
distroptions("RtoDPQ.e",6) # now RtoDPQ.e = 6

## The function is currently defined as
function (arg = "missing", value = -1) 
{
    globals <- list(DefaultNrFFTGridPointsExponent = DefaultNrFFTGridPointsExponent, 
        DefaultNrGridPoints = DefaultNrGridPoints, DistrResolution = DistrResolution, 
        RtoDPQ.e = RtoDPQ.e, TruncQuantile = TruncQuantile)
    if (arg == "missing") {
        print(globals)
        return(invisible())
    }
    if (!any(arg == names(globals))) 
        stop(paste("No such variable:", arg))
    if (value == -1) 
        switch(arg, DefaultNrGridPoints = DefaultNrGridPoints, 
            DistrResolution = DistrResolution, TruncQuantile = TruncQuantile, 
            DefaultNrFFTGridPointsExponent = DefaultNrFFTGridPointsExponent, 
            RtoDPQ.e = RtoDPQ.e)
    else eval.parent(parse(text = paste("assignInNamespace(\"", 
        arg, "\",", value, ", \"distr\")", sep = "")))
  }
}
\keyword{}
