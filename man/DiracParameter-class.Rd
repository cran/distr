\name{DiracParameter-class}
\docType{class}
\alias{DiracParameter-class}
\alias{initialize,DiracParameter-method}

\title{Class "DiracParameter"}
\description{ The parameter of a Dirac distribution, used by \code{Dirac}-class}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("DiracParameter", location)}.
Usually an object of this class is not needed on its own, it is generated automatically when an object of the class \code{Dirac}
is instantiated. 
}
\section{Slots}{
  \describe{
    \item{\code{location}:}{Object of class \code{"numeric"}: the location of a Dirac distribution }\cr
    \item{\code{name}:}{Object of class \code{"character"}: a name / comment for the parameters }
  }
}
\section{Extends}{
Class \code{"Parameter"}, directly.
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "DiracParameter")}: initialize method }
    \item{location}{\code{signature(object = "DiracParameter")}: returns the slot location of the parameter of the
    distribution }
    \item{location<-}{\code{signature(object = "DiracParameter")}: modifies the slot location of the parameter of the
    distribution }
  }
}

\author{Thomas Stabla \email{Thomas.Stabla@uni-bayreuth.de},\cr Florian Camphausen \email{Florian.Camphausen@uni-bayreuth.de},\cr
Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr Matthias Kohl \email{Matthias.Kohl@uni-bayreuth.de}}

\seealso{
\code{\link{Dirac-class}}
\code{\link{Parameter-class}}
}

\examples{
W=new("DiracParameter",location=1)
location(W) # location of this distribution is 1.
location(W)=2 # location of this distribution is now 2.
}
\keyword{}
