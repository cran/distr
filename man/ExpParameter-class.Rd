\name{ExpParameter-class}
\docType{class}
\alias{ExpParameter-class}
\alias{initialize,ExpParameter-method}

\title{Class "ExpParameter"}
\description{ The parameter of an exponential distribution, used by Exp-class}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("ExpParameter", rate)}.
Usually an object of this class is not needed on its own, it is generated automatically when an object of the class Exp
is instantiated. 
}
\section{Slots}{
  \describe{
    \item{\code{rate}:}{Object of class \code{"numeric"}: the rate of an exponential distribution }
    \item{\code{name}:}{Object of class \code{"character"}: a name / comment for the parameters }
  }
}
\section{Extends}{
Class \code{"Parameter"}, directly.
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "ExpParameter")}: initialize method }
    \item{rate}{\code{signature(object = "ExpParameter")}: returns the slot \code{rate} of the parameter of the distribution }
    \item{rate<-}{\code{signature(object = "ExpParameter")}: modifies the slot \code{rate} of the parameter of the distribution }
  }
}

\author{Thomas Stabla \email{Thomas.Stabla@uni-bayreuth.de},\cr Florian Camphausen \email{Florian.Camphausen@uni-bayreuth.de},\cr
 Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr Matthias Kohl \email{Matthias.Kohl@uni-bayreuth.de}}

\seealso{
\code{\link{Exp-class}}
\code{\link{AbscontParameter-class}}
}

\examples{
W=new("ExpParameter", rate=1)
rate(W) # rate of this distribution is 1.
rate(W)=2 # rate of this distribution is now 2.
}
\keyword{}
