\name{GeomParameter-class}
\docType{class}
\alias{GeomParameter-class}
\alias{initialize,GeomParameter-method}

\title{Class "GeomParameter"}
\description{ The parameter of a geometric distribution, used by Geom-class}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("GeomParameter", prob)}.
Usually an object of this class is not needed on its own, it is generated automatically when an object of the class Geom
is instantiated. 
}
\section{Slots}{
  \describe{
    \item{\code{prob}:}{Object of class \code{"numeric"}: the probability of a geometric distribution }
    \item{\code{name}:}{Object of class \code{"character"}: a name / comment for the parameters }
  }
}
\section{Extends}{
Class \code{"Parameter"}, directly.
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "GeomParameter")}: initialize method }
    \item{prob}{\code{signature(object = "GeomParameter")}: returns the slot \code{prob} of the parameter of the
    distribution }
    \item{prob<-}{\code{signature(object = "GeomParameter")}: modifies the slot \code{prob} of the parameter of the
    distribution }
  }
}

\author{
  Thomas Stabla \email{statho3@web.de},\cr 
  Florian Camphausen \email{fcampi@gmx.de},\cr
  Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr 
  Matthias Kohl \email{Matthias.Kohl@stamats.de}
  }


\seealso{
\code{\link{Geom-class}}
\code{\link{Parameter-class}}
}

\examples{
W=new("GeomParameter",prob=0.5)
prob(W) # prob of this distribution is 0.5.
prob(W)=0.4 # prob of this distribution is now 0.4.
}
\keyword{}
