\name{AbscontParameter-class}
\docType{class}
\alias{AbscontParameter-class}
\alias{initialize,AbscontParameter-method}

\title{Class "AbscontParameter"}
\description{ The parameter of an absolutely continouos distribution, used by \code{AbscontDistribution}-class}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("AbscontParameter")}.
Usually an object of this class is not needed on its own, it is generated automatically when an object of the class
\code{AbscontDistribution} is instantiated.}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"}: a name / comment for the parameters, by default
    "Parameter of an absolutely continouos distribution"}
  }
}
\section{Extends}{
Class \code{"UnivariateParameter"}, directly.\cr
Class \code{"Parameter"}, by class \code{"UnivariateParameter"}.
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "AbscontParameter")}: initialize method }
  }
}

\author{Thomas Stabla \email{Thomas.Stabla@uni-bayreuth.de},\cr Florian Camphausen \email{Florian.Camphausen@uni-bayreuth.de},\cr
 Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr Matthias Kohl \email{Matthias.Kohl@uni-bayreuth.de}}


\seealso{
\code{\link{Parameter-class}}
\code{\link{UnivariateParameter-class}}
\code{\link{AbscontDistribution-class}}
}
\examples{
A = new("AbscontParameter")
name(A) = "Parameter of AbscontDistribution"
}
\keyword{}
