\name{Parameter-class}
\docType{class}
\alias{Parameter-class}
\title{Class "Parameter"}
\description{
  Parameter is the mother-class of AbscontParameter and
  DiscreteParameter.
}
\section{Objects from the Class}{
  A virtual Class: No objects may be created from it.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"}:
      a name / comment for the parameters}
  }
}
\section{Methods}{
  \describe{
    \item{name}{\code{signature(object = "Parameter")}:
      returns the name of the parameter}
    \item{name<-}{\code{signature(object = "Parameter")}:
      modifies the name of the parameter}
  }
}

\author{
  Thomas Stabla \email{Thomas.Stabla@uni-bayreuth.de},\cr
  Florian Camphausen \email{Florian.Camphausen@uni-bayreuth.de},\cr
  Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr
  Matthias Kohl \email{Matthias.Kohl@uni-bayreuth.de}
}
\seealso{
\code{\link{UnivariateParameter-class}}
\code{\link{Distribution-class}}
}
\keyword{}
