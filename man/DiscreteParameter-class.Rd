\name{DiscreteParameter-class}
\docType{class}
\alias{DiscreteParameter-class}
\alias{initialize,DiscreteParameter-method}

\title{Class "DiscreteParameter"}
\description{ The parameter of a discrete distribution, used by \code{DiscreteDistribution}-class}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("DiscreteParameter")}.
Usually an object of this class is not needed on its own, it is generated automatically when an object of the class
\code{DiscreteDistribution} is instantiated.}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"}: a name / comment for the parameters, by default
    "Parameter of a discrete distribution"}
  }
}
\section{Extends}{
Class \code{"UnivariateParameter"}, directly.\cr
Class \code{"Parameter"}, by class \code{"UnivariateParameter"}.
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "DiscreteParameter")}: initialize method }
  }
}

\author{Thomas Stabla \email{Thomas.Stabla@uni-bayreuth.de},\cr Florian Camphausen \email{Florian.Camphausen@uni-bayreuth.de},\cr
 Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr Matthias Kohl \email{Matthias.Kohl@uni-bayreuth.de}}


\seealso{
\code{\link{Parameter-class}}
\code{\link{UnivariateParameter-class}}
\code{\link{DiscreteDistribution-class}}
}
\examples{
D = new("DiscreteParameter")
name(D) = "Parameter of DiscreteDistribution"}
\keyword{}
