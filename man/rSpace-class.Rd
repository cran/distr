\name{rSpace-class}
\docType{class}
\alias{rSpace-class}

\title{Class "rSpace" }
\description{ The distribution-classes contain a slot where the sample space is stored. Typically, discrete random variables
take naturals as values. rSpace is the mother-class of the class EuclideanSpace. }
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"}: the name of the space }
  }
}
\section{Methods}{
  \describe{
    \item{name}{\code{signature(object = "rSpace")}: returns the name of the space }
    \item{name<-}{\code{signature(object = "rSpace")}: changes the name of the space }
  }
}

\author{Thomas Stabla \email{Thomas.Stabla@uni-bayreuth.de},\cr Florian Camphausen \email{Florian.Camphausen@uni-bayreuth.de},\cr
 Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr Matthias Kohl \email{Matthias.Kohl@uni-bayreuth.de}}


\seealso{
\code{\link{EuclideanSpace-class}}
\code{\link{Distribution-class}}
}
\keyword{}
