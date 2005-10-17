\name{FParameter-class}
\docType{class}
\alias{FParameter-class}
\alias{initialize,FParameter-method}
\title{Class "FParameter"}
\description{ The parameter of a F distribution, used by Fd-class}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("FParameter", df1, df2)}.
  Usually an object of this class is not needed on its own, it is
  generated automatically when an object of the class Fd is instantiated. 
}
\section{Slots}{
  \describe{
    \item{\code{df1}:}{Object of class \code{"numeric"}: the df1 of a F distribution}
    \item{\code{df2}:}{Object of class \code{"numeric"}: the df2 of a F distribution}
    \item{\code{name}:}{Object of class \code{"character"}:
      a name / comment for the parameters}
  }
}
\section{Extends}{
Class \code{"Parameter"}, directly.
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "FParameter")}: initialize method }
    \item{df1}{\code{signature(object = "FParameter")}:
      returns the slot df1 of the parameter of the distribution}
    \item{df1<-}{\code{signature(object = "FParameter")}:
      modifies the slot df1 of the parameter of the distribution}
    \item{df2}{\code{signature(object = "FParameter")}:
      returns the slot df2 of the parameter of the distribution}
    \item{df2<-}{\code{signature(object = "FParameter")}:
      modifies the slot df2 of the parameter of the distribution}
  }
}

\author{
  Thomas Stabla \email{Thomas.Stabla@uni-bayreuth.de},\cr
  Florian Camphausen \email{Florian.Camphausen@uni-bayreuth.de},\cr
  Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr
  Matthias Kohl \email{Matthias.Kohl@stamats.de}
}
\seealso{
\code{\link{Fd-class}}
\code{\link{Parameter-class}}
}
\examples{
W <- new("FParameter", df1 = 1, df2 = 1)
df2(W) # df2 of this distribution is 1.
df2(W) <- 2 # df2 of this distribution is now 2.
}
\keyword{}
