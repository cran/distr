\name{TParameter-class}
\docType{class}
\alias{TParameter-class}
\alias{initialize,TParameter-method}

\title{Class "TParameter"}
\description{ The parameter of a t distribution, used by Td-class}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("TParameter", df)}.
Usually an object of this class is not needed on its own, it is generated automatically when an object of the class
\code{Td} is instantiated. 
}
\section{Slots}{
  \describe{
    \item{\code{df}:}{Object of class \code{"numeric"}: the df of a T distribution }
    \item{\code{name}:}{Object of class \code{"character"}: a name / comment for the parameters }
  }
}
\section{Extends}{
Class \code{"Parameter"}, directly.
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "TParameter")}: initialize method }
    \item{df}{\code{signature(object = "TParameter")}: returns the slot \code{df} of the parameter of the distribution }
    \item{df<-}{\code{signature(object = "TParameter")}: modifies the slot \code{df} of the parameter of the distribution }
  }
}

\author{Thomas Stabla \email{Thomas.Stabla@uni-bayreuth.de},\cr Florian Camphausen \email{Florian.Camphausen@uni-bayreuth.de},\cr
 Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr Matthias Kohl \email{Matthias.Kohl@uni-bayreuth.de}}

\seealso{
\code{\link{Td-class}}
\code{\link{Parameter-class}}
}

\examples{
W=new("TParameter",df=1)
df(W) # df of this distribution is 1.
df(W)=2 # df of this distribution is now 2.
}
\keyword{}
