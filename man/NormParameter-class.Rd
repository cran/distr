\name{NormParameter-class}
\docType{class}
\alias{NormParameter-class}
\alias{initialize,NormParameter-method}
\title{Class "NormParameter"}
\description{ The parameter of a normal distribution, used by Norm-class}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("NormParameter", sd, mean)}.
  Usually an object of this class is not needed on its own. It is the
  mother-class of the class UniNormParameter, which is generated
  automatically when such a distribution is instantiated.  
}
\section{Slots}{
  \describe{
    \item{\code{sd}:}{Object of class \code{"numeric"}: the sd of a normal distribution }
    \item{\code{mean}:}{Object of class \code{"numeric"}: the mean of a normal distribution }
    \item{\code{name}:}{Object of class \code{"character"}: a name / comment for the parameters }
  }
}
\section{Extends}{
Class \code{"Parameter"}, directly.
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "NormParameter")}:
      initialize method}
    \item{mean}{\code{signature(object = "NormParameter")}:
      returns the slot mean of the parameter of the distribution} 
    \item{mean<-}{\code{signature(object = "NormParameter")}:
      modifies the slot mean of the parameter of the distribution} 
    \item{sd}{\code{signature(object = "NormParameter")}:
      returns the slot sd of the parameter of the distribution}
    \item{sd<-}{\code{signature(object = "NormParameter")}:
      modifies the slot sd of the parameter of the distribution}
  }
}

\author{
  Thomas Stabla \email{Thomas.Stabla@uni-bayreuth.de},\cr
  Florian Camphausen \email{Florian.Camphausen@uni-bayreuth.de},\cr
  Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr
  Matthias Kohl \email{Matthias.Kohl@uni-bayreuth.de}
}
\seealso{
\code{\link{Norm-class}}
\code{\link{Parameter-class}}
}
\examples{
W <- new("NormParameter", mean = 0, sd = 1)
sd(W) # sd of this distribution is 1.
sd(W) <- 2 # sd of this distribution is now 2.
}
\keyword{}
