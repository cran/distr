\name{Distribution-class}
\docType{class}
\alias{Distribution-class}

\title{Class "Distribution"}
\description{ The \code{Distribution-class} is the mother-class of the class \code{UnivariateDistribution}.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("Distribution")}.
}

\section{Slots}{
  \describe{
    \item{\code{img}:}{Object of class \code{"Parameter"}: the space of the image }
    \item{\code{param}:}{Object of class \code{"function"}: the parameter }
    \item{\code{r}:}{Object of class \code{"function"}: generates random numbers}
    \item{\code{d}:}{Object of class \code{"OptionalFunction"}: density function}
    \item{\code{p}:}{Object of class \code{"OptionalFunction"}: cumulative distribution function}
    \item{\code{q}:}{Object of class \code{"OptionalFunction"}: quantile function}
  }
}
}
\section{Methods}{
  \describe{
    \item{img}{\code{signature(object = "Distribution")}: returns the space of the image}
    \item{param}{\code{signature(object = "Distribution")}: returns the parameter}
    \item{r}{\code{signature(object = "Distribution")}: returns the random number generator }
    \item{d}{\code{signature(object = "Distribution")}: returns the density function }
    \item{p}{\code{signature(object = "Distribution")}: returns the cumulative distribution function }
    \item{q}{\code{signature(object = "Distribution")}: returns the quantile function}
  }
}


\author{Thomas Stabla \email{Thomas.Stabla@uni-bayreuth.de},\cr Florian Camphausen \email{Florian.Camphausen@uni-bayreuth.de},\cr
 Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr Matthias Kohl \email{Matthias.Kohl@uni-bayreuth.de}}


\seealso{
\code{\link{UnivariateDistribution-class}}
\code{\link{Parameter-class}}

}
\keyword{distribution}
