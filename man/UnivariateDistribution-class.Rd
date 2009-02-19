\name{UnivariateDistribution-class}
\docType{class}
\alias{UnivariateDistribution-class}
\alias{initialize,UnivariateDistribution-method}
\title{Class "UnivariateDistribution"}
\description{
  The UnivariateDistribution-class is the mother-class of the classes
  AbscontDistribution and DiscreteDistribution.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("UnivariateDistribution")}.
}
\section{Slots}{
  \describe{
    \item{\code{img}:}{Object of class \code{"Reals"}:
      the space of the image of this distribution which has dimension 1
      and the name "Real Space"} 
    \item{\code{param}:}{Object of class \code{"Parameter"}:
      the parameter of this distribution}
    \item{\code{r}:}{Object of class \code{"function"}:
      generates random numbers}
    \item{\code{d}:}{Object of class \code{"function"}:
      density function}
    \item{\code{p}:}{Object of class \code{"function"}:
      cumulative distribution function}
    \item{\code{q}:}{Object of class \code{"function"}:
      quantile function}
  }
}
\section{Extends}{
Class \code{"Distribution"}, directly.
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "UnivariateDistribution")}:

      initialize method }
    \item{dim}{\code{signature(x = "UnivariateDistribution")}:

      returns the dimension of the support of the distribution}
    \item{-}{\code{signature(e1 = "UnivariateDistribution")}:

      application of `-' to this univariate distribution}
    \item{*}{\code{signature(e1 = "UnivariateDistribution", e2 = "numeric")}:

      multiplication of this univariate distribution by an object of
      class `numeric'} 
    \item{/}{\code{signature(e1 = "UnivariateDistribution", e2 = "numeric")}:

      division of this univariate distribution by an object of class `numeric'}
    \item{+}{\code{signature(e1 = "UnivariateDistribution", e2 = "numeric")}:

      addition of this univariate distribution to an object of class
      `numeric'} 
    \item{-}{\code{signature(e1 = "UnivariateDistribution", e2 = "numeric")}:

      subtraction of an object of class `numeric' from this univariate
      distribution} 
    \item{*}{\code{signature(e1 = "numeric", e2 = "UnivariateDistribution")}:

      multiplication of this univariate distribution by an object of
      class `numeric'} 
    \item{+}{\code{signature(e1 = "numeric", e2 = "UnivariateDistribution")}:

      addition of this univariate distribution to an object of class
      `numeric'} 
    \item{-}{\code{signature(e1 = "numeric", e2 = "UnivariateDistribution")}:

      subtraction of this univariate distribution from an object of
      class `numeric'}
    \item{+}{\code{signature(e1 = "UnivariateDistribution", e2 = "UnivariateDistribution")}:

      Convolution of two univariate distributions. The slots p, d and q
      are approximated by grids.}
    \item{-}{\code{signature(e1 = "UnivariateDistribution", e2 = "UnivariateDistribution")}:
      
      Convolution of two univariate distributions. The slots p, d and q
      are approximated by grids.}
    \item{simplifyr}{\code{signature(object = "UnivariateDistribution")}:

      simplifies the r-method of a distribution, see there for further information}
    \item{print}{\code{signature(object = "UnivariateDistribution")}:

      returns the class of the object and its parameters}
    \item{show}{\code{signature(object = "UnivariateDistribution")}:
      as print}
      }
  }



\author{
  Thomas Stabla \email{statho3@web.de},\cr 
  Florian Camphausen \email{fcampi@gmx.de},\cr
  Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de},\cr
  Matthias Kohl \email{Matthias.Kohl@stamats.de}
}
\seealso{
\code{\link{Parameter-class}}
\code{\link{Distribution-class}}
\code{\link{AbscontDistribution-class}} \cr
\code{\link{DiscreteDistribution-class}} 
\code{\link{Reals-class}} 
\code{\link{RtoDPQ}}
\code{\link{simplifyr-methods}}
}
\keyword{distribution}
\concept{univariate distribution}
\concept{S4 distribution class}

