\name{AbscontDistribution-class}
\docType{class}
\alias{AbscontDistribution-class}
\alias{initialize,AbscontDistribution-method}

\title{Class "AbscontDistribution"}
\description{The \code{AbscontDistribution}-class is the mother-class of the classes \code{Beta}, \code{Cauchy}, 
\code{Chisq}, \code{Exp}, \code{F}, \code{Gamma}, \code{Lnorm}, \code{Logis}, \code{Norm}, \code{T}, \code{Unif} and 
\code{Weibull}. Further absolutely continuous distributions can be defined either by declaration of
own random number generator, density, cumulative distribution and quantile functions, or as result of a 
convolution of two absolutely continuous distributions or by application of a mathematical operator to an absolutely
continuous distribution. An additional way is, to specify only the random number generator. The function \code{RtoDPQ} then 
approximates the three remaining slots d, p and q by random sampling.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("AbscontDistribution", r, d, p, q)}.
The result of this call is an absolutely continuous distribution.
}
\section{Slots}{
  \describe{
    \item{\code{img}:}{Object of class \code{"Reals"}: the space of the image of this distribution which has dimension 1
    and the name "Real Space" }
    \item{\code{param}:}{Object of class \code{"AbscontParameter"}: the parameter of this distribution, having only 
    the slot name "Parameter of an absolutely continuous distribution" }
    \item{\code{r}:}{Object of class \code{"function"}: generates random numbers}
    \item{\code{d}:}{Object of class \code{"function"}: density function}
    \item{\code{p}:}{Object of class \code{"function"}: cumulative distribution function}
    \item{\code{q}:}{Object of class \code{"function"}: quantile function}
  }
}
\section{Extends}{
Class \code{"UnivariateDistribution"}, directly. \cr
Class \code{"Distribution"}, by class \code{"UnivariateDistribution"}.
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "AbscontDistribution")}: initialize method }
    \item{Math}{\code{signature(x = "AbscontDistribution")}: application of a mathematical function, e.g. \code{sin} or
    \code{exp} (does not work with \code{log}!), to this absolutely continouos distribution}
    \item{-}{\code{signature(e1 = "AbscontDistribution")}: application of `-' to this absolutely continuous distribution}
    \item{*}{\code{signature(e1 = "AbscontDistribution", e2 = "numeric")}: multiplication of this absolutely continuous 
    distribution by an object of class `numeric'}
    \item{/}{\code{signature(e1 = "AbscontDistribution", e2 = "numeric")}: division of this absolutely continuous 
    distribution by an object of class `numeric'}
    \item{+}{\code{signature(e1 = "AbscontDistribution", e2 = "numeric")}: addition of this absolutely continuous 
    distribution to an object of class `numeric'}
    \item{-}{\code{signature(e1 = "AbscontDistribution", e2 = "numeric")}: subtraction of an object of class `numeric' from
    this absolutely continuous distribution }
    \item{*}{\code{signature(e1 = "numeric", e2 = "AbscontDistribution")}: multiplication of this absolutely continuous 
    distribution by an object of class `numeric'}
    \item{+}{\code{signature(e1 = "numeric", e2 = "AbscontDistribution")}: addition of this absolutely continuous 
    distribution to an object of class `numeric'}
    \item{-}{\code{signature(e1 = "numeric", e2 = "AbscontDistribution")}: subtraction of this absolutely continuous 
    distribution from an object of class `numeric'}
    \item{+}{\code{signature(e1 = "AbscontDistribution", e2 = "AbscontDistribution")}: Convolution of two absolutely
    continuous distributions. The slots p, d and q are approximated by grids.}
    \item{-}{\code{signature(e1 = "AbscontDistribution", e2 = "AbscontDistribution")}: Convolution of two absolutely
    continuous distributions. The slots p, d and q are approximated by grids.}
    \item{plot}{\code{signature(object = "AbscontDistribution")}: plots density, cumulative distribution and quantile 
    function }
  }
}


\author{Thomas Stabla \email{Thomas.Stabla@uni-bayreuth.de},\cr Florian Camphausen \email{Florian.Camphausen@uni-bayreuth.de},\cr
 Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr Matthias Kohl \email{Matthias.Kohl@uni-bayreuth.de}}

\seealso{
\code{\link{AbscontParameter-class}}
\code{\link{UnivariateDistribution-class}}
\code{\link{Beta-class}}
\code{\link{Cauchy-class}}
\code{\link{Chisq-class}}
\code{\link{Exp-class}}
\code{\link{F-class}}
\code{\link{Gamma-class}}
\code{\link{Lnorm-class}}
\code{\link{Logis-class}}
\code{\link{Norm-class}}
\code{\link{T-class}}
\code{\link{Unif-class}}
\code{\link{Weibull-class}}
\code{\link{DiscreteDistribution-class}}
\code{\link{Reals-class}}
\code{\link{RtoDPQ}}

}
\examples{
N = Norm() # N is a normal distribution with mean=0 and sd=1.
E = Exp() # E is an exponential distribution with rate=1.
A1 = E+1 # a new absolutely continuous distributions with exact slots d, p, q
A2 = A1*3 # a new absolutely continuous distributions with exact slots d, p, q
A3 = N*0.9 + E*0.1 # a new absolutely continuous distribution with approximated slots d, p, q
r(A3)(1) # one random number generated from this distribution, e.g. -0.7150937
d(A3)(0) # The (approximated) density for x=0 is 0.4379882.
p(A3)(0) # The (approximated) probability that x <= 0 is 0.4562021.
q(A3)(.1) # The (approximated) 10 percent quantile is 0.1.
}
\keyword{}
