\name{Cauchy-class}
\docType{class}
\alias{Cauchy-class}
\alias{Cauchy}
\alias{initialize,Cauchy-method}

\title{Class "Cauchy"}
\description{  The Cauchy distribution with location \eqn{l}, by default \eqn{=0}, and scale \eqn{s} , by default \eqn{=1},has
  density
  \deqn{f(x) = \frac{1}{\pi s}
    \left( 1 + \left(\frac{x - l}{s}\right)^2 \right)^{-1}%
  }{f(x) = 1 / (pi s (1 + ((x-l)/s)^2))}
  for all \eqn{x}.
C.f. \code{\link{rcauchy}}
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{Cauchy(location, scale)}.
This object is a Cauchy distribution.
}
\section{Slots}{
  \describe{
    \item{\code{img}:}{Object of class \code{"Reals"}: The domain of this distribution has got dimension 1
    and the name "Real Space". }
    \item{\code{param}:}{Object of class \code{"CauchyParameter"}: the parameter of this distribution (location and scale),
     declared at its instantiation }
    \item{\code{r}:}{Object of class \code{"function"}: generates random numbers (calls function \code{rcauchy})}
    \item{\code{d}:}{Object of class \code{"function"}: density function (calls function \code{dcauchy})}
    \item{\code{p}:}{Object of class \code{"function"}: cumulative function (calls function \code{pcauchy})}
    \item{\code{q}:}{Object of class \code{"function"}: inverse of the cumulative function (calls function \code{qcauchy})}
  }
}
\section{Extends}{
Class \code{"AbscontDistribution"}, directly.\cr
Class \code{"UnivariateDistribution"}, by class \code{"AbscontDistribution"}.\cr
Class \code{"Distribution"}, by class \code{"AbscontDistribution"}.
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "Cauchy")}: initialize method }
    \item{location}{\code{signature(object = "Cauchy")}: returns the slot \code{location} of the parameter of the distribution }
    \item{location<-}{\code{signature(object = "Cauchy")}: modifies the slot \code{location} of the parameter of the distribution }
    \item{scale}{\code{signature(object = "Cauchy")}: returns the slot \code{scale} of the parameter of the distribution }
    \item{scale<-}{\code{signature(object = "Cauchy")}: modifies the slot \code{scale} of the parameter of the distribution }
  }
}

\author{Thomas Stabla \email{Thomas.Stabla@uni-bayreuth.de},\cr Florian Camphausen \email{Florian.Camphausen@uni-bayreuth.de},\cr
 Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr Matthias Kohl \email{Matthias.Kohl@uni-bayreuth.de}}


\seealso{
\code{\link{CauchyParameter-class}}
\code{\link{AbscontDistribution-class}}
\code{\link{Reals-class}}
\code{\link{rcauchy}}





}
\examples{
C=Cauchy(location=1,scale=1) # C is a Cauchy distribution with location=1 and scale=1.
r(C)(1) # one random number generated from this distribution, e.g. 4.104603
d(C)(1) # Density of this distribution is 0.3183099 for x=1.
p(C)(1) # Probability that x<1 is 0.5.
q(C)(.1) # Probability that x<-2.077684 is 0.1.
location(C) # location of this distribution is 1.
location(C)=2 # location of this distribution is now 2.
}
\keyword{}
