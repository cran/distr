\name{Beta-class}
\docType{class}
\alias{Beta-class}
\alias{Beta}
\alias{initialize,Beta-method}
\title{Class "Beta"}
\description{
  The Beta distribution with parameters \code{shape1} \eqn{= a} and
  \code{shape2} \eqn{= b} has density
  \deqn{f(x)=\frac{\Gamma(a+b)}{\Gamma(a)\Gamma(b)}{x}^{a} {(1-x)}^{b}%
  }{Gamma(a+b)/(Gamma(a)Gamma(b))x^(a-1)(1-x)^(b-1)}
  for \eqn{a > 0}, \eqn{b > 0} and \eqn{0 \le x \le 1}{0 <= x <= 1}
  where the boundary values at \eqn{x=0} or \eqn{x=1} are defined as
  by continuity (as limits).

  C.f. \code{\link{rbeta}} 
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{Beta(shape1, shape2)}.
  This object is a beta distribution.
}
\section{Slots}{
  \describe{
    \item{\code{img}:}{Object of class \code{"Reals"}:
      The space of the image of this distribution has got dimension 1 and the name "Real Space". }
    \item{\code{param}:}{Object of class \code{"BetaParameter"}:
      the parameter of this distribution (shape1 and shape2), declared at its instantiation }
    \item{\code{r}:}{Object of class \code{"function"}:
      generates random numbers (calls function rbeta)}
    \item{\code{d}:}{Object of class \code{"function"}:
      density function (calls function dbeta)}
    \item{\code{p}:}{Object of class \code{"function"}:
      cumulative function (calls function pbeta)}
    \item{\code{q}:}{Object of class \code{"function"}:
      inverse of the cumulative function (calls function qbeta)}
  }
}
\section{Extends}{
  Class \code{"AbscontDistribution"}, directly. \cr
  Class \code{"UnivariateDistribution"}, by class \code{"AbscontDistribution"}. \cr
  Class \code{"Distribution"}, by class \code{"AbscontDistribution"}.
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "Beta")}:
      initialize method}
    \item{shape1}{\code{signature(object = "Beta")}:
      returns the slot shape1 of the parameter of the distribution}
    \item{shape1<-}{\code{signature(object = "Beta")}:
      modifies the slot shape1 of the parameter of the distribution}
    \item{shape2}{\code{signature(object = "Beta")}:
      returns the slot shape2 of the parameter of the distribution}
    \item{shape2<-}{\code{signature(object = "Beta")}:
      modifies the slot shape2 of the parameter of the distribution}
  }
}

\author{
  Thomas Stabla \email{Thomas.Stabla@uni-bayreuth.de},\cr
  Florian Camphausen \email{Florian.Camphausen@uni-bayreuth.de},\cr
  Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr
  Matthias Kohl \email{Matthias.Kohl@uni-bayreuth.de}
}
\seealso{
  \code{\link{BetaParameter-class}}
  \code{\link{AbscontDistribution-class}}
  \code{\link{Reals-class}}
  \code{\link{rbeta}}
}
\examples{
B <- Beta(shape1 = 1, shape2 = 1)
# B is a beta distribution with shape1 = 1 and shape2 = 1.
r(B)(1) # one random number generated from this distribution, e.g. 0.6979795
d(B)(1) # Density of this distribution is 1 for x=1.
p(B)(1) # Probability that x < 1 is 1.
q(B)(.1) # Probability that x < 0.1 is 0.1.
shape1(B) # shape1 of this distribution is 1.
shape1(B) <- 2 # shape1 of this distribution is now 2.
}
\keyword{}
