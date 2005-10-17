\name{Norm-class}
\docType{class}
\alias{Norm-class}
\alias{Norm}
\alias{initialize,Norm-method}

\title{Class "Norm"}
\description{  The normal distribution has density
  \deqn{
    f(x) =
    \frac{1}{\sqrt{2\pi}\sigma} e^{-(x-\mu)^2/2\sigma^2}}{
    f(x) = 1/(sqrt(2 pi) sigma) e^-((x - mu)^2/(2 sigma^2))
  }
  where \eqn{\mu}{mu} is the mean of the distribution and
  \eqn{\sigma}{sigma} the standard deviation.
 C.f. \code{\link[stats:Normal]{rnorm}}
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{Norm(mean, sd)}.
This object is a normal distribution.
}
\section{Slots}{
  \describe{
    \item{\code{img}:}{Object of class \code{"Reals"}: The domain of this distribution has got dimension 1
    and the name "Real Space". }
    \item{\code{param}:}{Object of class \code{"UniNormParameter"}: the parameter of this distribution (mean and sd),
     declared at its instantiation }
    \item{\code{r}:}{Object of class \code{"function"}: generates random numbers (calls function \code{rnorm})}
    \item{\code{d}:}{Object of class \code{"function"}: density function (calls function \code{dnorm})}
    \item{\code{p}:}{Object of class \code{"function"}: cumulative function (calls function \code{pnorm})}
    \item{\code{q}:}{Object of class \code{"function"}: inverse of the cumulative function (calls function \code{qnorm})}
  }
}
\section{Extends}{
Class \code{"AbscontDistribution"}, directly.\cr
Class \code{"UnivariateDistribution"}, by class \code{"AbscontDistribution"}.\cr
Class \code{"Distribution"}, by class \code{"AbscontDistribution"}.
}
\section{Methods}{
  \describe{
    \item{*}{\code{signature(e1 = "numeric", e2 = "Norm")}: multiplication of this normal distribution by an object of
    class `numeric'}
    \item{+}{\code{signature(e1 = "numeric", e2 = "Norm")}: addition of this normal distribution to an object of class
    `numeric'}
    \item{-}{\code{signature(e1 = "numeric", e2 = "Norm")}: subtraction of this normal distribution from an object of
    class `numeric'}
    \item{*}{\code{signature(e1 = "Norm", e2 = "numeric")}: multiplication of this normal distribution by an object of
    class `numeric'}
    \item{+}{\code{signature(e1 = "Norm", e2 = "numeric")}: addition of this normal distribution to an object of class
    `numeric'}
    \item{-}{\code{signature(e1 = "Norm", e2 = "numeric")}: subtraction of an object of class `numeric' from this normal
    distribution }
    \item{/}{\code{signature(e1 = "Norm", e2 = "numeric")}: division of this normal distribution by an object of class
    `numeric'}
    \item{-}{\code{signature(e1 = "Norm", e2 = "Norm")}}
    \item{+}{\code{signature(e1 = "Norm", e2 = "Norm")}: 
    For the normal distribution the exact convolution formulas are implemented thereby improving the general numerical
    approximation.}
    \item{initialize}{\code{signature(.Object = "Norm")}: initialize method }
    \item{mean}{\code{signature(object = "Norm")}: returns the slot \code{mean} of the parameter of the distribution }
    \item{mean<-}{\code{signature(object = "Norm")}: modifies the slot \code{mean} of the parameter of the distribution }
    \item{sd}{\code{signature(object = "Norm")}: returns the slot \code{sd} of the parameter of the distribution  }
    \item{sd<-}{\code{signature(object = "Norm")}: modifies the slot \code{sd} of the parameter of the distribution }
  }

}

\author{Thomas Stabla \email{Thomas.Stabla@uni-bayreuth.de},\cr Florian Camphausen \email{Florian.Camphausen@uni-bayreuth.de},\cr
 Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr Matthias Kohl \email{Matthias.Kohl@stamats.de}}

\seealso{
\code{\link{UniNormParameter-class}}
\code{\link{AbscontDistribution-class}}
\code{\link{Reals-class}}
\code{\link[stats:Normal]{rnorm}}

}
\examples{
N=Norm(mean=1,sd=1) # N is a normal distribution with mean=1 and sd=1.
r(N)(1) # one random number generated from this distribution, e.g. 2.257783
d(N)(1) # Density of this distribution is  0.3989423 for x=1.
p(N)(1) # Probability that x<1 is 0.5.
q(N)(.1) # Probability that x<-0.2815516 is 0.1.
mean(N) # mean of this distribution is 1.
sd(N)=2 # sd of this distribution is now 2.
M=Norm() # M is a normal distribution with mean=0 and sd=1.
O=M+N # O is a normal distribution with mean=1 (=1+0) and sd=sqrt(5) (=sqrt(2^2+1^2)).
}
\keyword{distribution}
