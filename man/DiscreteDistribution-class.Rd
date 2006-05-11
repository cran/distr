\name{DiscreteDistribution-class}
\docType{class}
\alias{DiscreteDistribution-class}
\alias{initialize,DiscreteDistribution-method}

\title{Class "DiscreteDistribution"}
\description{The \code{DiscreteDistribution}-class is the mother-class of the classes \code{Binom}, \code{Dirac},
\code{Geom}, \code{Hyper}, \code{Nbinom} and \code{Poisson}. Further discrete distributions can be defined either by 
declaration of own random number generator, density and cumulative distribution and quantile functions, or as result of a 
convolution of two discrete distributions or by application of a mathematical operator to a discrete distribution. An 
additional way is, to specify only the random number generator. The function \code{RtoDPQ.d} then approximates the three 
remaining slots \code{d}, \code{p} and \code{q} by random sampling.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("DiscreteDistribution", r, d, p, q)}.
The result of this call is a discrete distribution.
}
\section{Slots}{
  \describe{
    \item{\code{img}:}{Object of class \code{"Reals"}: the space of the image of this distribution which has dimension 1
    and the name "Real Space" }
    \item{\code{param}:}{Object of class \code{"Parameter"}: the parameter of this distribution, having only the
    slot name "Parameter of a discrete distribution" }
    \item{\code{r}:}{Object of class \code{"function"}: generates random numbers}
    \item{\code{d}:}{Object of class \code{"function"}: density/probability function}
    \item{\code{p}:}{Object of class \code{"function"}: cumulative distribution function}
    \item{\code{q}:}{Object of class \code{"function"}: quantile function}
    \item{\code{support}:}{Object of class \code{"numeric"}: a (sorted) vector containing the support of the discrete
    density function}
  }
}
\section{Extends}{
Class \code{"UnivariateDistribution"}, directly.\cr
Class \code{"Distribution"}, by class \code{"UnivariateDistribution"}.
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "DiscreteDistribution")}: initialize method }
    \item{Math}{\code{signature(x = "DiscreteDistribution")}: application of a mathematical function, e.g. \code{sin} or
    \code{exp} (does not work with \code{log}!), to this discrete distribution}
    \item{-}{\code{signature(e1 = "DiscreteDistribution")}: application of `-' to this discrete distribution}
    \item{*}{\code{signature(e1 = "DiscreteDistribution", e2 = "numeric")}: multiplication of this discrete distribution
    by an object of class `numeric'}
    \item{/}{\code{signature(e1 = "DiscreteDistribution", e2 = "numeric")}: division of this discrete distribution
    by an object of class `numeric'}
    \item{+}{\code{signature(e1 = "DiscreteDistribution", e2 = "numeric")}: addition of this discrete distribution
    to an object of class `numeric'}
    \item{-}{\code{signature(e1 = "DiscreteDistribution", e2 = "numeric")}: subtraction of an object of class `numeric'
    from this discrete distribution }
    \item{*}{\code{signature(e1 = "numeric", e2 = "DiscreteDistribution")}: multiplication of this discrete distribution
    by an object of class `numeric'}
    \item{+}{\code{signature(e1 = "numeric", e2 = "DiscreteDistribution")}: addition of this discrete distribution
    to an object of class `numeric'}
    \item{-}{\code{signature(e1 = "numeric", e2 = "DiscreteDistribution")}: subtraction of this discrete distribution
    from an object of class `numeric'}
    \item{+}{\code{signature(e1 = "DiscreteDistribution", e2 = "DiscreteDistribution")}: Convolution of two discrete
    distributions. The slots p, d and q are approximated by grids.}
    \item{-}{\code{signature(e1 = "DiscreteDistribution", e2 = "DiscreteDistribution")}: Convolution of two discrete
    distributions. The slots p, d and q are approximated by grids.}
    \item{support}{\code{signature(object = "DiscreteDistribution")}: returns the support}
    \item{plot}{\code{signature(object = "DiscreteDistribution")}: plots density, cumulative distribution and quantile
    function }
  }
}


\author{Thomas Stabla \email{statho3@web.de},\cr 
        Florian Camphausen \email{fcampi@gmx.de},\cr
        Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr 
        Matthias Kohl \email{Matthias.Kohl@stamats.de}}

\note{ Working with a computer, we use a finite interval as support which carries at least mass \code{1-getdistrOption("TruncQuantile")}. }

\seealso{
\code{\link{Parameter-class}}
\code{\link{UnivariateDistribution-class}}
\code{\link{Binom-class}}
\code{\link{Dirac-class}}
\code{\link{Geom-class}}
\code{\link{Hyper-class}}
\code{\link{Nbinom-class}}
\code{\link{Pois-class}}
\code{\link{AbscontDistribution-class}}
\code{\link{Reals-class}}
\code{\link{RtoDPQ.d}}

}
\examples{
B = Binom(prob=0.1,size=10) # B is a Binomial distribution with prob=0.1 and size=10.
P = Pois(lambda=1) # P is a Poisson distribution with lambda=1.
D1 = B+1 # a new discrete distributions with exact slots d, p, q
D2 = D1*3 # a new discrete distributions with exact slots d, p, q
D3 = B+P # a new discrete distributions with approximated slots d, p, q
D4 = D1+P # a new discrete distributions with approximated slots d, p, q
support(D4) # the (approximated) support of this distribution is 1, 2, ..., 21
r(D4)(1) # one random number generated from this distribution, e.g. 4
d(D4)(1) # The (approximated) density for x=1 is 0.1282716.
p(D4)(1) # The (approximated) probability that x<=1 is 0.1282716.
q(D4)(.5) # The (approximated) 50 percent quantile is 3.
}
\keyword{distribution}
