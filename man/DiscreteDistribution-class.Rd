\name{DiscreteDistribution-class}
\docType{class}
\alias{DiscreteDistribution}
\alias{DiscreteDistribution-class}
\alias{AffLinDiscreteDistribution-class}
\alias{initialize,DiscreteDistribution-method}
\alias{initialize,AffLinDiscreteDistribution-method}

\title{Class "DiscreteDistribution"}
\description{The \code{DiscreteDistribution}-class is the mother-class of the class \code{LatticeDistribution}.}
\section{Objects from the Class}{
Objects can be created by calls of the generating function \code{"DiscreteDistribution"}.
This generating function, from version 1.9 on, has been moved to this package from package \pkg{distrEx}.
}

\synopsis{DiscreteDistribution(supp, prob, .withArith = FALSE, .withSim = FALSE)}
\usage{
  DiscreteDistribution(supp, prob, .withArith=FALSE, .withSim=FALSE)
  DiscreteDistribution(supp)
}
\arguments{
  \item{supp}{numeric vector which forms the support 
    of the discrete distribution. }
  \item{prob}{vector of probability weights for the 
    elements of \code{supp}.}
  \item{.withArith}{normally not set by the user, but if determining the entries \code{supp}, \code{prob}
                    distributional arithmetics was involved, you may set this to \code{TRUE}.}
  \item{.withSim}{normally not set by the user, but if determining the entries \code{supp}, \code{prob}
                  simulations were involved, you may set this to \code{TRUE}.}
}
\details{
  If \code{prob} is missing, all elements in \code{supp}
  are equally weighted. 
}
\value{Object of class \code{"DiscreteDistribution"}}

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
    \item{\code{.withArith}:}{logical: used internally to issue warnings as to interpretation of arithmetics}
    \item{\code{.withSim}:}{logical: used internally to issue warnings as to accuracy}
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
    distributions. The slots p, d and q are approximated on a common grid.}
    \item{-}{\code{signature(e1 = "DiscreteDistribution", e2 = "DiscreteDistribution")}: Convolution of two discrete
    distributions. The slots p, d and q are approximated on a common grid.}
    \item{support}{\code{signature(object = "DiscreteDistribution")}: returns the support}
    \item{p.l}{\code{signature(object = "DiscreteDistribution")}: returns the 
               left continuous cumulative distribution function, i.e.; 
               \eqn{p.l(t) = P(object < t)}{p.l(t) = P(object < t)} }
    \item{q.r}{\code{signature(object = "DiscreteDistribution")}: returns the 
               right-continuous quantile function, i.e.; 
    \eqn{{\rm q.r}(s)=\sup\{t \,\big|\, P({\tt object}\ge t)\leq s\}}{q.r(s)=sup\{t|P(object>=t)<=s\}}}
    \item{plot}{\code{signature(object = "DiscreteDistribution")}: plots density, cumulative distribution and quantile
    function }
  }
}

\section{Internal subclass "AffLinDiscreteDistribution"}{
To enhance accuracy of several functionals on distributions,
  mainly from package \pkg{distrEx}, from version 1.9 of this package on, 
  there is an internally used (but exported) subclass 
  \code{"AffLinDiscreteDistribution"} which has extra slots 
  \code{a}, \code{b} (both of class \code{"numeric"}),  and \code{X0} 
  (of class \code{"DiscreteDistribution"}), to capture the fact 
  that the object has the same distribution as \code{a * X0 + b}. This is 
  the class of the return value of methods 
  \itemize{
    \item{-}{\code{signature(e1 = "DiscreteDistribution")}}
    \item{*}{\code{signature(e1 = "DiscreteDistribution", e2 = "numeric")}}
    \item{/}{\code{signature(e1 = "DiscreteDistribution", e2 = "numeric")}}
    \item{+}{\code{signature(e1 = "DiscreteDistribution", e2 = "numeric")}}
    \item{-}{\code{signature(e1 = "DiscreteDistribution", e2 = "numeric")}}
    \item{*}{\code{signature(e1 = "numeric", e2 = "DiscreteDistribution")}}
    \item{+}{\code{signature(e1 = "numeric", e2 = "DiscreteDistribution")}}
    \item{-}{\code{signature(e1 = "numeric", e2 = "DiscreteDistribution")}}
    \item{-}{\code{signature(e1 = "AffLinDiscreteDistribution")}}
    \item{*}{\code{signature(e1 = "AffLinDiscreteDistribution", e2 = "numeric")}}
    \item{/}{\code{signature(e1 = "AffLinDiscreteDistribution", e2 = "numeric")}}
    \item{+}{\code{signature(e1 = "AffLinDiscreteDistribution", e2 = "numeric")}}
    \item{-}{\code{signature(e1 = "AffLinDiscreteDistribution", e2 = "numeric")}}
    \item{*}{\code{signature(e1 = "numeric", e2 = "AffLinDiscreteDistribution")}}
    \item{+}{\code{signature(e1 = "numeric", e2 = "AffLinDiscreteDistribution")}}
    \item{-}{\code{signature(e1 = "numeric", e2 = "AffLinDiscreteDistribution")}}
  }
  There also is a class union of \code{"AffLinAbscontDistribution"} and
  \code{"AffLinDiscreteDistribution"} called \code{"AffLinDistribution"}
  which is used for functionals.
}

\author{
  Thomas Stabla \email{statho3@web.de},\cr 
  Florian Camphausen \email{fcampi@gmx.de},\cr
  Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr 
  Matthias Kohl \email{Matthias.Kohl@stamats.de}
  }

\note{ Working with a computer, we use a finite interval as support which carries at least mass \code{1-getdistrOption("TruncQuantile")}. }

\seealso{
\code{\link{Parameter-class}}
\code{\link{UnivariateDistribution-class}}
\code{\link{LatticeDistribution-class}}
\code{\link{AbscontDistribution-class}}
\code{\link{Reals-class}}
\code{\link{RtoDPQ.d}}
}
\examples{
# Dirac-measure at 0
D1 <- DiscreteDistribution(supp = 0)
support(D1)

# simple discrete distribution
D2 <- DiscreteDistribution(supp = c(1:5), prob = c(0.1, 0.2, 0.3, 0.2, 0.2))
plot(D2)
(pp <- p(D2)(support(D2)))
p(D2)(support(D2)-1e-5)
p(D2)(support(D2)+1e-5)
p.l(D2)(support(D2))
p.l(D2)(support(D2)-1e-5)
p.l(D2)(support(D2)+1e-5)
q(D2)(pp)
q(D2)(pp-1e-5)
q(D2)(pp+1e-5)
q.r(D2)(pp)
q.r(D2)(pp-1e-5)
q.r(D2)(pp+1e-5)
}
\keyword{distribution}
\concept{discrete distribution}
\concept{lattice distribution}
\concept{support of a distribution}
\concept{S4 distribution class}
\concept{generating function}