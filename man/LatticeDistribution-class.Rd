\name{LatticeDistribution-class}
\docType{class}
\alias{AffLinLatticeDistribution-class}
\alias{LatticeDistribution-class}
\alias{LatticeDistribution}
\alias{lattice}
\alias{lattice-method}
\alias{lattice,LatticeDistribution-method}
\alias{initialize,LatticeDistribution-method}
\alias{initialize,AffLinLatticeDistribution-method}
\alias{coerce,LatticeDistribution,DiscreteDistribution-method}
\alias{coerce,AffLinLatticeDistribution,AffLinDiscreteDistribution-method}


\title{Class "LatticeDistribution"}
\description{The \code{LatticeDistribution}-class is the mother-class of the 
classes \code{Binom}, \code{Dirac}, \code{Geom}, \code{Hyper}, \code{Nbinom} and 
\code{Poisson}. It formalizes a distribution on a regular affine
linear lattice.}
\section{Objects from the Class}{
The usual way to generate objects of class \code{LatticeDistribution} is to call 
the generating function \code{LatticeDistribution()}  (see details). \cr
Somewhat more flexible, but also proner to inconsistencies is a call to 
\code{new("LatticeDistribution")}, where you may explicitly specify random 
number generator, (counting) density, cumulative distribution and quantile 
functions. For conveniance, in this call to \code{new("LatticeDistribution")}, 
an additional possibility is to only specify the random number generator. The 
function \code{RtoDPQ.d} then approximates the three remaining slots \code{d}, 
\code{p} and \code{q} by random sampling. 
}

\synopsis{LatticeDistribution(lattice = NULL, supp = NULL, prob = NULL, 
                       .withArith = FALSE, .withSim = FALSE, 
                       DiscreteDistribution = NULL, check = TRUE)}
\usage{
  LatticeDistribution(DiscreteDistribution)
  LatticeDistribution(lattice, DiscreteDistribution)
  LatticeDistribution(lattice, supp, prob, .withArith, .withSim, check = FALSE)
  LatticeDistribution(lattice, supp, prob)
  LatticeDistribution(supp)
}
\arguments{
  \item{DiscreteDistribution}{an object of class \code{DiscreteDistribution} 
  or \code{AffLinDiscreteDistribution} to be
  coerced to \code{LatticeDistribution} or \code{AffLinLatticeDistribution}, 
  respectively}
  \item{lattice}{lattice (of class \code{Lattice}) which determines the support 
                 of the discrete distribution. }
  \item{supp}{ numeric vector which forms the support 
    of the discrete distribution. }
  \item{prob}{ vector of probability weights for the 
    elements of \code{supp}.}
  \item{.withArith}{normally not set by the user, but if determining the entries 
                    \code{supp}, \code{prob} distributional arithmetics was 
                    involved, you may set this to \code{TRUE}.}
  \item{.withSim}{normally not set by the user, but if determining the entries 
                  \code{supp}, \code{prob} simulations were involved, you may 
                  set this to \code{TRUE}.}
  \item{check}{ logical: if \code{TRUE}, \code{LatticeDistribution()} throws an
               error if argument \code{lattice} and other arguments are 
               inconsistent or if there is no way to automatically generate 
               a \code{lattice} argument. If \code{check == FALSE}, 
               \code{LatticeDistribution()}
               returns an object of \code{DiscreteDistribution}, ignoring
               argument \code{lattice}}
}
\details{
  For the generating function \code{LatticeDistribution()}, the arguments
  are processed in the following order:\cr
%
  Arguments \code{.withSim} and \code{.withArith} are used in any case.\cr 
%
  If there is an argument \code{DiscreteDistribution} (of the respective class), 
  all its slots (except for \code{.withSim} and \code{.withArith})
  will be used for the filling the slots of the object of class 
  \code{LatticeDistribution()}/\code{AffLinLatticeDistribution()}. 
  If in addition, there is an argument \code{lattice} of class \code{Lattice}, 
  it will be checked for consistency
  with argument \code{DiscreteDistribution} and if oK will be used for slot
  lattice of the object of class 
  \code{LatticeDistribution()}/\code{AffLinLatticeDistribution()}. In case 
  there is no \code{lattice} argument, slot \code{lattice} will be constructed
  from slot \code{support} from argument \code{DiscreteDistribution}.\cr
%
  If there is no argument \code{DiscreteDistribution}, but there are arguments
  \code{supp} and \code{lattice} (the latter of class \code{Lattice}) then
  these are checked for consistency and if oK, generating function
  \code{DiscreteDistribution()} is called with arguments \code{supp}, 
  \code{prob}, \code{.withArith}, and \code{.withSim} to produce an object
  of class \code{DiscreteDistribution} the slots of which will be used for the 
  filling the slots of the object of class 
  \code{LatticeDistribution()}/\code{AffLinLatticeDistribution()}.
  If in this case, argument \code{prob} is not given explicitely, all elements 
  in \code{supp} are equally weighted.
  \cr
%
  If there is no argument \code{DiscreteDistribution}, but there is an argument
  \code{lattice} of class \code{Lattice} (but no argument \code{slot}) then
 if \code{Length(lattice)} is finite, a corresponding support vector \code{supp}
  is generated from argument \code{lattice} and generating function
  \code{DiscreteDistribution()} is called with arguments \code{supp}, 
  \code{prob}, \code{.withArith}, and \code{.withSim} to produce an object
  of class \code{DiscreteDistribution} the slots of which will be used for the 
  filling the slots of the object of class \code{LatticeDistribution()}.
  If in the same situation \code{Length(lattice)} is not finite, a finite length
  for the support vector is extracted from argument \code{prob} and after 
  generating \code{supp} one procedes as in the finite \code{Length(lattice)}
  case. \cr
%
  If there is no argument \code{DiscreteDistribution} and no argument
  \code{lattice} of class \code{Lattice} but an argument \code{supp} then
  it will be checked if \code{supp} makes for a lattice, and if so,
  \code{DiscreteDistribution()} is called with arguments \code{supp}, 
  \code{prob}, \code{.withArith}, and \code{.withSim} to produce an object
  of class \code{DiscreteDistribution} the slots of which will be used for the 
  filling the slots of the object of class \code{LatticeDistribution()}. The
  corresponding \code{lattice}-slot will be filled with information from
  argument \code{supp}.  \cr
  %    
  The price for this flexibility of arguments, \code{LatticeDistribution()} may
  be called with, is that you should call \code{LatticeDistribution()} with
  \emph{named arguments} only.\cr
%
  Note that internally we suppress lattice points from the support where
  the probability is 0.
}

\section{Slots}{
  \describe{
    \item{\code{img}:}{Object of class \code{"Reals"}: the space of the image 
    of this distribution which has dimension 1 and the name "Real Space" }
    \item{\code{param}:}{Object of class \code{"Parameter"}: the parameter of 
    this distribution, having only the slot name 
    "Parameter of a discrete distribution" }
    \item{\code{r}:}{Object of class \code{"function"}: 
                     generates random numbers}
    \item{\code{d}:}{Object of class \code{"function"}: 
                     (counting) density/probability function}
    \item{\code{p}:}{Object of class \code{"function"}: 
                     cumulative distribution function}
    \item{\code{q}:}{Object of class \code{"function"}: 
                     quantile function}
    \item{\code{.withArith}:}{logical: used internally to issue warnings as to 
                              interpretation of arithmetics}
    \item{\code{.withSim}:}{logical: used internally to issue warnings as 
                            to accuracy}
    \item{\code{support}:}{Object of class \code{"numeric"}: a (sorted) vector 
                           containing the support of the discrete
    density function}
    \item{\code{lattice}:}{Object of class \code{"Lattice"}: the lattice 
                           generating the support.}
  }
}
\section{Extends}{
Class \code{"UnivariateDistribution"}, directly.\cr
Class \code{"Distribution"}, by class \code{"UnivariateDistribution"}.
}
\section{Methods}{
  \describe{
    \item{\code{initialize}}{\code{signature(.Object = "LatticeDistribution")}: 
                              initialize method }
    \item{-}{\code{signature(e1 = "LatticeDistribution")}: 
                   application of `-' to this lattice distribution}
    \item{*}{\code{signature(e1 = "LatticeDistribution", e2 = "numeric")}: 
                   multiplication of this lattice distribution
    by an object of class `numeric'}
    \item{/}{\code{signature(e1 = "LatticeDistribution", e2 = "numeric")}: 
    division of this lattice distribution by an object of class `numeric'}
    \item{+}{\code{signature(e1 = "LatticeDistribution", e2 = "numeric")}: 
    addition of this lattice distribution to an object of class `numeric'}
    \item{-}{\code{signature(e1 = "LatticeDistribution", e2 = "numeric")}: 
    subtraction of an object of class `numeric' from this lattice 
    distribution }
    \item{*}{\code{signature(e1 = "numeric", e2 = "LatticeDistribution")}: 
    multiplication of this lattice distribution by an object of class `numeric'}
    \item{+}{\code{signature(e1 = "numeric", e2 = "LatticeDistribution")}: 
    addition of this lattice distribution to an object of class `numeric'}
    \item{-}{\code{signature(e1 = "numeric", e2 = "LatticeDistribution")}: 
    subtraction of this lattice distribution from an object of class `numeric'}
    \item{+}{\code{signature(e1 = "LatticeDistribution", 
    e2 = "LatticeDistribution")}: Convolution of two lattice distributions. 
    Slots p, d and q are approximated by grids.}
    \item{-}{\code{signature(e1 = "LatticeDistribution", 
    e2 = "LatticeDistribution")}: Convolution of two lattice
    distributions. The slots p, d and q are approximated by grids.}
    \item{\code{lattice}}{accessor method to the corresponding slot.}
    \item{\code{coerce}}{\code{signature(from = "LatticeDistribution",
    to = "DiscreteDistribution")}: coerces an object from 
    \code{"LatticeDistribution"} to \code{"DiscreteDistribution"}
    thereby cancelling out support points with probability 0.}
  }
}

\section{Internal subclass "AffLinLatticeDistribution"}{
To enhance accuracy of several functionals on distributions,
  mainly from package \pkg{distrEx}, there is an internally used 
  (but exported) subclass \code{"AffLinLatticeDistribution"} which has extra slots 
  \code{a}, \code{b} (both of class \code{"numeric"}),  and \code{X0} 
  (of class \code{"LatticeDistribution"}), to capture the fact 
  that the object has the same distribution as \code{a * X0 + b}. This is 
  the class of the return value of methods 
  \itemize{
    \item{-}{\code{signature(e1 = "LatticeDistribution")}}
    \item{*}{\code{signature(e1 = "LatticeDistribution", e2 = "numeric")}}
    \item{/}{\code{signature(e1 = "LatticeDistribution", e2 = "numeric")}}
    \item{+}{\code{signature(e1 = "LatticeDistribution", e2 = "numeric")}}
    \item{-}{\code{signature(e1 = "LatticeDistribution", e2 = "numeric")}}
    \item{*}{\code{signature(e1 = "numeric", e2 = "LatticeDistribution")}}
    \item{+}{\code{signature(e1 = "numeric", e2 = "LatticeDistribution")}}
    \item{-}{\code{signature(e1 = "numeric", e2 = "LatticeDistribution")}}
    \item{-}{\code{signature(e1 = "AffLinLatticeDistribution")}}
    \item{*}{\code{signature(e1 = "AffLinLatticeDistribution", e2 = "numeric")}}
    \item{/}{\code{signature(e1 = "AffLinLatticeDistribution", e2 = "numeric")}}
    \item{+}{\code{signature(e1 = "AffLinLatticeDistribution", e2 = "numeric")}}
    \item{-}{\code{signature(e1 = "AffLinLatticeDistribution", e2 = "numeric")}}
    \item{*}{\code{signature(e1 = "numeric", e2 = "AffLinLatticeDistribution")}}
    \item{+}{\code{signature(e1 = "numeric", e2 = "AffLinLatticeDistribution")}}
    \item{-}{\code{signature(e1 = "numeric", e2 = "AffLinLatticeDistribution")}}
  }
 There is also an explicit \code{coerce}-method from class
 \code{"AffLinLatticeDistribution"} to class \code{"AffLinDoscreteDistribution"}
 which cancels out support points with probability 0.
}

\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de}}

\note{ Working with a computer, we use a finite interval as support which 
carries at least mass \code{1-getdistrOption("TruncQuantile")}. }

\seealso{
\code{\link{Parameter-class}}
\code{\link{Lattice-class}}
\code{\link{UnivariateDistribution-class}}
\code{\link{DiscreteDistribution-class}}
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
B <- Binom(prob = 0.1,size = 10) # B is a Binomial distribution w/ prob=0.1 and size=10.
P <- Pois(lambda = 1) # P is a Poisson distribution with lambda = 1.
D1 <- B+1 # a new Lattice distributions with exact slots d, p, q
D2 <- D1*3 # a new Lattice distributions with exact slots d, p, q
D3 <- B+P # a new Lattice distributions with approximated slots d, p, q
D4 <- D1+P # a new Lattice distributions with approximated slots d, p, q
support(D4) # the (approximated) support of this distribution is 1, 2, ..., 21
r(D4)(1) # one random number generated from this distribution, e.g. 4
d(D4)(1) # The (approximated) density for x=1 is 0.1282716.
p(D4)(1) # The (approximated) probability that x<=1 is 0.1282716.
q(D4)(.5) # The (approximated) 50 percent quantile is 3.
}
\keyword{distribution}
\concept{discrete distribution}
\concept{lattice distribution}
\concept{lattice of a distribution}
\concept{S4 distribution class}
\concept{generating function}