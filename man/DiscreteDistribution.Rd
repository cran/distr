\name{DiscreteDistribution}
\alias{DiscreteDistribution}

\title{Generating function "DiscreteDistribution"}
\description{Generates an object of class  \code{"DiscreteDistribution"}}

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


\author{
  Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de},\cr
  Matthias Kohl \email{Matthias.Kohl@stamats.de}
  }

\note{ Working with a computer, we use a finite interval as support which carries at least mass \code{1-getdistrOption("TruncQuantile")}. }

\seealso{
\code{\link{DiscreteDistribution-class}}
\code{\link{AbscontDistribution-class}}
\code{\link{RtoDPQ.d}}
}
\examples{
# Dirac-measure at 0
D1 <- DiscreteDistribution(supp = 0)
D1
# simple discrete distribution
D2 <- DiscreteDistribution(supp = c(1:5), prob = c(0.1, 0.2, 0.3, 0.2, 0.2))
D2

plot(D2)
}
\keyword{distribution}
\concept{discrete distribution}
\concept{generating function}
