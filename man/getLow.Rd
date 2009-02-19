\name{getLow,getUp}
\alias{getUp}
\alias{getUp-methods}
\alias{getUp,AbscontDistribution-method}
\alias{getUp,DiscreteDistribution-method}
\alias{getLow}
\alias{getLow-methods}
\alias{getLow,AbscontDistribution-method}
\alias{getLow,DiscreteDistribution-method}

\title{getLow, getUp functions of package distr}

\description{
getLow, getUp return lower and upper endpoint of a distribution ---
truncated to lower/upper \code{TruncQuantile} if infinite.}

\usage{
\S4method{getUp}{AbscontDistribution}(object,
                                      eps = getdistrOption("TruncQuantile"))
\S4method{getUp}{DiscreteDistribution}(object)
\S4method{getLow}{AbscontDistribution}(object,
                                       eps = getdistrOption("TruncQuantile"))
\S4method{getLow}{DiscreteDistribution}(object)
}
\arguments{
  \item{object}{a distribution object}
  \item{eps}{truncation point (numeric)}
}



\value{
\item{getLow,getUp}{a \code{numeric} of length 1}
}


\author{
  Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}
  }

\keyword{methods}
\keyword{distribution}
\concept{support}
\concept{utilities}
