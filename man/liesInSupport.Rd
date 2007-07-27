\name{liesInSupport}
\alias{liesInSupport}
\alias{liesInSupport-methods}
\alias{liesInSupport,DiscreteDistribution,numeric-method}
\alias{liesInSupport,DiscreteDistribution,matrix-method}
\alias{liesInSupport,AbscontDistribution,numeric-method}
\alias{liesInSupport,AbscontDistribution,matrix-method}

\title{Generic Function for Testing the Support of a Distribution }
\description{
  The function tests if \code{x} lies in the support of the
  distribution \code{object}.
}
\usage{
liesInSupport(object, x)
}
\arguments{
  \item{object}{ object of class \code{"Distribution"} }
  \item{x}{ numeric vector or matrix }
}
%\details{}
\value{logical vector}
\section{Methods}{
\describe{
  \item{object = "DiscreteDistribution", x = "numeric":}{ 
    does \code{x} lie in the support of \code{object}. }

  \item{object = "DiscreteDistribution", x = "matrix":}{
    does \code{x} lie in the support of \code{object}. }

  \item{object = "AbscontDistribution", x = "numeric":}{
    does \code{x} lie in the support of \code{object}. }

  \item{object = "AbscontDistribution", x = "matrix":}{ 
    does \code{x} lie in the support of \code{object}. }

}}
%\references{}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link{Distribution-class}}}
\examples{
liesInSupport(Exp(1), rnorm(10))

# note
x <- rpois(10, lam = 10)
liesInSupport(Pois(1), x)
# better
distroptions("TruncQuantile"=1e-15)
liesInSupport(Pois(1), x)
distroptions("TruncQuantile"=1e-05) # default

}
\keyword{distribution}
\keyword{methods}
\concept{space}
\concept{S4 space class}
\concept{support}
