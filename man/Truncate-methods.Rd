\name{Truncate-methods}
\docType{methods}
\alias{Truncate-methods}
\alias{Truncate}
\alias{Truncate,AbscontDistribution-method}
\alias{Truncate,DiscreteDistribution-method}
\alias{Truncate,UnivarLebDecDistribution-method}
\title{ Methods for function Truncate in Package `distr' }
\description{Truncate-methods}
\usage{
Truncate(object, ...)
\S4method{Truncate}{AbscontDistribution}(object, lower = -Inf, upper = Inf)
\S4method{Truncate}{DiscreteDistribution}(object, lower= -Inf, upper = Inf)
\S4method{Truncate}{UnivarLebDecDistribution}(object, lower = -Inf, upper = Inf, 
                    withSimplify = getdistrOption("simplifyD"))
}
\arguments{
  \item{object}{distribution object}
  \item{\dots}{not yet used; takes up \code{lower}, 
  \code{upper}, \code{withSimplify}.}
  \item{lower}{numeric; lower truncation point}
  \item{upper}{numeric; upper truncation point}
  \item{withSimplify}{logical; is result to be piped through a call to 
         \code{\link{simplifyD}}?}
}
\value{the corresponding distribution of the truncated random variable}
\section{Methods}{\describe{
\item{Truncate}{\code{signature(object = "AbscontDistribution")}: 
returns the distribution of \code{min(upper,max(X,lower))}
conditioned to \code{lower<X<=upper}, if \code{X} is 
distributed according to \code{object};
the result is again of class \code{"AbscontDistribution"}}
\item{Truncate}{\code{signature(object = "DiscreteDistribution")}: 
returns the distribution of \code{min(upper,max(X,lower))}
conditioned to \code{lower<X<=upper}, if \code{X} is 
distributed according to \code{object};
the result is again of class \code{"DiscreteDistribution"}}
\item{Truncate}{\code{signature(object = "UnivarLebDecDistribution")}: 
returns the distribution of \code{min(upper,max(X,lower))}
conditioned to \code{lower<X<=upper}, if \code{X} is 
distributed according to \code{object};
the result is again of class \code{"UnivarLebDecDistribution"}}
}}
\seealso{\code{\link{Huberize}}, \code{\link{Minimum}}}
\examples{
plot(Truncate(Norm(),lower=-1,upper=2))
}
\keyword{distribution}
\keyword{methods}
\concept{truncation}
