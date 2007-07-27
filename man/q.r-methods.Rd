\name{q.r-methods}
\docType{methods}
\alias{q.r-methods}
\alias{q.r}
\alias{q.r,DiscreteDistribution-method}
\title{Methods for Function q.r in Package `distr'}
\description{q.r-methods}

\section{Methods}{\describe{
\item{q.r}{\code{signature(object  = "DiscreteDistribution")}: returns the 
right-continuous quantile function, i.e.; 
\eqn{{\rm q.r}(s)=\sup\{t \,\big|\, P({\tt object}\leq t)\leq s\}}{q.r(s)=sup\{t|P(object<=t)<=s\}}
}}}

\seealso{
\code{\link{DiscreteDistribution-class}}
}
\keyword{distribution}
\keyword{methods}
\concept{pseudo-inverse distribution function}
\concept{quantile function}
\concept{left-continuous}
\concept{right-continuous}
