\name{q-methods}
\docType{methods}
\alias{q-methods}
\alias{q}
\alias{q,Distribution-method}
\title{Methods for Function q in Package `distr'}
\description{q-methods}

\section{Methods}{\describe{
\item{q}{\code{signature(object  = "Distribution")}: returns the
(left-continuous) quantile function, i.e.;
\eqn{{\rm q}(s)=\inf\{t \,\big|\, P({\tt object}\leq t)\geq s\}}{q(s)=inf\{t|P(object<=t)>=s\}}
}}}
\seealso{
\code{\link{Distribution-class}}
}
\keyword{distribution}
\keyword{methods}
\concept{pseudo-inverse distribution function}
\concept{quantile function}
\concept{accessor function}
\concept{left-continuous}
