\name{prob-methods} 
\docType{methods}
\alias{prob-methods}
\alias{prob<--methods}
\alias{prob}
\alias{prob<-}
\alias{prob,BinomParameter-method}
\alias{prob,Binom-method}
\alias{prob,NbinomParameter-method}
\alias{prob,Nbinom-method}
\alias{prob,GeomParameter-method}
\alias{prob,Geom-method}
\alias{prob<-,BinomParameter-method}
\alias{prob<-,Binom-method}
\alias{prob<-,NbinomParameter-method}
\alias{prob<-,Nbinom-method}
\alias{prob<-,GeomParameter-method}
\alias{prob<-,Geom-method}
\title{ Methods for Function prob in Package `distr' }
\description{prob-methods}

\section{Methods}{\describe{

\item{prob}{\code{signature(object = "BinomParameter")}: returns the slot \code{prop} of the parameter of the distribution }
\item{prob<-}{\code{signature(object = "BinomParameter")}: modifies the slot \code{prob} of the parameter of the distribution }

\item{prob}{\code{signature(object = "Binom")}: returns the slot \code{prop} of the parameter of the distribution }
\item{prob<-}{\code{signature(object = "Binom")}: modifies the slot \code{prob} of the parameter of the distribution }

\item{prob}{\code{signature(object = "NbinomParameter")}: returns the slot \code{prop} of the parameter of the distribution }
\item{prob<-}{\code{signature(object = "NbinomParameter")}: modifies the slot \code{prob} of the parameter of the distribution }

\item{prob}{\code{signature(object = "Nbinom")}: returns the slot \code{prop} of the parameter of the distribution }
\item{prob<-}{\code{signature(object = "Nbinom")}: modifies the slot \code{prob} of the parameter of the distribution }

\item{prob}{\code{signature(object = "GeomParameter")}: returns the slot \code{prop} of the parameter of the distribution (deprecated from 1.9 on)}
\item{prob<-}{\code{signature(object = "GeomParameter")}: modifies the slot \code{prob} of the parameter of the distribution (deprecated from 1.9 on)}

\item{prob}{\code{signature(object = "Geom")}: returns the slot \code{prop} of the parameter of the distribution }
\item{prob<-}{\code{signature(object = "Geom")}: modifies the slot \code{prob} of the parameter of the distribution }
}}
\keyword{distribution}
\concept{parameter}
\concept{Binomial distribution}
\concept{Negative Binomial distribution}
\concept{Geometric distribution}
\concept{S4 parameter class}
\concept{accessor function}
\concept{replacement function}
