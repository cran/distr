\name{seed-methods}
\docType{methods}
\alias{seed-methods}
\alias{seed<--methods}
\alias{seed}
\alias{seed<-}
\alias{seed,Simulation-method}
\alias{seed<-,Simulation-method}
\alias{seed,Contsimulation-method}
\alias{seed<-,Contsimulation-method}
\title{ Methods for Function seed in Package `distr' }
\description{seed-methods}

\section{Methods}{\describe{

\item{seed}{\code{signature(object = "Simulation")}: returns the slot \code{seed} of an object of class \code{"Simulation"}}
\item{seed<-}{\code{signature(object = "Simulation")}: changes the slot \code{seed} of an object of class \code{"Simulation"}}

\item{seed}{\code{signature(object = "Contsimulation")}: returns the slot \code{seed} of an object of class \code{"Contsimulation"} }
\item{seed<-}{\code{signature(object = "Contsimulation")}: changes the slot \code{seed} of an object of class \code{"Contsimulation"} }
}}
\note{The value to which the seed is set has to be consistent with the \pkg{setRNG}-package;
to draw a ``new'' simulation, use something like \code{seed(X)<-setRNG();simulate(X)}; 
cf. manual to this package, p.~9}
\keyword{}
