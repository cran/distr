\name{print-methods}
\docType{methods}
\alias{print-methods}
\alias{print,UnivariateDistribution-method}
\alias{print,Dataclass-method}
\alias{print,Simulation-method}
\alias{print,Contsimulation-method}
\alias{print,Evaluation-method}
\title{ Methods for Function print in Package `distr' }
\description{print-methods}

\section{Methods}{\describe{

\item{print}{\code{signature(x = "UnivariateDistribution")}: returns the class of the object and its parameters }

\item{print}{\code{signature(x = "Dataclass")}: returns filename, number of runs and the size of the sample }

\item{print}{\code{signature(x = "Simulation")}: returns filename, seed, number of runs, the size of the sample and the distribution }

\item{print}{\code{signature(x = "Contsimulation")}: returns filename, seed, number of runs, the size of the sample, the rate and the distributions }

\item{print}{\code{signature(x = "Evaluation")}: returns the name of the data object, its filename, the estimator used and the result }
}}
\keyword{}
