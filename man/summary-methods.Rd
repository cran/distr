\name{summary-methods}
\docType{methods}
\alias{summary-methods}
\alias{summary,Dataclass-method}
\alias{summary,Simulation-method}
\alias{summary,Contsimulation-method}
\alias{summary,Evaluation-method}
\title{ Methods for Function summary in Package `distr' }
\description{summary-methods}

\section{Methods}{\describe{


\item{summary}{\code{signature(object = "Dataclass")}: returns the same information as print, moreover a statistical summary for 
each run }

\item{summary}{\code{signature(object = "Simulation")}: returns filename, seed, number of runs, the size of the sample and a
statistical summary for each run }

\item{summary}{\code{signature(object = "Contsimulation")}: returns filename, seed, number of runs, the size of the sample, the
rate and a statistical summary for each run of the real data }

\item{summary}{\code{signature(object = "Evaluation")}: returns the name of the data object, its filename, the estimator used and
a statistical summary of the result }

}}
\keyword{}
