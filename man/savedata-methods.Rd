\name{savedata-methods}
\docType{methods}
\alias{savedata-methods}
\alias{savedata}
\alias{savedata,Dataclass-method}
\alias{savedata,Simulation-method}
\alias{savedata,Contsimulation-method}
\alias{savedata,Evaluation-method}
\title{ Methods for Function savedata in Package `distr' }
\description{savedata-methods}

\section{Methods}{\describe{

\item{savedata}{\code{signature(object = "Dataclass")}: saves the object (with the data) in the directory of R }

\item{savedata}{\code{signature(object = "Simulation")}: saves the object without the data in the directory of R
    (After loading the data can be reproduced by using simulate.) }

\item{savedata}{\code{signature(object = "Contsimulation")}: saves the object without the data in the directory of R
    (After loading the data can be reproduced by using simulate.) }

\item{savedata}{\code{signature(object = "Evaluation")}: saves the object in two files in the directory of R - one with data, 
one without as comment file }
}}
\seealso{
\code{\link{Dataclass-class}}
\code{\link{Simulation-class}}
\code{\link{Contsimulation-class}}
\code{\link{Evaluation-class}}
}
\keyword{}
