\name{Simulation-class}
\docType{class}
\alias{Simulation-class}
\alias{Simulation}
\alias{initialize,Simulation-method}

\title{Class "Simulation" }
\description{  In an object of type \code{Simulation} data can be simulated in any distribution and size.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{Simulation(filename, runs, samplesize, seed, distribution)}.
A \code{Simulation}-object includes a filename, the number of runs, the size of the sample, the seed and the distribution of the
random numbers. The slot \code{Data} stays empty until the method simulate has been used.}
\section{Slots}{
  \describe{
    \item{\code{seed}:}{Object of class \code{"list"}: the seed the simulation has been generated with }
    \item{\code{distribution}:}{Object of class \code{"UnivariateDistribution"}: the distribution of the random numbers}
    \item{\code{filename}:}{Object of class \code{"character"}: the filename the simulation shall be saved }
    \item{\code{Data}:}{Object of class \code{"vectororNULL"}: the simulated data }
    \item{\code{runs}:}{Object of class \code{"numeric"}: the number of runs of the data }
    \item{\code{samplesize}:}{Object of class \code{"numeric"}: the size of the sample, so the dimension of the data }
  }
}
\section{Extends}{
Class \code{"Dataclass"}, directly.
}
\section{Methods}{
  \describe{
    \item{Data<-}{\code{signature(object = "Contsimulation")}: ERROR: A modification of simulated data is not allowed. }
    \item{distribution}{\code{signature(object = "Simulation")}: returns the distribution }
    \item{distribution<-}{\code{signature(object = "Simulation")}: changes the distribution }
    \item{seed}{\code{signature(object = "Simulation")}: returns the seed }
    \item{seed<-}{\code{signature(object = "Simulation")}: changes the seed }
    \item{runs<-}{\code{signature(object = "Simulation")}: changes the number of runs }
    \item{samplesize<-}{\code{signature(object = "Simulation")}: changes the size of the sample }
    \item{savedata}{\code{signature(object = "Simulation")}: saves the object without the data in the directory of R
    (After loading the data can be reproduced by using simulate.) }
    \item{initialize}{\code{signature(.Object = "Simulation")}: initialize method }
    \item{plot}{\code{signature(x = "Simulation")}: produces a plot of the data matrix }
    \item{print}{\code{signature(x = "Simulation")}: returns filename, seed, number of runs, the size of the sample and the
    distribution }
    \item{simulate}{\code{signature(x = "Simulation")}: generates the random numbers for the simulation }
    \item{summary}{\code{signature(object = "Simulation")}: returns filename, seed, number of runs, the size of the sample
    and a statistical summary for each run }
}}

\note{ Changing distribution, seed, runs or samplesize deletes possibly simulated data, as it would not fit to the new
parameters. }


\author{Thomas Stabla \email{Thomas.Stabla@uni-bayreuth.de},\cr Florian Camphausen \email{Florian.Camphausen@uni-bayreuth.de},\cr
 Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr Matthias Kohl \email{Matthias.Kohl@uni-bayreuth.de}}

\seealso{
\code{\link{Dataclass-class}}
\code{\link{Contsimulation-class}}
\code{\link{plot-methods}}
\code{\link{print-methods}}
\code{\link{summary-methods}}
\code{\link{simulate-methods}}
}
\examples{
N=Norm() # N is a standard normal distribution.
S=Simulation(filename="xyz",runs=10,samplesize=3,seed=setRNG(),distribution=N)
Data(S) # no data yet
simulate(S)
Data(S) # now there are random numbers
Data(S) # the same data as before because the seed has not changed
seed(S)=setRNG()
simulate(S)
Data(S) # different data
savedata(S) # saves the object in the directory of R...
load("xyz") # loads it again...
Data(S) # ...without the data - use simulate to return it!
}
\keyword{}
