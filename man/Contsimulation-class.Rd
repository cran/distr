\name{Contsimulation-class}
\docType{class}
\alias{Contsimulation-class}
\alias{Contsimulation}
\alias{initialize,Contsimulation-method}

\title{Class "Contsimulation" }
\description{  In an object of type \code{Contsimulation} data can be simulated in any distribution and size. One part 
(usually the largest) of the random numbers stems from an ideal distribution, the rest is contaminated.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{Contsimulation(filename, runs, samplesize, seed, distribution.id,
distribution.c, rate)}.
A \code{Contsimulation}-object includes a filename, the number of runs, the size of the sample, the seed, the distribution
of the ideal and the contaminated data and the contamination rate. The slot Data stays empty until the method simulate has
been used.}
\section{Slots}{
  \describe{
    \item{\code{ind}:}{Object of class \code{"vectororNULL"}: Indicator of the same length as the data; saves whether each
    element of the data vector is contaminated or not}
    \item{\code{Data.id}:}{Object of class \code{"vectororNULL"}: -- the ideal data }
    \item{\code{Data.c}:}{Object of class \code{"vectororNULL"}: -- the contaminated data }
    \item{\code{rate}:}{Object of class \code{"numeric"}: the contamination rate, so the probability for each random number
    to be contaminated }
    \item{\code{distribution.c}:}{Object of class \code{"UnivariateDistribution"}: the distribution of the ideal data }
    \item{\code{distribution.id}:}{Object of class \code{"UnivariateDistribution"}: the distribution of the contaminated
    data }
    \item{\code{seed}:}{Object of class \code{"list"}: the seed the simulation has been generated with }
    \item{\code{filename}:}{Object of class \code{"character"}: the filename the Contsimulation shall be saved }
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
    \item{Data.c}{\code{signature(object = "Contsimulation")}: returns the contaminated data }
    \item{Data.id}{\code{signature(object = "Contsimulation")}: returns the ideal data }
    \item{Data<-}{\code{signature(object = "Contsimulation")}: ERROR: A modification of simulated data is not allowed. }
    \item{distribution.c}{\code{signature(object = "Contsimulation")}: returns the distribution of the contaminated data }
    \item{distribution.c<-}{\code{signature(object = "Contsimulation")}: changes the distribution of the contaminated data}
    \item{distribution.id}{\code{signature(object = "Contsimulation")}: returns the distribution of the ideal data }
    \item{distribution.id<-}{\code{signature(object = "Contsimulation")}: changes the distribution of the ideal data }
    \item{seed}{\code{signature(object = "Contsimulation")}: returns the seed }
    \item{seed<-}{\code{signature(object = "Contsimulation")}: changes the seed }
    \item{ind}{\code{signature(object = "Contsimulation")}: returns the indicator which saves which data is contaminated }
    \item{initialize}{\code{signature(.Object = "Contsimulation")}: initialize method }
    \item{rate}{\code{signature(object = "Contsimulation")}: returns the contamination rate }
    \item{rate<-}{\code{signature(object = "Contsimulation")}: changes the contamination rate }
    \item{runs<-}{\code{signature(object = "Contsimulation")}: changes the number of runs }
    \item{samplesize<-}{\code{signature(object = "Contsimulation")}: changes the size of the sample }
    \item{savedata}{\code{signature(object = "Contsimulation")}: saves the simulation in the directory of R }
    \item{simulate}{\code{signature(x = "Contsimulation")}: generates the random numbers for the simulation }
    \item{plot}{\code{signature(x = "Contsimulation")}: produces a plot of the real data matrix }
    \item{print}{\code{signature(x = "Contsimulation")}: returns filename, seed, number of runs, the size of the sample, 
    the rate and the distributions }
    \item{summary}{\code{signature(object = "Contsimulation")}: returns filename, seed, number of runs, the size of the
    sample, the rate and a statistical summary for each run of the real data}
  }
}

\note{ Changing distributions, seed, runs, samplesize or rate deletes possibly simulated data, as it would not fit to the
new parameters. }


\author{Thomas Stabla \email{Thomas.Stabla@uni-bayreuth.de},\cr Florian Camphausen \email{Florian.Camphausen@uni-bayreuth.de},\cr
 Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr Matthias Kohl \email{Matthias.Kohl@uni-bayreuth.de}}

\seealso{
\code{\link{Dataclass-class}}
\code{\link{Simulation-class}}
\code{\link{savedata-methods}}
\code{\link{plot-methods}}
\code{\link{simulate-methods}}
\code{\link{summary-methods}}
}
\examples{
N <- Norm() # N is a standard normal distribution.
C <- Cauchy() # C is a Cauchy distribution
cs <- Contsimulation(filename = "csim",
                     runs = 10,
                     samplesize = 3,
                     seed = setRNG(),
                     distribution.id = N,
                     distribution.c = C,
                     rate = 0.1)
simulate(cs)
# Each of the 30 random numbers is ideal (N-distributed) with
# probability 0.9 and contaminated (C-distributed) with
# probability = 0.1
summary(cs)
Data(cs) # different data
savedata(cs) # saves the object in the working directory of R...
load("csim") # loads it again...
Data(cs) # ...without the data - use simulate to return it!
}
\keyword{}
