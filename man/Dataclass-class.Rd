\name{Dataclass-class}
\docType{class}
\alias{Dataclass-class}
\alias{Dataclass}
\alias{initialize,Dataclass-method}
\title{Class "Dataclass" }
\description{
  In an object of type "Dataclass" data can be saved containing any
  number of runs in any dimension. All information about the data is
  stored in a unified way.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{Dataclass(filename, Data)}. 
  A Dataclass-object includes, aside from the actual data, a filename
  and the number of runs and the size of the sample, which give the
  number of rows and columns of the data matrix.
}
\section{Slots}{
  \describe{
    \item{\code{filename}:}{
      Object of class \code{"character"}: the filename the data shall be saved}
    \item{\code{Data}:}{Object of class \code{"vectororNULL"}:
      the actual data, either of type "NULL" (means no data) or "vector"}
    \item{\code{runs}:}{Object of class \code{"numeric"}:
      the number of runs of the data}
    \item{\code{samplesize}:}{Object of class \code{"numeric"}:
      the size of the sample}
  }
}
\section{Methods}{
  \describe{
    \item{Data}{\code{signature(object = "Dataclass")}:
      returns the actual data}
    \item{Data<-}{\code{signature(object = "Dataclass")}:
      changes the data}
    \item{evaluate}{\code{signature(object = "Dataclass", estimator =
	"function")}:

      creates an object of type "Evaluation", see there for further information }
    \item{filename}{\code{signature(object = "Dataclass")}:
      returns the the filename}
    \item{filename<-}{\code{signature(object = "Dataclass")}:
      changes the the filename}
    \item{initialize}{\code{signature(.Object = "Dataclass")}:
      initialize method}
    \item{runs}{\code{signature(object = "Dataclass")}:
      returns the number of runs}
    \item{samplesize}{\code{signature(object = "Dataclass")}:
      returns the size of the sample}
    \item{savedata}{\code{signature(object = "Dataclass")}:
      saves the object in the directory of R and also a copy without data}
    \item{plot}{\code{signature(x = "Dataclass")}:
      produces a plot of the data matrix}
    \item{print}{\code{signature(x = "Dataclass")}:
      returns filename, number of runs and the size of the sample}
    \item{summary}{\code{signature(object = "Dataclass")}:
      returns the same information as print, moreover a statistical
      summary for each run}
  }
}

\author{
  Thomas Stabla \email{Thomas.Stabla@uni-bayreuth.de},\cr
  Florian Camphausen \email{Florian.Camphausen@uni-bayreuth.de},\cr
  Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr
  Matthias Kohl \email{Matthias.Kohl@uni-bayreuth.de}
}
\note{
  The saved Dataclass can be loaded with the usual load-command, the
  saved comment with the function cload.
}
\seealso{
\code{\link{Simulation-class}}
\code{\link{Contsimulation-class}}
\code{\link{Evaluation-class}}
\code{\link{plot-methods}} \cr
\code{\link{print-methods}}
\code{\link{summary-methods}}
\code{\link{load}}
\code{\link{cload}}
\code{\link{savedata-methods}}
}
\examples{
D <- Dataclass(Data = matrix(c(1,2,3,4,5,6),2),
               filename = "xyz.sav")
# A new object of type "Dataclass" is created.
savedata(D)
# creates a file called "xyz.sav" where the information is saved and a
# copy "xyz.sav.comment" without data
Data(D) <- matrix(c(11,12,13,14,15,16),2) # changes the data of D
cload("xyz.sav") # loads the object without data - it is called "D.comment"
D.comment
load("xyz.sav") # loads the original object "D"
Data(D) # the original data: matrix(c(1,2,3,4,5,6),2)
evaluate(object = D, estimator = mean) # returns the mean of each variable
}
\keyword{}
