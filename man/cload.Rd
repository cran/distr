\name{cload}
\alias{cload}
\title{ cload }
\description{
loads the comment file from a saved \code{Dataclass} or \code{Evaluation} object
}
\usage{
cload(filename)
}
\arguments{
  \item{filename}{ the filename which was declared at the instantiaton of the Dataclass }
}
\details{}
\value{ no value is returned }
\author{Thomas Stabla \email{Thomas.Stabla@uni-bayreuth.de},\cr Florian Camphausen \email{Florian.Camphausen@uni-bayreuth.de},\cr
 Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr Matthias Kohl \email{Matthias.Kohl@uni-bayreuth.de}}
\seealso{
\code{\link{Evaluation-class}}
\code{\link{Dataclass-class}}
\code{\link{load}}
\code{\link{savedata-methods}}
}

\examples{
# see Dataclass and Evaluation for examples
## The function is currently defined as
function(filename){
  eval.parent(parse(text=paste("load(\"",filename,".comment\")", sep = "")))
  }
}
\keyword{}
