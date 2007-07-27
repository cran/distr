\name{DistrList}
\alias{DistrList}

\title{Generating function for DistrList-class}
\description{Generates an object of class \code{"DistrList"}.}
\usage{DistrList(...)}
\arguments{
  \item{\dots}{Objects of class \code{"Distribution"} which
    shall form the list of distributions.}
}
%\details{}
\value{Object of class \code{"DistrList"}}
%\references{}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link{DistrList-class}}, \code{\link{UnivarDistrList-class}},
  \code{\link{UnivarDistrList}}}
\examples{
(DL <- DistrList(Norm(), Exp(), Pois()))
plot(DL)
as(Norm(), "DistrList")

## The function is currently defined as
function(...){ 
    new("DistrList", list(...)) 
}
}
\keyword{distribution}
\keyword{list}
\concept{multivariate distribution}
\concept{S4 distribution class}
\concept{generating function}
