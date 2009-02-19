\name{UnivarDistrList}
\alias{UnivarDistrList}

\title{Generating function for UnivarDistrList-class}
\description{Generates an object of class \code{"UnivarDistrList"}.}
\usage{UnivarDistrList(...)}
\arguments{
  \item{\dots}{Objects of class \code{"UnivariateDistribution"} which
    shall form the list of univariate distributions.}
}
%\details{}
\value{Object of class \code{"UnivarDistrList"}}
%\references{}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link{DistrList-class}}, \code{\link{UnivarDistrList-class}},
  \code{\link{UnivarDistrList}}}
\examples{
(DL <- UnivarDistrList(Norm(), Exp(), Pois()))
plot(DL)
as(Norm(), "UnivarDistrList")

## The function is currently defined as
function(...){ 
    new("UnivarDistrList", list(...)) 
}
}
\keyword{list}
\keyword{distribution}
\concept{univariate distribution}
\concept{S4 distribution class}

