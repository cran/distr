\name{showobj-methods}
\docType{methods}
\alias{showobj}
\alias{showobj-methods}
\alias{showobj,UnivariateDistribution-method}
\alias{showobj,DistrList-method}
\alias{showobj,UnivarMixingDistribution-method}
\alias{showobj,UnivarLebDecDistribution-method}
\title{Helping Methods for Functions print/show in Package `distr' }
\description{showobj-methods}

\usage{
showobj(object, ...)
\S4method{showobj}{UnivariateDistribution}(object)
\S4method{showobj}{DistrList}(object)
\S4method{showobj}{UnivarMixingDistribution}(object)
\S4method{showobj}{UnivarLebDecDistribution}(object,objs)
}
\arguments{
  \item{\dots}{further parameters for \code{showobj}; not yet used.}
  \item{object}{distribution [list] object}
  \item{objs}{a character capturing the input from \code{show}, \code{print}}
}
\value{a character vector, which is then shown by method \code{show},
but can also be indented if necessary --- like in the possibly nested 
list classes}

\seealso{\code{\link{print-methods}}}
\keyword{print}
\keyword{methods}
\keyword{internal}
\keyword{distribution}





