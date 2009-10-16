\name{UnivarLebDecDistribution}
\alias{UnivarLebDecDistribution}

\title{Generating function for Class "UnivarLebDecDistribution"}
\description{Generates an object of class \code{"UnivarLebDecDistribution"}.}
\usage{
UnivarLebDecDistribution(acPart, discretePart, acWeight, discreteWeight,
                                     r = NULL, e = NULL, n = NULL)
}
\arguments{
  \item{acPart}{Object of class \code{"AbscontDistribution"} (or subclasses); 
                a.c. part of the distribution}
  \item{discretePart}{Object of class \code{"AbscontDistribution"} (or subclasses);
  discrete part of the distribution}
  \item{acWeight}{Object of class \code{"numeric"}; weight of the a.c. part of 
                  the distribution}
  \item{discreteWeight}{Object of class \code{"numeric"}; weight of the discrete 
        part of the distribution}
  \item{r}{optional argument; if given, this is a random number generator as function
           \code{r <- function(n){....}} to produce r.v.'s distributed 
            according to the distribution; used in a call to \code{\link{RtoDPQ.LC}}
            if  \code{acPart} and \code{discretePart} are missing.}
  \item{e}{optional argument; if argument \code{r} is given, this is the number
           of r.v.'s drawn to fill the empty slots of this object; if missing filled
           with \code{getdistrOption("RtoDPQ.e")}.}
  \item{n}{optional argument; if argument \code{r} is given, this is the number
           gridpoints used in filling the empty p,d,q slots of this object; if missing filled
           with \code{getdistrOption("DefaultNrGridPoints")}.}
}

\details{At least one of arguments \code{discretePart}, \code{acPart}, or \code{r}
         must be given; if the first two are missing, slots are filled by a call
         to \code{RtoDPQ.LV}. For this purpose argument \code{r} is used together
         with arguments \code{e} and \code{n}. If the latter are missing they are
         filled with \code{getdistrOption("RtoDPQ.e")} and 
         \code{getdistrOption("DefaultNrGridPoints")}, respectively.
         If argument \code{discretePart} is missing but  \code{acPart} is not,
         \code{discreteWeight} is set to 0 and \code{discretePart} is set to \code{Dirac(0)}.
         If argument   \code{acPart}  is missing but \code{discretePart} is not,
         \code{acWeight} is set to 0 and \code{discretePart} is set to \code{Norm()}.
         If both arguments   \code{acPart} and \code{discretePart} are given,
         at least one of arguments \code{discreteWeight} and \code{acWeight} must
         be given and lie in [0,1], else an error is thrown.
         If only one argument \code{acWeight} or \code{discreteWeight} is given
         the other one is gotten as 1-[ac/discrete]Weight.
         Else if both are given, they must sum up to 1.
         If a weight is smaller than \code{getdistrOption("TruncQuantile")}, it 
         is set to 0.
         }
\value{Object of class \code{"UnivarLebDecDistribution"}.}
%\references{}
\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
%\note{}
\seealso{\code{\link{UnivarLebDecDistribution-class}},
  \code{\link{simplifyD}}}
\examples{
mylist <- UnivarLebDecDistribution(discretePart=Binom(3,.3), acPart=Norm(2,2),
          acWeight=11/20)
mylist          
}
\keyword{distribution}
\keyword{list}
\concept{multivariate distribution}
\concept{S4 distribution class}
\concept{generating function}

