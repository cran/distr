\name{convpow-methods}
\docType{methods}
\alias{convpow}
\alias{convpow-methods}
\alias{convpow,AbscontDistribution-method}
\alias{convpow,LatticeDistribution-method}
\alias{convpow,Norm-method}
\alias{convpow,Binom-method}
\alias{convpow,Nbinom-method}
\alias{convpow,ExpOrGammaOrChisq-method}
\alias{convpow,Cauchy-method}
\alias{convpow,Pois-method}
\alias{convpow,Dirac-method}

\title{Distribution of the sum of univariate i.i.d r.v's}
\description{
  Method \code{convpow} determines the distribution of the sum of N univariate
  i.i.d r.v's by means of DFT}


\usage{
  convpow(D1,...)
  \S4method{convpow}{AbscontDistribution}(D1,N)
  \S4method{convpow}{LatticeDistribution}(D1,N)
}

\arguments{
  \item{D1}{an object of (a sub)class (of) \code{"AbscontDistribution"} or  
            \code{"LatticeDistribution"}}
  \item{...}{not yet used; meanwhile takes up N }
  \item{N}{ an integer }
  }
\details{
  in the methods implemented a second argument \code{N} is obligatory;
  the general methods use a general purpose convolution algorithm for 
  distributions by means of D/FFT.}

\value{Object of class \code{"AbscontDistribution"} resp. 
       \code{"LatticeDistribution"}}

\section{further S4-Methods}{
  There are particular methods for the following classes, using explicit 
  convolution formulae:
  \describe{
    \item{\code{signature(D1="Norm")}}{returns class \code{"Norm"}}
    \item{\code{signature(D1="Nbinom")}}{returns class \code{"Nbinom"}}
    \item{\code{signature(D1="Binom")}}{returns class \code{"Binom"}}
    \item{\code{signature(D1="Cauchy")}}{returns class \code{"Cauchy"}}
    \item{\code{signature(D1="ExpOrGammaOrChisq")}}{returns class 
         \code{"Gammad"} ---if \code{D1} may be coerced to \code{Gammad}}
    \item{\code{signature(D1="Pois")}}{returns class \code{"Pois"}}
    \item{\code{signature(D1="Dirac")}}{returns class \code{"Dirac"}}
  }
}

\author{Peter Ruckdeschel \email{peter.ruckdeschel@uni-bayreuth.de}\cr
Matthias Kohl \email{matthias.kohl@stamats.de}
Thomas Stabla \email{statho3@web.de}
}
%\note{}
\seealso{\code{\link{operators}}, \code{distrARITH()}}

\examples{
convpow(Exp()+Pois(),4)
}

\references{
Kohl, M., Ruckdeschel, P., Stabla, T. (2005): 
   General purpose convolution algorithm for distributions 
  in S4-Classes by means of FFT.
 Technical report, Feb. 2005. Also available in
\url{http://www.uni-bayreuth.de/departments/math/org/mathe7/RUCKDESCHEL/pubs/comp.pdf}
}

\concept{convolution for distributions}
\concept{arithmetics for distributions}
\concept{info file}
\concept{FFT}
\concept{DFT}
\keyword{distribution}

