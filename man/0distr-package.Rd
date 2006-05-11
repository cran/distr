\name{distr-package}
\alias{distr-package}
\alias{distr}
\docType{package}
\title{
distr -- object orientated implementation of distributions
}
\description{
\pkg{distr} provides a conceptual treatment of distributions by means of S4 classes.
A mother class \code{Distribution} is introduced with slots for a parameter and ---most important---
for the four constitutive methods \code{r},  \code{d}, \code{p}, and \code{q} for simulation respectively for evaluation of
density / c.d.f.\ and quantile function of the corresponding distribution. 

Most distributions of  package \pkg{stats} (like normal, Poisson, etc.)
are implemented as subclasses of either \code{AbscontDistribution} or \code{DiscreteDistribution},
which themselves are again subclasses of \code{Distribution}. 

Arithmetics and unary mathematical transformations for distributions are available:
For \code{Distribution} objects \code{X} and \code{Y} expressions like
\code{3*X+sin(exp(-Y/4+3))} have their natural interpretation as corresponding image distributions. 
}
\details{
\tabular{ll}{
Package: \tab distr\cr
Version: \tab 1.7\cr
Date: \tab 2006-05-11\cr
Depends: \tab R(>= 2.2.0), methods, graphics\cr
Imports: \tab stats\cr
SaveImage: \tab no\cr
LazyLoad: \tab yes\cr
License: \tab GPL (version 2 or later)\cr
URL: \tab http://www.uni-bayreuth.de/departments/math/org/mathe7/DISTR/\cr
Packaged: \tab Thu May 11 10:16:03 2006; Peter\cr
Built: \tab R 2.4.0dev; i386-pc-mingw32; 2006-05-11 10:20:11; windows\cr
}}


\section{Classes}{
Distribution classes have a slot \code{param} the class of which 
is is specialized for the particualar distributions. The parameter 
classes for the particular distributions have slots with names 
according to the corresponding \var{[rdpq]<name>} functions of
package \pkg{base}. DiscreteDistribution and descendants have 
an additional slot \code{support}. For all particular distributions, 
generating functions are provided, e.g. \code{X <- Norm(mean = 3, sd = 2)}. 
All slots should be inspected / modified by means of corresponding 
accessor- /replacement functions; e.g. \code{mean(X) <- 3}

(see Parameter classes).

\preformatted{

Distribution classes
slots: [<name>(<class>)] 
img(rSpace), param(OptionalParameter),
r(function), d(OptionalFunction), p(OptionalFunction), q(OptionalFunction),
.withSim(logical), .withArith(logical)

"Distribution"
|>"UnivariateDistribution"
|>|>"AbscontDistribution"
|>|>|>"Beta"
|>|>|>"Cauchy"
|>|>|>"ExpOrGammaOrChisq" (VIRTUAL)
|>|>|>|>"Exp"
|>|>|>|>"Gammad"
|>|>|>|>"Chisq"
|>|>|>"Fd"
|>|>|>"Lnorm"
|>|>|>"Logis"
|>|>|>"Norm"
|>|>|>"Td"
|>|>|>"Unif"
|>|>|>"Weibull"
|>|>|"DiscreteDistribution"
|>|>|>"Binom"
|>|>|>"Dirac"
|>|>|>"Hyper"
|>|>|>"NBinom"
|>|>|>|>"Geom"
|>|>|>"Pois"

Parameter classes

"OptionalParameter"
|>"Parameter"
|>|>"BetaParameter"
|>|>"BinomParameter"
|>|>"CauchyParameter"
|>|>"ChisqParameter"
|>|>"DiracParameter"
|>|>"ExpParameter"
|>|>"FParameter"
|>|>"GammaParameter"
|>|>"GeomParameter"
|>|>"HyperParameter"
|>|>"LnormParameter"
|>|>"LogisParameter"
|>|>"NbinomParameter"
|>|>"NormParameter"
|>|>"UniNormParameter"
|>|>|>"PoisParameter"
|>|>"TParameter"
|>|>"UnifParameter"
|>|>"WeibullParameter"

Space classes

"rSpace"
|>"EuclideanSpace"
|>|>"Reals"
|>"Naturals"

}
}
\section{Methods}{

The group \code{Math} of unary (see \link[base]{Math}) as well as 
convolution are made available for distributions, see \link{operators-methods}.
Besides, there are \code{plot} and \code{print}-methods for distributions.
For the space classes, we have \code{liesIn}. The "history" of distributions
obtained by chaining operations may be shortened using \code{simplifyr}.

}
\section{Functions}{
\preformatted{
RtoDPQ                  Default procedure to fill slots d,p,q given r 
                        for a.c. distributions
RtoDPQ.d                Default procedure to fill slots d,p,q given r 
                        for discrete distributions
distroptions            Functions to change the global variables of the
                        package 'distr'
standardMethods         Utility to automatically generate accessor and 
                        replacement functions
}}

\note{Arithmetics on distribution objects are understood as operations on corresponding 
(independent) r.v.'s and \strong{not} on distribution functions or densities.\cr
See also \code{infoShow("distr","ARITHMETICS")}.

Accuracy of these arithmetics is controlled by global options which may be inspected / set by \code{distroptions()} 
and \code{getdistrOption()}, confer \link{distroptions} .
}
\section{Acknowledgement}{
We thank Martin Maechler, Josef Leydold, John Chambers,
Duncan Murdoch, Gregory Warnes, Paul Gilbert, Kurt Hornik,
Uwe Ligges, Torsten Hothorn, and Seth Falcon for their help
in preparing this package.}

\author{
Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr 
Thomas Stabla \email{statho3@web.de},\cr 
Florian Camphausen \email{fcampi@gmx.de},\cr
Matthias Kohl \email{Matthias.Kohl@stamats.de}\cr

\emph{Maintainer:} Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de}
}

\references{
a more detailed manual for \pkg{distr}, \pkg{distrSim}, \pkg{distrTEst}, and \pkg{distrEx} may be downloaded from  
\url{http://www.uni-bayreuth.de/departments/math/org/mathe7/DISTR/distr.pdf}

a homepage to this package is available under\cr
\url{http://www.uni-bayreuth.de/departments/math/org/mathe7/DISTR/}
}

\section{Start-up-Banner}{
You may suppress the start-up banner/message completely by setting \code{options("StartupBanner"="off")}
somewhere before loading this package by \code{library} or \code{require} in your R-code / R-session.

If option \code{"StartupBanner"} is not defined (default) or setting    
\code{options("StartupBanner"=NULL)} or  \code{options("StartupBanner"="complete")} 
the complete start-up banner is displayed.

For any other value of option \code{"StartupBanner"} (i.e., not in \code{c(NULL,"off","complete")})
only the version information is displayed.

The same can be achieved by wrapping the \code{library} or \code{require}  call into
either \code{suppressStartupMessages()} or \code{onlytypeStartupMessages(.,atypes="version")}. }

\section{Demos}{
Demos are available --- see \code{demo(package="distr")}}
\keyword{package}
\keyword{distribution}
\examples{
X <- Unif(2,3)
Y <- Pois(lambda = 3)
Z <- X+Y  # generates Law of corresponding independent variables
p(Z)(0.2)
r(Z)(1000)
plot(Z+sin(Norm()))}
