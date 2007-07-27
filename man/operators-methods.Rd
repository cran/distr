\name{operators-methods}
\docType{methods}

\alias{operators-methods}
\alias{operators}
\alias{-,UnivariateDistribution,missing-method}
\alias{-,Norm,missing-method}
\alias{+,UnivariateDistribution,numeric-method}
\alias{+,AbscontDistribution,numeric-method}
\alias{+,DiscreteDistribution,numeric-method}
\alias{+,LatticeDistribution,numeric-method}
\alias{+,AffLinAbscontDistribution,numeric-method}
\alias{+,AffLinDiscreteDistribution,numeric-method}
\alias{+,AffLinLatticeDistribution,numeric-method}
\alias{+,Cauchy,numeric-method}
\alias{+,Dirac,numeric-method}
\alias{+,Norm,numeric-method}
\alias{+,Unif,numeric-method}
\alias{+,numeric,UnivariateDistribution-method}
%%\alias{-,UnivariateDistribution,ANY-method}
\alias{-,UnivariateDistribution,numeric-method}
\alias{-,UnivariateDistribution,UnivariateDistribution-method}
\alias{-,numeric,UnivariateDistribution-method}
\alias{*,UnivariateDistribution,numeric-method}
\alias{*,DiscreteDistribution,numeric-method}
\alias{*,LatticeDistribution,numeric-method}
\alias{*,AffLinAbscontDistribution,numeric-method}
\alias{*,AffLinDiscreteDistribution,numeric-method}
\alias{*,AffLinLatticeDistribution,numeric-method}
\alias{*,AbscontDistribution,numeric-method}
\alias{*,DExp,numeric-method}
\alias{*,Exp,numeric-method}
\alias{*,ExpOrGammaOrChisq,numeric-method}
\alias{*,Weibull,numeric-method}
\alias{*,Cauchy,numeric-method}
\alias{*,Dirac,numeric-method}
\alias{*,Norm,numeric-method}
\alias{*,Logis,numeric-method}
\alias{*,Lnorm,numeric-method}
\alias{*,Unif,numeric-method}
\alias{*,numeric,UnivariateDistribution-method}
\alias{/,UnivariateDistribution,numeric-method}
\alias{+,UnivariateDistribution,UnivariateDistribution-method}
\alias{+,AbscontDistribution,AbscontDistribution-method}
\alias{+,AbscontDistribution,DiscreteDistribution-method}
\alias{+,DiscreteDistribution,AbscontDistribution-method}
\alias{+,DiscreteDistribution,DiscreteDistribution-method}
\alias{+,LatticeDistribution,DiscreteDistribution-method}
\alias{+,LatticeDistribution,LatticeDistribution-method}
\alias{+,Binom,Binom-method}
\alias{+,Cauchy,Cauchy-method}
\alias{+,Chisq,Chisq-method}
\alias{+,Dirac,Dirac-method}
\alias{+,ExpOrGammaOrChisq,ExpOrGammaOrChisq-method}
\alias{+,Pois,Pois-method}
\alias{+,Nbinom,Nbinom-method}
\alias{+,Norm,Norm-method}
\alias{+,Logis,numeric-method}
\alias{+,Dirac,UnivariateDistribution-method}
\alias{+,UnivariateDistribution,Dirac-method}
\alias{-,Dirac,Dirac-method}
\alias{*,Dirac,Dirac-method}
\alias{*,Dirac,UnivariateDistribution-method}
\alias{*,UnivariateDistribution,Dirac-method}
\alias{/,Dirac,Dirac-method}
\alias{/,numeric,Dirac-method}

\title{Methods for operators +,-,*,/ in Package distr}

\description{Arithmetics and unary mathematical transformations for distributions}

\details{Arithmetics as well as all functions from group \code{Math}, see \code{\link[methods]{Math}}
are provided for distributions; wherever possible exact expressions are used; else 
random variables are generated according to this transformation and subsequently the remaining
slots filled by \code{\link{RtoDPQ}}, \code{\link{RtoDPQ.d}}}

\section{Methods}{\describe{
\item{\code{-}}{\code{signature(e1 = "UnivariateDistribution", e2 = "missing")} unary operator; result again of class \code{"UnivariateDistribution"}; exact}
\item{\code{-}}{\code{signature(e1 = "Norm", e2 = "missing")} unary operator; result again of \code{"Norm"}; exact}
\item{\code{+}}{\code{signature(e1 = "UnivariateDistribution", e2 = "numeric")} result again of class \code{"UnivariateDistribution"}; exact}
\item{\code{+}}{\code{signature(e1 = "AbscontDistribution", e2 = "numeric")} result of 
 class \code{"AffLinAbscontDistribution"}; exact}
\item{\code{+}}{\code{signature(e1 = "DiscreteDistribution", e2 = "numeric")} result of 
 class \code{"AffLinDiscreteDistribution"}; exact}
\item{\code{+}}{\code{signature(e1 = "LatticeDistribution", e2 = "numeric")} result  of 
 class \code{"AffLinLatticeDistribution"}; exact}
\item{\code{+}}{\code{signature(e1 = "AffLinAbscontDistribution", e2 = "numeric")} result again of 
 class \code{"AffLinAbscontDistribution"}; exact}
\item{\code{+}}{\code{signature(e1 = "AffLinDiscreteDistribution", e2 = "numeric")} result again of 
 class \code{"AffLinDiscreteDistribution"}; exact}
\item{\code{+}}{\code{signature(e1 = "AffLinLatticeDistribution", e2 = "numeric")} result again of 
 class \code{"AffLinLatticeDistribution"}; exact}
\item{\code{+}}{\code{signature(e1 = "Cauchy", e2 = "numeric")} result again of class \code{"Cauchy"}; exact}
\item{\code{+}}{\code{signature(e1 = "Dirac", e2 = "numeric")} result again of class \code{"Dirac"}; exact}
\item{\code{+}}{\code{signature(e1 = "Norm", e2 = "numeric")} result again of class \code{"Norm"}; exact}
\item{\code{+}}{\code{signature(e1 = "Unif", e2 = "numeric")} result again of class \code{"Unif"}; exact}
\item{\code{+}}{\code{signature(e1 = "Logis", e2 = "numeric")}  result again of class \code{"Logis"}; exact}
\item{\code{+}}{\code{signature(e1 = "numeric", e2 = "UnivariateDistribution")} is translated to  
         \code{signature(e1 = "UnivariateDistribution", e2 = "numeric")}; exact}
\item{\code{-}}{\code{signature(e1 = "UnivariateDistribution", e2= "ANY")};exact}
\item{\code{-}}{\code{signature(e1 = "UnivariateDistribution", e2 = "numeric")} is translated to 
 \code{e1 + (-e2)}; exact}
\item{\code{-}}{\code{signature(e1 = "numeric", e2 = "UnivariateDistribution")} is translated to \code{(-e1) + e2}; exact}
\item{\code{*}}{\code{signature(e1 = "UnivariateDistribution", e2 = "numeric")} result again of class \code{"UnivariateDistribution"}; exact}
\item{\code{*}}{\code{signature(e1 = "AbscontDistribution", e2 = "numeric")} result of 
 class \code{"AffLinAbscontDistribution"}; exact}
\item{\code{*}}{\code{signature(e1 = "DiscreteDistribution", e2 = "numeric")} result of 
 class \code{"AffLinDiscreteDistribution"}; exact}
\item{\code{*}}{\code{signature(e1 = "LatticeDistribution", e2 = "numeric")} result  of 
 class \code{"AffLinLatticeDistribution"}; exact}
\item{\code{*}}{\code{signature(e1 = "AffLinAbscontDistribution", e2 = "numeric")} result again of 
 class \code{"AffLinAbscontDistribution"}; exact}
\item{\code{*}}{\code{signature(e1 = "AffLinDiscreteDistribution", e2 = "numeric")} result again of 
 class \code{"AffLinDiscreteDistribution"}; exact}
\item{\code{*}}{\code{signature(e1 = "AffLinLatticeDistribution", e2 = "numeric")} result again of 
 class \code{"AffLinLatticeDistribution"}; exact}
\item{\code{*}}{\code{signature(e1 = "DExp", e2 = "numeric")} if \code{abs(e2)>0} result again of class \code{"DExp"}; exact}
\item{\code{*}}{\code{signature(e1 = "Exp", e2 = "numeric")} if \code{e2>0} result again of class \code{"Exp"}; exact}
\item{\code{*}}{\code{signature(e1 = "ExpOrGammaOrChisq", e2 = "numeric")} if \code{e1} is a Gamma distribution and \code{e2>0} 
                                                                    result of class \code{"Gammad"}; exact}
\item{\code{*}}{\code{signature(e1 = "Weibull", e2 = "numeric")} if \code{e2>0} 
                                                                    result of class \code{"Weibull"}; exact}
\item{\code{*}}{\code{signature(e1 = "Cauchy", e2 = "numeric")}  if \code{abs(e2)>0} result again of class \code{"Cauchy"}; exact}
\item{\code{*}}{\code{signature(e1 = "Dirac", e2 = "numeric")}  result again of class \code{"Dirac"}; exact}
\item{\code{*}}{\code{signature(e1 = "Norm", e2 = "numeric")}  if \code{abs(e2)>0} result again of class \code{"Norm"}; exact}
\item{\code{*}}{\code{signature(e1 = "Unif", e2 = "numeric")}  if \code{abs(e2)>0} result again of class \code{"Unif"}; exact}
\item{\code{*}}{\code{signature(e1 = "Logis", e2 = "numeric")}  if \code{e2>0} result again of class \code{"Logis"}; exact}
\item{\code{*}}{\code{signature(e1 = "Lnorm", e2 = "numeric")}  if \code{e2>0} result again of class \code{"Lnorm"}; exact}
\item{\code{*}}{\code{signature(e1 = "numeric", e2 = "UnivariateDistribution")} is translated to 
         \code{signature(e1 = "UnivariateDistribution", e2 = "numeric")}; exact}
\item{\code{/}}{\code{signature(e1 = "UnivariateDistribution", e2 = "numeric")} is translated to \code{e1 * (1/e2)}; exact}
\item{\code{+}}{\code{signature(e1 = "UnivariateDistribution", e2 = "UnivariateDistribution")}  result again of class 
         \code{"UnivariateDistribution"}; is generated by simulations}
\item{\code{-}}{\code{signature(e1 = "UnivariateDistribution", e2 = "UnivariateDistribution")} is translated to \code{(-e1) + (-e2)}; 
          result again of class   \code{"UnivariateDistribution"}; is generated by simulations}
\item{\code{+}}{\code{signature(e1 = "AbscontDistribution", e2 = "AbscontDistribution")} assumes \code{e1}, \code{e2} independent; result again of class 
         \code{"AbscontDistribution"}; is generated by FFT}
\item{\code{+}}{\code{signature(e1 = "AbscontDistribution", e2 = "DiscreteDistribution")} assumes \code{e1}, \code{e2} independent; result again of class 
         \code{"AbscontDistribution"}; is generated by FFT}
\item{\code{+}}{\code{signature(e1 = "DiscreteDistribution", e2 = "AbscontDistribution")} assumes \code{e1}, \code{e2} independent; result again of class 
         \code{"AbscontDistribution"}; is generated by FFT}
\item{\code{+}}{\code{signature(e1 = "LatticeDistribution", e2 = "LatticeDistribution")} assumes \code{e1}, \code{e2} independent; 
if the larger lattice-width is an integer multiple of the smaller(in abs. value) one: result again of class 
         \code{"LatticeDistribution"}; is generated by D/FFT}
\item{\code{+}}{\code{signature(e1 = "DiscreteDistribution", e2 = "DiscreteDistribution")} assumes \code{e1}, \code{e2} independent; result again of class 
         \code{"DiscreteDistribution"}; is generated by explicite convolution}
\item{\code{+}}{\code{signature(e1 = "LatticeDistribution", e2 = "DiscreteDistribution")} assumes \code{e1}, \code{e2} independent; result again of class 
         \code{"DiscreteDistribution"}; is generated by explicite convolution}
\item{\code{+}}{\code{signature(e1 = "Binom", e2 = "Binom")} assumes \code{e1}, \code{e2} independent; 
if \code{prob(e1)==prob(e2)}, result again of class 
         \code{"Binom"}; uses the convolution formula for binomial distributions; exact}
\item{\code{+}}{\code{signature(e1 = "Cauchy", e2 = "Cauchy")}  assumes \code{e1}, \code{e2} independent; result again of class 
         \code{"Cauchy"}; uses the convolution formula for Cauchy distributions; exact}
\item{\code{+}}{\code{signature(e1 = "Chisq", e2 = "Chisq")}  assumes \code{e1}, \code{e2} independent; result again of class 
         \code{"Chisq"}; uses the convolution formula for Chisq distributions; exact}
\item{\code{+}}{\code{signature(e1 = "Dirac", e2 = "Dirac")} result again of class \code{"Dirac"}; exact}
\item{\code{+}}{\code{signature(e1 = "ExpOrGammaOrChisq", e2 = "ExpOrGammaOrChisq")}  assumes \code{e1}, \code{e2} independent; if 
         \code{e1}, \code{e2} are Gamma distributions, result is of class 
         \code{"Gammad"}; uses the convolution formula for Gamma distributions; exact}
\item{\code{+}}{\code{signature(e1 = "Pois", e2 = "Pois")}  assumes \code{e1}, \code{e2} independent; result again of class 
         \code{"Pois"}; uses the convolution formula for Poisson distributions; exact}
\item{\code{+}}{\code{signature(e1 = "Nbinom", e2 = "Nbinom")}  assumes \code{e1}, \code{e2} independent; if 
\code{prob(e1)==prob(e2)}, result again of class 
         \code{"Nbinom"}; uses the convolution formula for negative binomial distributions; exact}
\item{\code{+}}{\code{signature(e1 = "Norm", e2 = "Norm")}  assumes \code{e1}, \code{e2} independent; result again of class 
         \code{"Norm"}; uses the convolution formula for normal distributions; exact}
\item{\code{+}}{\code{signature(e1 = "UnivariateDistribution", e2 = "Dirac")}  translated to \code{e1 + location(e2)}; 
         result again of class \code{"Dirac"}; exact}
\item{\code{+}}{\code{signature(e1 = "Dirac", e2 = "UnivariateDistribution")}  translated to \code{e2 + location(e1)}; 
         result again of class \code{"Dirac"}; exact}
\item{\code{-}}{\code{signature(e1 = "Dirac", e2 = "Dirac")}  result again of class \code{"Dirac"}; exact}
\item{\code{*}}{\code{signature(e1 = "Dirac", e2 = "Dirac")}  result again of class \code{"Dirac"}; exact}
\item{\code{*}}{\code{signature(e1 = "UnivariateDistribution", e2 = "Dirac")}  translated to \code{e1 * location(e2)}; 
         result again of class \code{"Dirac"}; exact}
\item{\code{*}}{\code{signature(e1 = "Dirac", e2 = "UnivariateDistribution")}  translated to \code{e2 * location(e1)}; 
         result again of class \code{"Dirac"}; exact}
\item{\code{/}}{\code{signature(e1 = "Dirac", e2 = "Dirac")}  result again of class \code{"Dirac"}; exact}
\item{\code{/}}{\code{signature(e1 = "numeric", e2 = "Dirac")}  result again of class \code{"Dirac"}; exact}
}}


\seealso{
\code{\link{UnivariateDistribution-class}}
\code{\link{AbscontDistribution-class}} \cr
\code{\link{DiscreteDistribution-class}}
\code{\link{LatticeDistribution-class}}\cr
\code{\link{Norm-class}}
\code{\link{Binom-class}}
\code{\link{Pois-class}}
\code{\link{Dirac-class}}\cr
\code{\link{Cauchy-class}}
\code{\link{Gammad-class}}
\code{\link{Logis-class}}
\code{\link{Lnorm-class}}\cr
\code{\link{Exp-class}}
\code{\link{Weibull-class}}
\code{\link{Nbinom-class}}
}

\arguments{
  \item{e1,e2}{ objects of class \code{"UnivariateDistribution"} (or subclasses) or \code{"numeric"}}
}
\examples{
N <- Norm(0,3)
P <- Pois(4)
a <- 3
N + a
N + P
N - a
a * N
a * P
N / a + sin( a * P - N)
}
\keyword{math}
\keyword{distribution}
\keyword{arith}
\concept{convolution}
\concept{FFT}
\concept{affine linear}
\concept{image distribution}
