\name{operators-methods}
\docType{methods}
\alias{operators-methods}
\alias{-,UnivariateDistribution}
\alias{*,UnivariateDistribution,numeric-method}
\alias{/,UnivariateDistribution,numeric-method}
\alias{+,UnivariateDistribution,numeric-method}
\alias{-,UnivariateDistribution,numeric-method}
\alias{*,numeric-method,UnivariateDistribution}
\alias{+,numeric-method,UnivariateDistribution}
\alias{-,numeric-method,UnivariateDistribution}
\alias{+,UnivariateDistribution,DiscreteDistribution-method}
\alias{-,UnivariateDistribution,DiscreteDistribution-method}
\alias{-,AbscontDistribution}
\alias{*,AbscontDistribution,numeric-method}
\alias{/,AbscontDistribution,numeric-method}
\alias{+,AbscontDistribution,numeric-method}
\alias{-,AbscontDistribution,numeric-method}
\alias{*,numeric-method,AbscontDistribution}
\alias{+,numeric-method,AbscontDistribution}
\alias{-,numeric-method,AbscontDistribution}
\alias{+,AbscontDistribution,DiscreteDistribution-method}
\alias{-,AbscontDistribution,DiscreteDistribution-method}
\alias{-,DiscreteDistribution}
\alias{*,DiscreteDistribution,numeric-method}
\alias{/,DiscreteDistribution,numeric-method}
\alias{+,DiscreteDistribution,numeric-method}
\alias{-,DiscreteDistribution,numeric-method}
\alias{*,numeric-method,DiscreteDistribution}
\alias{+,numeric-method,DiscreteDistribution}
\alias{-,numeric-method,DiscreteDistribution}
\alias{+,DiscreteDistribution,DiscreteDistribution-method}
\alias{-,DiscreteDistribution,DiscreteDistribution-method}
\alias{-,Norm,Norm-method}
\alias{+,Norm,Norm-method}
\alias{+,numeric,Norm-method}
\alias{*,numeric,Norm-method}
\alias{-,numeric,Norm-method}
\alias{+,Norm,numeric-method}
\alias{*,Norm,numeric-method}
\alias{-,Norm,numeric-method}
\alias{/,Norm,numeric-method}
\alias{+,Binom,Binom-method}
\alias{+,Pois,Pois-method}
\title{ Methods for operators +,-,*,/ in Package `distr' }
\description{operator-methods}
\section{Methods}{\describe{

    \item{-}{\code{signature(e1 = "UnivariateDistribution")}:

      application of `-' to this univariate distribution}
    \item{*}{\code{signature(e1 = "UnivariateDistribution", e2 =
	"numeric")}:

      multiplication of this univariate 
    distribution by an object of class `numeric'}
  \item{/}{\code{signature(e1 = "UnivariateDistribution", e2 =
      "numeric")}:

    division of this univariate distribution
    by an object of class `numeric'}
  \item{+}{\code{signature(e1 = "UnivariateDistribution", e2 =
      "numeric")}:

    addition of this univariate distribution
    to an object of class `numeric'}
  \item{-}{\code{signature(e1 = "UnivariateDistribution", e2 =
      "numeric")}:

    subtraction of an object of class `numeric'
    from this univariate distribution }
  \item{*}{\code{signature(e1 = "numeric", e2 =
      "UnivariateDistribution")}:

    multiplication of this univariate
    distribution by an object of class `numeric'}
  \item{+}{\code{signature(e1 = "numeric", e2 =
      "UnivariateDistribution")}:

    addition of this univariate distribution to
    an object of class `numeric'}
  \item{-}{\code{signature(e1 = "numeric", e2 =
      "UnivariateDistribution")}:

    subtraction of this univariate distribution
    from an object of class `numeric'}
  \item{+}{\code{signature(e1 = "UnivariateDistribution", e2 =
      "UnivariateDistribution")}:

    Convolution of two univariate
    distributions. The slots p, d and q are approximated by grids.}
  \item{-}{\code{signature(e1 = "UnivariateDistribution", e2 =
      "UnivariateDistribution")}:

    Convolution of two univariate
    distributions. The slots p, d and q are approximated by grids.}
  \item{-}{\code{signature(e1 = "AbscontDistribution")}:

    application of `-' to this absolutely continuous distribution}
  \item{*}{\code{signature(e1 = "AbscontDistribution", e2 = "numeric")}:

    multiplication of this absolutely continuous 
    distribution by an object of class `numeric'}
  \item{/}{\code{signature(e1 = "AbscontDistribution", e2 = "numeric")}:

    division of this absolutely continuous 
    distribution by an object of class `numeric'}
  \item{+}{\code{signature(e1 = "AbscontDistribution", e2 = "numeric")}:

    addition of this absolutely continuous 
    distribution to an object of class `numeric'}
  \item{-}{\code{signature(e1 = "AbscontDistribution", e2 = "numeric")}:

    subtraction of an object of class `numeric' from
    this absolutely continuous distribution }
  \item{*}{\code{signature(e1 = "numeric", e2 = "AbscontDistribution")}:

    multiplication of this absolutely continuous 
    distribution by an object of class `numeric'}
  \item{+}{\code{signature(e1 = "numeric", e2 = "AbscontDistribution")}:

    addition of this absolutely continuous 
    distribution to an object of class `numeric'}
  \item{-}{\code{signature(e1 = "numeric", e2 = "AbscontDistribution")}:

    subtraction of this absolutely continuous 
    distribution from an object of class `numeric'}
  \item{+}{\code{signature(e1 = "AbscontDistribution", e2 =
      "AbscontDistribution")}:

    Convolution of two absolutely
    continuous distributions. The slots p, d and q are approximated by grids.}
  \item{-}{\code{signature(e1 = "AbscontDistribution", e2 =
      "AbscontDistribution")}:

    Convolution of two absolutely
    continuous distributions. The slots p, d and q are approximated by grids.}
  \item{-}{\code{signature(e1 = "DiscreteDistribution")}:

    application of `-' to this discrete distribution}
  \item{*}{\code{signature(e1 = "DiscreteDistribution", e2 =
      "numeric")}:

    multiplication of this discrete distribution
    by an object of class `numeric'}
  \item{/}{\code{signature(e1 = "DiscreteDistribution", e2 =
      "numeric")}:

    division of this discrete distribution
    by an object of class `numeric'}
  \item{+}{\code{signature(e1 = "DiscreteDistribution", e2 =
      "numeric")}:

    addition of this discrete distribution
    to an object of class `numeric'}
  \item{-}{\code{signature(e1 = "DiscreteDistribution", e2 =
      "numeric")}:

    subtraction of an object of class `numeric'
    from this discrete distribution }
  \item{*}{\code{signature(e1 = "numeric", e2 =
      "DiscreteDistribution")}:

    multiplication of this discrete distribution
    by an object of class `numeric'}
  \item{+}{\code{signature(e1 = "numeric", e2 =
      "DiscreteDistribution")}:

    addition of this discrete distribution
    to an object of class `numeric'}
  \item{-}{\code{signature(e1 = "numeric", e2 =
      "DiscreteDistribution")}:

    subtraction of this discrete distribution
    from an object of class `numeric'}
  \item{+}{\code{signature(e1 = "DiscreteDistribution", e2 =
      "DiscreteDistribution")}:

    Convolution of two discrete
    distributions. The slots p, d and q are approximated by grids.}
  \item{-}{\code{signature(e1 = "DiscreteDistribution", e2 =
      "DiscreteDistribution")}:

    Convolution of two discrete
    distributions. The slots p, d and q are approximated by grids.}
  \item{*}{\code{signature(e1 = "numeric", e2 = "Norm")}:

    multiplication of this normal distribution by an object of
    class `numeric'}
  \item{+}{\code{signature(e1 = "numeric", e2 = "Norm")}:

    addition of this normal distribution to an object of class
    `numeric'}
  \item{-}{\code{signature(e1 = "numeric", e2 = "Norm")}:

    subtraction of this normal distribution from an object of
    class `numeric'}
  \item{*}{\code{signature(e1 = "Norm", e2 = "numeric")}:

    multiplication of this normal distribution by an object of
    class `numeric'}
  \item{+}{\code{signature(e1 = "Norm", e2 = "numeric")}:

    addition of this normal distribution to an object of class
    `numeric'}
  \item{-}{\code{signature(e1 = "Norm", e2 = "numeric")}:

    subtraction of an object of class `numeric' from this normal
    distribution }
  \item{/}{\code{signature(e1 = "Norm", e2 = "numeric")}:

    division of this normal distribution by an object of class
    `numeric'}
    \item{-}{\code{signature(e1 = "Norm", e2 = "Norm")}}
    \item{+}{\code{signature(e1 = "Norm", e2 = "Norm")}: 

      For the normal distribution the exact convolution formulas are implemented thereby improving the general numerical
    approximation.}
  \item{+}{\code{signature(e1 = "Binom", e2 = "Binom")}:

    For two binomial distributions with the same probabilities
    the exact convolution formula is implemented thereby improving the general numerical approximation.}
  \item{+}{\code{signature(e1 = "Pois", e2 = "Pois")}:

    For the Poisson distribution the exact convolution formula is
    implemented thereby improving the general numerical approximation.}
}}

\seealso{
\code{\link{UnivariateDistribution-class}}
\code{\link{AbscontDistribution-class}} \cr
\code{\link{DiscreteDistribution-class}}
\code{\link{Norm-class}}
\code{\link{Binom-class}}
\code{\link{Pois-class}}
}


\keyword{}
