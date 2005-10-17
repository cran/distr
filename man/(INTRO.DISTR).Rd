\name{(INTRO.DISTR)}
\alias{(INTRO.DISTR)}
\title{DISTR}
\center{S4 Classes for Distributions}
\synopsis{}

\section{Usage}{
   library("distr") 
}


\description{
\pkg{``distr''} is a package for R from version 1.8.1 onwards that is distributed
under GPL license 2.0. Its own current version is 1.6.

The aim of this package is to provide a conceptual treatment of random variables (r.v.'s) by means of S4 classes.
A mother class \code{Distribution} is introduced with slots for a parameter and ---most important---
for the four constitutive methods \code{r},  \code{d}, \code{p}, and \code{q} for simulation respectively for evaluation of
density / c.d.f.\ and quantile function of the corresponding distribution. All distributions of the \pkg{base} package for which corresponding
\code{r}-,  \code{d}-, \code{p}-, and \code{q}-functions exist (like normal, Poisson, etc.)
are implemented as subclasses of either \code{AbscontDistribution} or \code{DiscreteDistribution},
which themselves are again subclasses of \code{Distribution}. %\\

This approach seems very appealing to us from a conceptual viewpoint:

\emph{Just pass an object of some derived distribution class to a generic function as argument
and let the dispatching mechanism decide what to do on run-time.}

As an example, we may automatically generate new objects of these classes with corresponding
\code{r},  \code{d}, \code{p}, and \code{q}-slots for the laws of r.v.'s under standard mathematical
univariate transformations and under convolution of independent r.v.'s. For \code{Distribution} objects \code{X}
and \code{Y} expressions like
\code{3*X+sin(exp(-Y/4+3))} have their natural interpretation as corresponding image distributions. 

A somewhat longer and more detailed manual can be obtained under

\url{http://www.uni-bayreuth.de/departments/math/org/mathe7/DISTR/}

}
\keyword{}
