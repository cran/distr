\name{internals_for_distr}
\alias{internals_for_distr}
\alias{.is.vector.lattice}
\alias{.is.consistent}
\alias{.make.lattice.es.vector}
\alias{.presubs}
\alias{.inArgs}
\alias{.isEqual}
\alias{.isIn}
\alias{.setEqual}
\alias{.isEqual01}
\alias{.makeD}
\alias{.makeP}
\alias{.makeQ}
\alias{.makeDNew}
\alias{.makePNew}
\alias{.makeQNew}
\alias{.makeDd}
\alias{.makePd}
\alias{.makeQd}
\alias{.makeQc}
\alias{.fM2}
\alias{.fM}
\alias{.fm}
\alias{.notwithLArg}
\alias{.multm}
\alias{.plusm}
\alias{.getObjName}
\alias{.discretizeP}

\title{Internal functions of package distr}

\description{
These functions are used internally by package distr.}

\usage{
.is.vector.lattice(x)
.is.consistent(lattice, support, eq.space = TRUE)
.make.lattice.es.vector(x)
.inArgs(arg, fct)
.isEqual(p0, p1, tol = min( getdistrOption("TruncQuantile")/2,
                                          .Machine$double.eps^.7))
.isEqual01(x)
.isIn(p0, pmat, tol = min( getdistrOption("TruncQuantile")/2,
                                          .Machine$double.eps^.7
                                          ))
.setEqual(x, y, tol = 1e-7)
.presubs(inp, frompat, topat)
.makeD(object, argList,  stand = NULL)
.makeP(object, argList,  sign = TRUE, correct = NULL)
.makeQ(object, lastCall, sign = TRUE, Cont = TRUE)
.plusm(e1, e2, Dclass = "DiscreteDistribution")
.multm(e1, e2, Dclass = "DiscreteDistribution")
.notwithLArg(D)
.getObjName(i = 1)
.discretizeP(D, lower, upper, h)   
.fm(x,f)
.fM(x,f)
.fM2(x,f)
.makeDd(x,y, yleft, yright)
.makePd(x,y, yleft, yright)
.makeQd(x,y, yleft, yright)
.makeQc(x,y, yleft, yright)
.makeDNew(x, dx, h = NULL, Cont = TRUE, standM = "sum")
.makePNew(x, dx, h = NULL, notwithLLarg = FALSE,
                      Cont = TRUE, myPf = NULL, pxl = NULL, pxu = NULL)
.makeQNew(x, px.l, px.u, notwithLLarg = FALSE, yL , yR, Cont = TRUE)
}
\arguments{
  \item{x}{a (numeric) vector}
  \item{y}{a (numeric) vector}
  \item{f}{a function in one (numeric) argument}
  \item{lattice}{a lattice (of class \code{Lattice})}
  \item{support}{a support vector}
  \item{eq.space}{logical: shall we check for the support to be equally spaced?} 
  \item{arg}{a formal argument as character}
  \item{fct}{a function}
  \item{p0,p1}{(numeric) vectors} 
  \item{pmat}{(matrix) a matrix with two columns where row-wise the left column
              is smaller than the right one} 
  \item{tol}{an error tolerance (numeric)}
  \item{e1}{a distribution object}
  \item{e2}{a numeric}
  \item{object}{a distribution object}
  \item{argList}{an (unevaluated) list of arguments passed to \code{m(object)}  
        where  \code{m} is in \code{d,p,q}}
  \item{stand}{factor for a (Lebesgue) density to integrate to 1}
  \item{sign}{the sign of the second operand --- for multiplication at 
               the moment}
  \item{correct}{unevaluated R-code to correct for right-continuity 
                  (for multiplication with negative numerics at the moment)}
  \item{lastCall}{unevaluated R-Code ---gives how the result of a call to 
                   \code{q(e1)} is further transformed}
  \item{Cont}{logical: \code{TRUE} if \code{object} is continuous}
  \item{DClass}{character: name of distribution class}
  \item{D}{a distribution object}
  \item{i}{an integer}
  \item{yleft, yright}{extrapolation value beyond left/right endpoint of grid}
  \item{h}{numeric: grid width}
  \item{standM}{standardization method --- summation or integration}
  \item{notwithLLarg}{logical --- can we use \code{log.p}, \code{lower.tail} 
  arguments for \code{p,q}-methods of first operand?}
  \item{dx}{numeric: vector of cell-probabilities for the (discretized) 
  distribution}
  \item{myPf}{function with args \code{x,y, yleft, yright} 
  (as \code{approxfun}): 
              if given: replaces \code{approxfun} as interpolation method for 
              continuos distributions}
  \item{pxl,pxu}{numeric: if given vector of (lower/upper) cumulative 
  probabilities}
  \item{yL, yR}{argmin / argmax of p()-method}
  \item{inp}{either a language object or a character vector}
  \item{frompat}{vector of character strings containing regular expressions (or
          character string for \code{fixed = TRUE}) to be matched in the
          given character vector.  Coerced by \code{as.character} to a
          character string if possible; (as argument \code{pattern} in 
          \code{\link{gsub}} --- but possibly of length >1).}
  \item{topat}{ a (vector of) replacement(s) for matched pattern in 
          \code{.presubs}. 
          Coerced to character if possible.  For \code{fixed = FALSE} this
          can include backreferences \code{"\\1"'} to \code{"\\9"} to 
          parenthesized subexpressions of \code{pattern}.  For 
          \code{perl = TRUE} only, it can
          also contain \code{"\\U"} or \code{"\\L"} to convert the rest of the
          replacement to upper or lower case; (as argument \code{replacement} 
          in \code{\link{gsub}}--- but possibly of length >1).}
}

\details{
\code{.is.vector.lattice} checks whether a given vector \code{x} is equally 
spaced.
\code{.is.consistent} checks whether a given support vector \code{support} is 
consistent to a given lattice \code{lattice} --- with or without checking
if \code{support} is equally spaced.  \code{.make.lattice.es.vector} 
makes an object of class \code{Lattice} out of a given (equally spaced) vector 
\code{x}.

\code{.inArgs} checks whether an argument \code{arg} is a formal argument of 
     \code{fct} --- not vectorized.

\code{.isEqual} checks whether \code{p0} and \code{p1} are equal to given
      tolerance.
\code{.isIn} checks whether \code{p0} lies in any of the intervals given by
     matrix \code{pmat} to given tolerance.
\code{.isEqual01}(x) checks whether \code{x} is 0 or 1 to given tolerance. 
\code{.setEqual} sets all elements of x which are equal to some element of y 
         up to tolerance tol, to exactly the respective element of y. 
         
\code{.notwithLArg} checks whether object \code{D} was generated by simulations 
or if its slots \code{p,q} do not have \code{lower.tail} arguments.

\code{.getObjName} returns the name of the object in the \code{i}th operand.
\code{.discretizeP} discretizes \code{D} to a grid of probabilities from 
     \code{lower} to \code{upper} with width \code{h}.   

\code{.fm}, \code{.fM} return the smallest / biggest value in (0,1) such that
  \code{f}(x) is finite; \code{.fM2} is a variant of \code{.fM} using a 
  \code{lower.tail = FALSE} argument.

\code{.makeD}, \code{.makeP}, \code{.makeQ} generate slots \code{p,d,q} for
    binary operations \code{e1 /op/ e2} for a distribution object \code{e1} 
    and a numeric \code{e2} ---for the moment only \code{/op/}'s 
     \code{+,-,*,/} are implemented.

\code{.plusm}, \code{.multm} more specifically use \code{.makeD}, \code{.makeP}, 
\code{.makeQ} to generate slots \code{p,d,q} for \code{+}, \code{*}, 
      respectively. 


\code{.makeDd}, \code{.makePd}, \code{.makeQd} provide discrete analogues to 
\code{approxfun} for interpolation at non grid-values 

\code{.makeQc} is an analogue to \code{makeQd} for absolutely continuous 
distributions using \code{approxfun}. 

\code{.makeDNew} generates slot \code{d} for a new distribution object.
In case of a discrete distribution it produces a step function with 
\code{stepfun} (using \code{.makeDd}) and standardizes to 1 by summation.
In case of a continuous distribution it produces a density function with 
\code{approxfun} and standardizes to 1 by integration if the latter fails,
  it uses a trapezoid rule / summation for this purpose.

\code{.makePNew} generates slot \code{p} for a new distribution object.
In case of a discrete distribution it produces a step function from 
\code{cumsum} applied to \code{dx} ---or from \code{pxl} if this is given, with 
\code{stepfun} (using \code{.makePd}). 
In case of a continuous distribution it produces a cdf with 
\code{approxfun}. In case of \code{RtoDPQ}, \code{approxfun} is replaced by
\code{myPf} which calls \code{ecdf} directly.

\code{.makeQNew} generates slot \code{q} for a new distribution object.
In case of a discrete distribution it produces a step function 
(using \code{.makeQd}). Special care is taken for left continuity...
In case of a continuous distribution it produces a quantile function with 
\code{approxfun}. 
}


\value{
\item{.is.vector.lattice}{\code{logical} (length 1)}
\item{.is.consistent}{\code{logical} (length 1)}
\item{.notwithLArg}{\code{logical} (length 1)}
\item{.make.lattice.es.vector}{an object of class \code{Lattice}}
\item{.inArgs}{\code{logical} (length 1)}
\item{.isIn, .isEqual,.isEqual01}{vector of \code{logical}}
\item{.fm,.fM, .fM2}{a \code{numeric} of length 1}
\item{.plusm,.multm}{an object of class \code{DiscreteDistribution} or 
       \code{AbscontDistribution} according to argument \code{DClass}}
\item{.getObjName}{\code{character}}
\item{.discretizeP}{\code{numeric} --- the probabilities for the grid-values}
\item{.makeDd,.makePd, .makeQd}{a function with args 
       \code{x, y, yleft, yright}}
\item{.makeD,.makeDNew}{a function with args \code{x, log = FALSE}}
\item{.makeP,.makePNew}{a function with args \code{q, lower.tail = TRUE, 
       log.p = FALSE}}
\item{.makeQ,.makeQNew}{a function with args \code{p, lower.tail = TRUE, 
       log.p = FALSE}}
}


\author{
  Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de}
  }

\seealso{
\code{\link{LatticeDistribution}},
\code{\link{RtoDPQ}},
\code{\link{RtoDPQ.d}},
\code{\link{convpow}},
\code{\link{operators}},
\code{\link{plot-methods}}
}

\keyword{internal}
\concept{utilities}