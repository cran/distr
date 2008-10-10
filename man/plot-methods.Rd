\name{plot-methods}
\docType{methods}
\alias{plot}
\alias{plot-methods}
\alias{plot,AffLinUnivarLebDecDistribution,missing-method}
\alias{plot,UnivarLebDecDistribution,missing-method}
\alias{plot,AbscontDistribution,missing-method}
\alias{plot,DiscreteDistribution,missing-method}
\alias{plot,DistrList,missing-method}
\title{ Methods for Function plot in Package `distr' }
\description{plot-methods}
\usage{plot(x, y, ...)
\S4method{plot}{AbscontDistribution,missing}(x, width = 10, height = 5.5,
     withSweave = getdistrOption("withSweave"), xlim = NULL, ylim = NULL, ngrid = 1000,
     verticals = TRUE, do.points = TRUE, main = FALSE, inner = TRUE,
     sub = FALSE, bmar = par("mar")[1], tmar = par("mar")[3], ...,
     cex.main = par("cex.main"), cex.inner = 1.2, cex.sub = par("cex.sub"), 
     col.points = par("col"), col.vert = par("col"), col.main = par("col.main"),  
     col.inner = par("col.main"), col.sub = par("col.sub"), cex.points = 2.0, 
     pch.u = 21, pch.a = 16, mfColRow = TRUE)
\S4method{plot}{DiscreteDistribution,missing}(x, width = 10, height = 5.5,
     withSweave = getdistrOption("withSweave"), xlim = NULL, ylim = NULL, verticals = TRUE,
     do.points = TRUE, main = FALSE, inner = TRUE, sub = FALSE, 
     bmar = par("mar")[1], tmar = par("mar")[3], ..., 
     cex.main = par("cex.main"), cex.inner = 1.2, cex.sub = par("cex.sub"), 
     col.points = par("col"), col.hor = par("col"), col.vert = par("col"), 
     col.main = par("col.main"), col.inner = par("col.main"), 
     col.sub = par("col.sub"),  cex.points = 2.0, pch.u = 21, pch.a = 16, mfColRow = TRUE)
\S4method{plot}{AffLinUnivarLebDecDistribution,missing}(x, width = 10, 
     height = 5.5, withSweave = getdistrOption("withSweave"), xlim = NULL, ylim = NULL, ngrid = 1000,
     verticals = TRUE, do.points = TRUE, main = FALSE, inner = TRUE, sub = FALSE,
     bmar = par("mar")[1], tmar = par("mar")[3], ..., cex.main = par("cex.main"), 
     cex.inner = 1.2, cex.sub = par("cex.sub"), col.points = par("col"),
     col.hor = par("col"), col.vert = par("col"), col.main = par("col.main"), 
     col.inner = par("col.main"), col.sub = par("col.sub"),  cex.points = 2.0,
     pch.u = 21, pch.a = 16, mfColRow = TRUE)
\S4method{plot}{UnivarLebDecDistribution,missing}(x, width = 10, 
     height = 14.5, withSweave = getdistrOption("withSweave"), xlim = NULL, ylim = NULL, ngrid = 1000,
     verticals = TRUE, do.points = TRUE, main = FALSE, inner = TRUE, sub = FALSE,
     bmar = par("mar")[1], tmar = par("mar")[3], ..., cex.main = par("cex.main"), 
     cex.inner = 0.9, cex.sub = par("cex.sub"), col.points = par("col"),
     col.hor = par("col"), col.vert = par("col"), col.main = par("col.main"), 
     col.inner = par("col.main"), col.sub = par("col.sub"),  cex.points = 2.0,
     pch.u = 21, pch.a = 16, mfColRow = TRUE)
\S4method{plot}{DistrList,missing}(x, y, ...)
}
\arguments{
  \item{x}{object of class 
            \code{"AffLinUnivarLebDecDistribution"} or class
            \code{"UnivarLebDecDistribution"} or class
            \code{"AbscontDistribution"} or class
            \code{"DiscreteDistribution"} or class
            \code{"DistrList"}: (list of) distribution(s) to be plotted }
  \item{y}{ missing }
  \item{xlim}{the x limits (x1, x2) of the plot.  Note that \code{x1 > x2} is
              allowed and leads to a "reversed axis". As in \code{plot.default}.}
  \item{ylim}{the y limits of the plot. As in \code{plot.default}.}
  \item{width}{width (in inches) of the graphics device opened}
  \item{height}{height (in inches) of the graphics device opened}
  \item{withSweave}{logical: if \code{TRUE} (for working with \command{Sweave}) 
        no extra device is opened and height/width are not set}
  \item{ngrid}{integer: number of grid points used for plots of absolutely continuous 
               distributions}
  \item{main}{logical: is a main title to be used? or \cr
              just as argument \code{main} in \code{\link{plot.default}}.}
  \item{inner}{logical: panels for density/probability function -
                        cdf - quantile function have their own titles? or \cr
               list of length 3: titles for density/probability function -
               cdf - quantile function (each of the same form as argument 
               \code{main} in \code{\link{plot.default}})} 
  \item{sub}{logical: is a sub-title to be used? or \cr
              just as argument \code{sub} in \code{\link{plot.default}}.}
  \item{tmar}{top margin -- useful for non-standard main title sizes}
  \item{bmar}{bottom margin -- useful for non-standard sub title sizes}
  \item{verticals}{logical: if \code{TRUE}, draw vertical lines at steps;
                   as in \code{\link[stats]{plot.stepfun}}} 
  \item{do.points}{logical: if \code{TRUE}, draw  also draw points at the 
         (\code{xlim} restricted) knot locations; as 
         in \code{\link[stats]{plot.stepfun}}} 
  \item{cex.points}{numeric; character expansion factor; as in 
                    \code{\link[stats]{plot.stepfun}}}              
  \item{col.points}{character or integer code; color of points; as in 
                    \code{\link[stats]{plot.stepfun}}}              
  \item{col.hor}{character or integer code; color of horizontal lines; as in 
                    \code{\link[stats]{plot.stepfun}}}              
  \item{col.vert}{character or integer code; color of vertical lines; as in 
                    \code{\link[stats]{plot.stepfun}}}              
  \item{cex.main}{magnification to be used for main titles relative
          to the current setting of \code{cex}; as in 
          \code{\link[stats]{par}}}
  \item{cex.inner}{magnification to be used for inner titles relative
          to the current setting of \code{cex}; as in 
          \code{\link[stats]{par}}}
  \item{cex.sub}{magnification to be used for sub titles relative
          to the current setting of \code{cex}; as in 
          \code{\link[stats]{par}}}
  \item{col.main}{character or integer code; color for the main title}              
  \item{col.inner}{character or integer code; color for the inner title}              
  \item{col.sub}{character or integer code; color for the sub title}              
  \item{pch.u}{character or integer code; plotting characters or symbols for
               unattained value; see \code{\link[stats]{points}}}              
  \item{pch.a}{character or integer code; plotting characters or symbols for
               attained value; see \code{\link[stats]{points}}}              
  \item{mfColRow}{shall default partition in panels be used --- defaults to \code{TRUE}}
  \item{\dots}{addtional arguments for \code{plot} --- see 
               \code{\link[stats]{plot}}, 
               \code{\link[stats]{plot.default}}, 
               \code{\link[stats]{plot.stepfun}} }
}
\details{
\describe{
\item{plot}{\code{signature(x = "AffLinUnivarLebDecDistribution", y = "missing")}: plots  
   cumulative distribution function and the quantile function }
\item{plot}{\code{signature(x = "UnivarLebDecDistribution", y = "missing")}: plots a set of
  eight plots: in the first row,  it plots the 
   cumulative distribution function and the quantile function; in the second
   row the absolutely continuous part (with density, cdf and quantile fct.),
   and in the last row the discrete part (with prob.fct., cdf and quantile fct.). }
\item{plot}{\code{signature(x = "AbscontDistribution", y = "missing")}: plots density, 
   cumulative distribution function and the quantile function }
\item{plot}{\code{signature(x = "DiscreteDistribution", y = "missing")}: plots probability 
function, cumulative distribution function and the quantile function}
}
Any parameters of \code{plot.default} may be passed on to this particular
\code{plot} method. 

For main-, inner, and subtitles given as arguments \code{main}, 
\code{inner}, and \code{sub}, top and bottom margins are enlarged to 5 resp. 
6 by default but may also be specified by \code{tmar} / \code{bmar} arguments. 
If \code{main} / \code{inner} / \code{sub} are 
logical then if the respective argument is \code{FALSE} nothing is done/plotted, 
but if it is \code{TRUE}, we use a default main title taking up the calling 
argument \code{x} in case of \code{main}, default inner titles taking up the
class and (named) parameter slots of argument \code{x} in case of \code{inner},
and a "generated on <data>"-tag in case of \code{sub}.
Of course, if \code{main} / \code{inner} / \code{sub} are \code{character}, this
is used for the title; in case of \code{inner} it is then checked whether it
has length 3. In all title arguments, the following patterns are substituted:
\itemize{
\item{\code{"\%C"}}{class of argument \code{x}}
\item{\code{"\%P"}}{parameters of \code{x} in form of a comma-separated list of 
                   <value>'s coerced to character}
\item{\code{"\%Q"}}{parameters of \code{x} in form of a comma-separated list of 
                   <value>'s coerced to character and in parenthesis --- unless 
                   empty; then ""}
\item{\code{"\%N"}}{parameters of \code{x} in form of a comma-separated list 
                   <name> = <value> coerced to character}
\item{\code{"\%A"}}{deparsed argument \code{x}}
\item{\code{"\%D"}}{time/date-string when the plot was generated}
}

If not explicitly set, \code{col.points}, \code{col.vert},
\code{col.hor}, \code{col.main}, \code{col.inner}, \code{col.sub} are set 
to \code{col} if this arg is given and else to
\code{par("col")} resp. for the titles \code{par("col.main")},
\code{par("col.main")}, \code{par("col.sub")}.  

If not explicitly set, \code{pch.a}, \code{pch.u} are set 
to \code{pch} if this arg is given and else to 16, 21, respectively.

If not explicitly set, \code{cex} is set to 1. If not explicitly set, 
\code{cex.points} is set to $2.0 \code{cex}$ (if \code{cex} is given)
and to 2.0 else.              
}

\examples{
plot(Binom(size = 4, prob = 0.3))
plot(Binom(size = 4, prob = 0.3), do.points = FALSE)
plot(Binom(size = 4, prob = 0.3), verticals = FALSE)
plot(Binom(size = 4, prob = 0.3), main = TRUE)
plot(Binom(size = 4, prob = 0.3), main = FALSE)
plot(Binom(size = 4, prob = 0.3), cex.points = 1.2, pch = 20)
B <- Binom(size = 4, prob = 0.3)
plot(B, col = "red", col.points = "green", main = TRUE, col.main = "blue", 
     col.sub = "orange", sub = TRUE, cex.sub = 0.6, col.inner = "brown")
plot(Nbinom(size = 4,prob = 0.3), cex.points = 1.2, col = "red", 
     col.points = "green")
plot(Nbinom(size = 4,prob = 0.3), cex.points = 1.2, pch.u = 20, pch.a = 10)
plot(Norm(), main = TRUE, cex.main = 3, tmar = 6)
plot(Norm(), inner = FALSE, main = TRUE, cex.main = 3, tmar = 6)
plot(Norm(), lwd = 3, col = "red", ngrid = 200, lty = 3, las = 2)
plot(Norm(), main = "my Distribution: \%A", 
     inner = list(expression(paste(lambda,"-density of \%C(\%P)")), "CDF",
                  "Pseudo-inverse with param's \%N"), 
     sub = "this plot was correctly generated on \%D", 
     cex.inner = 0.9, cex.sub = 0.8)
plot(Cauchy())
plot(Cauchy(), xlim = c(-4,4))
plot(Chisq())
plot(Chisq(), log = "xy", ngrid = 100)
Ch <- Chisq(); setgaps(Ch); plot(Ch, do.points = FALSE)
setgaps(Ch, exactq = 3); plot(Ch, verticals = FALSE)
plot(Ch, cex = 1.2, pch.u = 20, pch.a = 10, col.points = "green", 
     col.vert = "red")

## some distribution with gaps
wg <- flat.mix(UnivarMixingDistribution(Unif(0,1),Unif(4,5), 
               withSimplify=FALSE))
# some Lebesgue decomposed distribution 
mymix <- UnivarLebDecDistribution(acPart = wg, discretePart = Binom(4,.4),
         acWeight = 0.4)
plot(mymix)         
}
\seealso{\code{\link[stats]{plot}},\code{\link[stats]{plot.default}}, 
         \code{\link[stats]{plot.stepfun}},  \code{\link[stats]{par}}}
\keyword{methods}
\keyword{hplot}
\keyword{distribution}

