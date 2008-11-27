.distroptions <- list(
                      DefaultNrGridPoints = 2^12,
                      DistrResolution = 1e-6,
                      TruncQuantile = 1e-5,
                      DefaultNrFFTGridPointsExponent = 12,
                      RtoDPQ.e = 5, 
                      # new Warning-items P.R. 28.03.06
                      WarningArith = TRUE,
                      WarningSim = TRUE,
                      ## new Items from 2.0:
                      withgaps = TRUE,
                      simplifyD = TRUE,
                      DistrCollapse = TRUE,
                      withSweave = FALSE
                      )
distroptions <- function(...) {
  if (nargs() == 0) return(.distroptions)
  current <- .distroptions
  temp <- list(...)
  if (length(temp) == 1 && is.null(names(temp))) {
    arg <- temp[[1]]
    switch(mode(arg),
           list = temp <- arg,
           character = return(.distroptions[arg]),
           stop("invalid argument: ", sQuote(arg)))
  }
  if (length(temp) == 0) return(current)
  n <- names(temp)
  if (is.null(n)) stop("options must be given by name")
  changed <- current[n]
  current[n] <- temp
  env <- if (sys.parent() == 0) asNamespace("distr") else parent.frame()
  assign(".distroptions", current, envir = env)
  invisible(current)
}

getdistrOption <- function(x)distroptions(x)[[1]]

options("newDevice" = FALSE)