.onLoad <- function(lib, pkg) { # extended 03-28-06: P.R. 
    require("methods", character = TRUE, quietly = TRUE)
}

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

getdistrOption<-function(x)distroptions(x)[[1]]

###must happen between load and attach

