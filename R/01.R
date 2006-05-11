.onLoad <- function(lib, pkg) { # extended 03-28-06: P.R. 
    require("methods", character = TRUE, quietly = TRUE)
}

setGeneric("log", function(x, base) standardGeneric("log"), group = "Math")
setGeneric("log10", function(x) standardGeneric("log10"), group = "Math")
setGeneric("gamma", function(x) standardGeneric("gamma"), group = "Math")
setGeneric("lgamma", function(x) standardGeneric("lgamma"), group = "Math")


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
  if (sys.parent() == 0) env <- asNamespace("distr") else env <- parent.frame()
  assign(".distroptions", current, envir = env)
  invisible(current)
}

getdistrOption<-function(x)
distroptions(x)[[1]]

## masking function df

df <- function(x, ...)
       {dots <- list(...)
        if(hasArg(df1)) df1 <- dots$"df1"
           else stop("Argument df1 missing")
        if(hasArg(df2)) df2 <- dots$"df2"
           else stop("Argument df2 missing")
        log.arg <- ifelse(!hasArg(log), FALSE, dots$"log") 
        
## preliminary version for ncp in df:
        if(hasArg(ncp)) ncp <- dots$"ncp"
           else ncp = 0
 
        if(isTRUE(all.equal(ncp,0)))
             return(stats::df(x = x, df1 = df1, df2 = df2, log = log.arg))
        else      
             { TQ <- getdistrOption("TruncQuantile")
               xz <- qchisq(1-TQ,df=df1,ncp=ncp); xn<-qchisq(TQ,df=df2,ncp=0)
               xl <- c(0,df2*xz/xn/df1)
               pfun <- function(x){pf(x, df1=df1, df2=df2, ncp=ncp)}
               dfun <- P2D(pfun, xl, ngrid = getdistrOption("DefaultNrGridPoints"))
               #
               ## simulational alternative:
               #rfun <- function(x){rf(x, df1=df1, df2=df2, ncp=ncp)}
               #dfun <-R2D(rfun, nsim = 10^getdistrOption("RtoDPQ.e"), 
               #            n = getdistrOption("DefaultNrGridPoints"))
               d <- dfun(x)
               if(log.arg==TRUE) return(log(d))
               else return(d)
              }
        }   
