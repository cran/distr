.onLoad <- function(lib, pkg){
    require("methods", character = TRUE, quietly = TRUE) 
}

setGeneric("log", function(x, base) standardGeneric("log"), group = "Math")
setGeneric("log10", function(x) standardGeneric("log10"), group = "Math")
setGeneric("gamma", function(x) standardGeneric("gamma"), group = "Math")
setGeneric("lgamma", function(x) standardGeneric("lgamma"), group = "Math")


DefaultNrGridPoints <- 2^12
DistrResolution <- 1e-6
TruncQuantile <- 1e-5
DefaultNrFFTGridPointsExponent <- 12
RtoDPQ.e <- 5

distroptions <- function(arg = "missing", value = -1){
  globals <- list(DefaultNrFFTGridPointsExponent = DefaultNrFFTGridPointsExponent,
    DefaultNrGridPoints = DefaultNrGridPoints,
    DistrResolution = DistrResolution,
    RtoDPQ.e = RtoDPQ.e,
    TruncQuantile = TruncQuantile)
  if(arg == "missing"){
    print(globals)
    return(invisible())
  }
  if(!any(arg == names(globals)))
    stop(paste("No such variable:", arg))
  if(value == -1)
    switch(arg,
           DefaultNrGridPoints = DefaultNrGridPoints,
           DistrResolution = DistrResolution,
           TruncQuantile = TruncQuantile,
           DefaultNrFFTGridPointsExponent = DefaultNrFFTGridPointsExponent,
           RtoDPQ.e = RtoDPQ.e)
  else
     eval.parent(parse(text = paste("assignInNamespace(\"",arg, "\",", value, ", \"distr\")", sep = "")))
}


