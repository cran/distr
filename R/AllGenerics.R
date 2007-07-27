########################################################################
### intentional maskings --- purpose: add formal arg "..." to generic...
########################################################################

############################################################################
### ------------------------------------------------
### In this comment substitute 'xxx' by 'df' and 'sd', respectively
### ------------------------------------------------
### We intentionally mask function 'xxx' from stats in order to add a formal
### argument "...". 
### functionality of 'stats::xxx' is completely retained, however
### for help to the original 'stats::xxx' function write
###       'help("xxx", package="stats")'                             
### for code to the original 'stats::xxx' function write
###       'stats::xxx'
############################################################################

## masking function df

df <- function(x, ...)
       {
        dots <- list(...)
        if(hasArg(df1)) df1 <- dots$"df1"
           else stop("Argument df1 missing")
        if(hasArg(df2)) df2 <- dots$"df2"
           else stop("Argument df2 missing")
        log.arg <- if(hasArg(log)) dots$"log" else FALSE 

        if(hasArg(ncp)) ncp <- dots$"ncp"
           else ncp <- 0

        if(isTRUE(all.equal(ncp,0))||(getRversion()>='2.4.0'))
             return(stats::df(x = x, df1 = df1, df2 = df2, 
                              log = log.arg, ncp=ncp))
        else
          ## preliminary version for ncp in df:
             { TQ <- getdistrOption("TruncQuantile")
               xz <- qchisq(1-TQ,df=df1,ncp=ncp)
               xn <- qchisq(TQ,df=df2,ncp=0)
               xl <- c(0,df2*xz/xn/df1)
               pfun <- function(x){pf(x, df1=df1, df2=df2, ncp=ncp)}
               dfun <- P2D(pfun, xl, 
                       ngrid = getdistrOption("DefaultNrGridPoints"))
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

## masking function sd

sd <- function(x, ...){
      dots <- list(...)
      na.rm <- ifelse(hasArg(na.rm), dots$"na.rm", FALSE)
      stats::sd(x = x, na.rm = na.rm)
      }

### ---------------------------
### "multiple-purpose generics" --- 
###   for  
###      + accessor method  
###      + stats/base function
###      + functional method (in case of 'sd' with additional package 'distrEx') 
### ---------------------------

### .. + stats function

if(!isGeneric("df")) 
    setGeneric("df", function(x, ...) standardGeneric("df"))

if(!isGeneric("sd")) 
    setGeneric("sd", function(x, ...) standardGeneric("sd"))

if(!isGeneric("mean")) 
    setGeneric("mean", function(x, ...) standardGeneric("mean"))

### .. + base function

if(!isGeneric("q")) 
    setGeneric("q", function(save = "default", status = 0, runLast = TRUE) 
                    standardGeneric("q"))

if(!isGeneric("scale")) 
    setGeneric("scale", function(x, center = TRUE, scale = TRUE) 
                                standardGeneric("scale"))


############################################################################
# Arithmetics
############################################################################

if(!isGeneric("log")) 
    setGeneric("log", function(x, base) standardGeneric("log"), group = "Math")
if(!isGeneric("log10")) 
    setGeneric("log10", function(x) standardGeneric("log10"), group = "Math")
if(!isGeneric("gamma")) 
    setGeneric("gamma", function(x) standardGeneric("gamma"), group = "Math")
if(!isGeneric("lgamma")) 
    setGeneric("lgamma", function(x) standardGeneric("lgamma"), group = "Math")




############################################################################
# Access methods
############################################################################

if(!isGeneric("name")) 
    setGeneric("name", function(object) standardGeneric("name"))
if(!isGeneric("dimension")) 
   setGeneric("dimension", function(object) standardGeneric("dimension"))

if(!isGeneric("shape")) 
   setGeneric("shape", function(object) standardGeneric("shape"))

if(!isGeneric("rate")) 
   setGeneric("rate", function(object) standardGeneric("rate"))

if(!isGeneric("Length"))  
   setGeneric("Length", function(object) standardGeneric("Length"))
if(!isGeneric("width"))   
   setGeneric("width", function(object) standardGeneric("width"))
if(!isGeneric("pivot"))   
   setGeneric("pivot", function(object) standardGeneric("pivot"))
if(!isGeneric("lattice")) 
   setGeneric("lattice", function(object) standardGeneric("lattice"))

if(!isGeneric("location")) 
   setGeneric("location", function(object) standardGeneric("location"))


if(!isGeneric("ncp")) 
   setGeneric("ncp", function(object) standardGeneric("ncp"))

if(!isGeneric("lambda")) 
   setGeneric("lambda", function(object) standardGeneric("lambda"))


if(!isGeneric("size")) 
   setGeneric("size", function(object) standardGeneric("size"))
if(!isGeneric("prob")) 
   setGeneric("prob", function(object) standardGeneric("prob"))

if(!isGeneric("support")) 
   setGeneric("support", function(object) standardGeneric("support"))

if(!isGeneric("m")) 
   setGeneric("m", function(object) standardGeneric("m"))
if(!isGeneric("n")) 
   setGeneric("n", function(object) standardGeneric("n"))
if(!isGeneric("k")) 
   setGeneric("k", function(object) standardGeneric("k"))

if(!isGeneric("img")) 
   setGeneric("img", function(object) standardGeneric("img"))
if(!isGeneric("param")) 
   setGeneric("param", function(object) standardGeneric("param"))
if(!isGeneric("r")) 
   setGeneric("r", function(object) standardGeneric("r"))
if(!isGeneric("d")) 
   setGeneric("d", function(object) standardGeneric("d"))
if(!isGeneric("p")) 
   setGeneric("p", function(object) standardGeneric("p"))

if(!isGeneric("p.l"))
   setGeneric("p.l", function(object) standardGeneric("p.l"))
if(!isGeneric("q.r"))
   setGeneric("q.r", function(object) standardGeneric("q.r"))

if(!isGeneric("gaps"))
   setGeneric("gaps", function(object) standardGeneric("gaps"))

if(!isGeneric("Min")) 
   setGeneric("Min", function(object) standardGeneric("Min"))
if(!isGeneric("Max")) 
   setGeneric("Max", function(object) standardGeneric("Max"))

if(!isGeneric("df1")) 
   setGeneric("df1", function(object) standardGeneric("df1"))
if(!isGeneric("df2")) 
   setGeneric("df2", function(object) standardGeneric("df2"))

if(!isGeneric("meanlog")) 
   setGeneric("meanlog", function(object) standardGeneric("meanlog"))
if(!isGeneric("sdlog")) 
   setGeneric("sdlog", function(object) standardGeneric("sdlog"))

if(!isGeneric("shape1")) 
   setGeneric("shape1", function(object) standardGeneric("shape1"))
if(!isGeneric("shape2")) 
    setGeneric("shape2", function(object) standardGeneric("shape2"))

############################################################################
# Replacement methods
############################################################################

if(!isGeneric("name<-")) 
    setGeneric("name<-", 
                function(object, value) standardGeneric("name<-"))
if(!isGeneric("dimension<-")) 
   setGeneric("dimension<-", 
               function(object, value) standardGeneric("dimension<-"))

if(!isGeneric("gaps<-")) 
   setGeneric("gaps<-", function(object, value) standardGeneric("gaps<-"))

if(!isGeneric("shape<-")) 
   setGeneric("shape<-", function(object, value) standardGeneric("shape<-"))
if(!isGeneric("scale<-")) 
   setGeneric("scale<-", function(object, value) standardGeneric("scale<-"))

if(!isGeneric("rate<-")) 
   setGeneric("rate<-", function(object, value) standardGeneric("rate<-"))

if(!isGeneric("Length<-")) 
   setGeneric("Length<-", function(object, value) standardGeneric("Length<-"))
if(!isGeneric("width<-"))  
   setGeneric("width<-", function(object, value) standardGeneric("width<-"))
if(!isGeneric("pivot<-"))  
   setGeneric("pivot<-", function(object, value) standardGeneric("pivot<-"))

if(!isGeneric("location<-")) 
   setGeneric("location<-", function(object, value) 
                                     standardGeneric("location<-"))

if(!isGeneric("df<-")) 
   setGeneric("df<-", function(object, value) standardGeneric("df<-"))
if(!isGeneric("ncp<-")) 
   setGeneric("ncp<-", function(object, value) standardGeneric("ncp<-"))

if(!isGeneric("lambda<-")) 
   setGeneric("lambda<-", function(object, value) standardGeneric("lambda<-"))

if(!isGeneric("size<-")) 
   setGeneric("size<-", function(object, value) standardGeneric("size<-"))
if(!isGeneric("prob<-")) 
   setGeneric("prob<-", function(object, value) standardGeneric("prob<-"))

if(!isGeneric("m<-")) 
   setGeneric("m<-", function(object, value) standardGeneric("m<-"))
if(!isGeneric("n<-")) 
   setGeneric("n<-", function(object, value) standardGeneric("n<-"))
if(!isGeneric("k<-")) 
   setGeneric("k<-", function(object, value) standardGeneric("k<-"))

if(!isGeneric("mean<-")) 
   setGeneric("mean<-", function(object, value) standardGeneric("mean<-"))
if(!isGeneric("sd<-")) 
   setGeneric("sd<-", function(object, value) standardGeneric("sd<-"))

if(!isGeneric("Min<-")) 
   setGeneric("Min<-", function(object, value) standardGeneric("Min<-"))
if(!isGeneric("Max<-")) 
   setGeneric("Max<-", function(object, value) standardGeneric("Max<-"))

if(!isGeneric("df1<-")) 
   setGeneric("df1<-", function(object, value) standardGeneric("df1<-"))
if(!isGeneric("df2<-")) 
   setGeneric("df2<-", function(object, value) standardGeneric("df2<-"))

if(!isGeneric("meanlog<-")) 
   setGeneric("meanlog<-", function(object, value) 
                           standardGeneric("meanlog<-"))
if(!isGeneric("sdlog<-")) 
   setGeneric("sdlog<-", function(object, value) standardGeneric("sdlog<-"))

if(!isGeneric("shape1<-")) 
   setGeneric("shape1<-", function(object, value) standardGeneric("shape1<-"))
if(!isGeneric("shape2<-")) 
    setGeneric("shape2<-", function(object, value) standardGeneric("shape2<-"))

############################################################################
# generics to  "usual"  methods
############################################################################

if(!isGeneric("liesIn")) 
   setGeneric("liesIn", function(object, x) standardGeneric("liesIn"))

if(!isGeneric("liesInSupport")) 
   setGeneric("liesInSupport", function(object, x) 
                               standardGeneric("liesInSupport"))
if(!isGeneric("convpow")) 
    setGeneric("convpow", function(D1, ...) standardGeneric("convpow"))

if(!isGeneric("getLow")) 
    setGeneric("getLow", function(object, ...) standardGeneric("getLow"))

if(!isGeneric("getUp")) 
    setGeneric("getUp", function(object, ...) standardGeneric("getUp"))

# general methods

if(!isGeneric("isOldVersion")) 
   setGeneric("isOldVersion", function(object) standardGeneric("isOldVersion"))

if(!isGeneric("conv2NewVersion")) 
   setGeneric("conv2NewVersion", 
               function(object) standardGeneric("conv2NewVersion"))
### setting gaps

if(!isGeneric("setgaps"))
   setGeneric("setgaps", function(object, ...) standardGeneric("setgaps"))

#### generics for log, log10, lgamma, gamma


if(!isGeneric("log"))
   setGeneric("log") #, function(x, base) standardGeneric("log"))
if(!isGeneric("log10"))
   setGeneric("log10")
if(!isGeneric("lgamma"))
   setGeneric("lgamma")
if(!isGeneric("gamma"))
   setGeneric("gamma")


