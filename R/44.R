require(setRNG)

################################
##
## Class: Contsimulation
##
################################

setClass("Contsimulation",
         representation("Dataclass",
                        ind = "vectororNULL",
                        Data.id = "vectororNULL",
                        Data.c = "vectororNULL",
                        rate = "numeric",
                        distribution.c = "UnivariateDistribution",
                        distribution.id = "UnivariateDistribution",
                        seed = "list"),
         contains = "Dataclass")            


setMethod("initialize", "Contsimulation",
          function(.Object,
                   filename = NULL,
                   runs = 100,
                   samplesize = 10,
                   seed = setRNG(),
                   distribution.id = Norm(),
                   distribution.c = Norm(sd = 3),
                   rate = 0.1) {
            .Object@Data <- NULL
            .Object@Data.id <- NULL
            .Object@Data.c <- NULL
            .Object@filename <- filename
            .Object@runs <- runs
            .Object@samplesize <- samplesize        
            .Object@seed <- seed        
            .Object@distribution.id <- distribution.id
            .Object@distribution.c <- distribution.c
            .Object@rate <- rate
            
            validObject(.Object)
            .Object
          })

## Access methods
if(!isGeneric("ind")) setGeneric("ind", function(object) standardGeneric("ind"))
if(!isGeneric("Data.id")) setGeneric("Data.id", function(object) standardGeneric("Data.id"))
if(!isGeneric("Data.c")) setGeneric("Data.c", function(object) standardGeneric("Data.c"))
if(!isGeneric("rate")) setGeneric("rate", function(object) standardGeneric("rate"))
if(!isGeneric("distribution.c")) setGeneric("distribution.c", function(object) standardGeneric("distribution.c"))
if(!isGeneric("distribution.id")) setGeneric("distribution.id", function(object) standardGeneric("distribution.id"))
if(!isGeneric("seed")) setGeneric("seed", function(object) standardGeneric("seed"))
setMethod("ind", "Contsimulation", function(object) object@ind)
setMethod("Data.id", "Contsimulation", function(object) object@Data.id)
setMethod("Data.c", "Contsimulation", function(object) object@Data.c)
setMethod("rate", "Contsimulation", function(object) object@rate)
setMethod("distribution.c", "Contsimulation", function(object) object@distribution.c)
setMethod("distribution.id", "Contsimulation", function(object) object@distribution.id)
setMethod("seed", "Contsimulation", function(object) object@seed)
## Replace Methoden
if(!isGeneric("rate<-")) setGeneric("rate<-", function(object, value) standardGeneric("rate<-"))
if(!isGeneric("distribution.c<-")) setGeneric("distribution.c<-", function(object, value) standardGeneric("distribution.c<-"))
if(!isGeneric("distribution.id<-")) setGeneric("distribution.id<-", function(object, value) standardGeneric("distribution.id<-"))
if(!isGeneric("seed<-")) setGeneric("seed<-", function(object, value) standardGeneric("seed<-"))
setReplaceMethod("rate", "Contsimulation",
                 function(object, value){
                   object <- new("Contsimulation",
                                 seed = seed(object),
                                 distribution.id = distribution.id(object),
                                 distribution.c = distribution.c(object),
                                 rate = value,
                                 filename = filename(object),
                                 runs = runs(object),
                                 samplesize = samplesize(object))                                              
                   object
                 })
setReplaceMethod("distribution.c", "Contsimulation",
                 function(object, value){
                   object <- new("Contsimulation",
                                 seed = seed(object),
                                 distribution.id = distribution.id(object),
                                 distribution.c = value,
                                 rate = rate(object),
                                 filename = filename(object),
                                 runs = runs(object),
                                 samplesize = samplesize(object))                                              
                   object
                 })
setReplaceMethod("distribution.id", "Contsimulation",
                 function(object, value){
                   object <- new("Contsimulation",
                                 seed = seed(object),
                                 distribution.id = value,
                                 distribution.c = distribution.c(object),
                                 rate = rate(object),
                                 filename = filename(object),
                                 runs = runs(object),
                                 samplesize = samplesize(object))                                              
                   object
                 })
setReplaceMethod("seed", "Contsimulation",
                 function(object, value){
                   object <- new("Contsimulation",
                                 seed = value,
                                 distribution.id = distribution.id(object),
                                 distribution.c = distribution.c(object),
                                 rate = rate(object),
                                 filename = filename(object),
                                 runs = runs(object),
                                 samplesize = samplesize(object))                                              
                   object})
setReplaceMethod("runs", "Contsimulation",
                 function(object, value){
                   object <- new("Contsimulation",
                                 seed = seed(object),
                                 distribution.id = distribution.id(object),
                                 distribution.c = distribution.c(object),
                                 rate = rate(object),
                                 filename = filename(object),
                                 runs = value,
                                 samplesize = samplesize(object))                                              
                   object})
setReplaceMethod("samplesize", "Contsimulation",
                 function(object, value){
                   object <- new("Contsimulation",
                                 seed = seed(object),
                                 distribution.id = distribution.id(object),
                                 distribution.c = distribution.c(object),
                                 rate = rate(object),
                                 filename = filename(object),
                                 runs = runs(object),
                                 samplesize = value)
                   object
                 })

setReplaceMethod("Data", "Contsimulation",
                 function(object, value){
                   stop("This slot should not be altered")
                   object
                 })

validContsimulation <- function(object){
  if(!identical(floor(samplesize(object)), samplesize(object)))
    stop("samplesize has to be a positive integer")      
  if(samplesize(object) <= 0)
    stop("samplesize has to be a positive integer")
  if(!identical(floor(runs(object)), runs(object)))
    stop("runs has to be a positive integer")      
  if(runs(object) <= 0)
    stop("runs has to be a positive integer")      
  if(rate(object) < 0)
    stop("rate has to be in [0,1]")
  if(rate(object) > 1)
    stop("rate has to be in [0,1]")
  else return(TRUE)
}

setValidity("Contsimulation", validContsimulation)




## Save method
setMethod("savedata", "Contsimulation", function(object){
  if(is.null(filename(object))) stop("This simulation has to be given a filename before it can be saved to harddisk")
  
  name <- as.character(substitute(object))
  
  eval.parent(parse(text=paste(name,"@Data <- NULL",sep=""))) 
  eval.parent(parse(text=paste(name,"@Data.id <- NULL",sep=""))) 
  eval.parent(parse(text=paste(name,"@Data.c <- NULL",sep="")))
  eval.parent(parse(text=paste(name,"@ind <- NULL",sep="")))     
  eval.parent(substitute(save(object, file = filename(object))))
})

## Simulation method

setMethod("simulate",signature(x="Contsimulation"),
          function(x){
            if(!is.null(Data(x)))
              return(invisible())

            setRNG(seed(x)) 
            
            eval.parent(substitute(x@Data.id <- matrix(r(distribution.id(x))(x@runs*x@samplesize),
                                                       x@runs,x@samplesize))) 
            eval.parent(substitute(x@Data.c <- matrix(r(distribution.c(x))(x@runs*x@samplesize),
                                                      x@runs,x@samplesize))) 
            eval.parent(substitute(x@ind <- matrix(rbinom(x@runs*x@samplesize,1,x@rate),x@runs,x@samplesize)))
            eval.parent(substitute(x@Data <- (1-x@ind)*x@Data.id+x@ind*x@Data.c))
            
            return(invisible())
          })


###Plot

setMethod("plot","Contsimulation",
          function(x,y=NULL,...){
            if(is.null(Data(x)))
              stop("No Data found -> simulate first")
            
            if(any(Data(x) == 0)) return("Warning: plot won't work properly")
            
            y0<-1:runs(x)
            x1 <- Data(x) * (1 - ind(x))
            x1[x1 == 0] <- Inf
            x2 <- Data(x) * ind(x)
            x2[x2 == 0] <- Inf
            
            matplot(y0,x1,ylim = range(Data(x)), xlab="run-index",ylab="data",type="p",pch="*",col="blue")
            if(any(x2 != Inf)) matpoints(y0,x2,type="p",pch="x",col="red")
          })



###summary

setMethod("summary","Contsimulation",
          function(object,...){
            if(is.null(Data(object)))
              stop("No Data found -> simulate first")
            
            cat("name of simulation: ",filename(object),"\n")
            cat("number of runs: ",runs(object),"\n")
            cat("size of sample: ",samplesize(object),"\n")
            cat("rate of contamination: ",rate(object),"\n")
            cat("real Data:\n")
            y0<-1:(min(6,runs(object)))
            x0<-Data(object)[y0,]
            if(runs(object) == 1) apply(t(x0),1,summary)
            else if(samplesize(object) == 1) apply(as.matrix(x0),1,summary)
            else apply(x0,1,summary)
          })

setMethod("print","Contsimulation",
          function(x,...){
            cat("filename of Contsimulation: ",filename(x),"\n")
            cat("number of runs: ",runs(x),"\n")
            cat("size of sample: ",samplesize(x),"\n")
            cat("rate of contamination: ",rate(x),"\n")
            
            distr.id <-distribution.id(x)
            distr.c <- distribution.c(x)
            
            cat("ideal distribution:\n")
            print(distr.id)
            cat("contaminating distribution:\n")
            print(distr.c)
          })
