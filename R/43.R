require(setRNG)

################################
##
## Class: Simulation
##
################################

setClass("Simulation",
         representation("Dataclass",
                        seed = "list",
                        distribution = "UnivariateDistribution"),
         contains="Dataclass")

setMethod("initialize", "Simulation",
          function(.Object, filename = NULL, runs = 100, samplesize = 10, seed = setRNG(), distribution = Norm()) {
            .Object@filename <- filename
            .Object@Data <- NULL
            .Object@runs <- runs
            .Object@samplesize <- samplesize        
            .Object@seed <- seed        
            .Object@distribution <- distribution        
            validObject(.Object)
            .Object
          })



## Access Methods
if(!isGeneric("seed")) setGeneric("seed", function(object) standardGeneric("seed"))
if(!isGeneric("distribution")) setGeneric("distribution", function(object) standardGeneric("distribution"))
setMethod("seed", "Simulation", function(object) object@seed)
setMethod("distribution", "Simulation", function(object) object@distribution)
## Replace Methoden
if(!isGeneric("seed<-")) setGeneric("seed<-", function(object, value) standardGeneric("seed<-"))
if(!isGeneric("distribution<-")) setGeneric("distribution<-", function(object, value) standardGeneric("distribution<-"))
setReplaceMethod("distribution", "Simulation",
                 function(object, value){
                   object <- new("Simulation",
                     seed = seed(object),
                     distribution = value,
                     filename = filename(object),
                     runs = runs(object),
                     samplesize = samplesize(object))                                              
                   object
                 })
setReplaceMethod("seed", "Simulation",
                 function(object, value){
                   object <- new("Simulation",
                     seed = value,
                     distribution = distribution(object),
                     filename = filename(object),
                     runs = runs(object),
                     samplesize = samplesize(object))                                              
                   object
                 })
setReplaceMethod("runs", "Simulation",
                 function(object, value){
                   object <- new("Simulation",
                     seed = seed(object),
                     distribution = distribution(object),
                     filename = filename(object),
                     runs = value,
                     samplesize = samplesize(object))                                              
                   object
                 })
setReplaceMethod("samplesize", "Simulation",
                 function(object, value){
                   object <- new("Simulation",
                     seed = seed(object),
                     distribution = distribution(object),
                     filename = filename(object),
                     runs = runs(object),
                     samplesize = value)
                   object
                 })

setReplaceMethod("Data", "Simulation", function(object, value){ stop("This slot should not be altered"); object})



validSimulation <- function(object){
  if(!identical(floor(samplesize(object)), samplesize(object)))
    stop("samplesize has to be a positive integer")      
  if(samplesize(object) <= 0)
    stop("samplesize has to be a positive integer")
  if(!identical(floor(runs(object)), runs(object)))
    stop("runs has to be a positive integer")      
  if(runs(object) <= 0)
    stop("runs has to be a positive integer")      
  else return(TRUE)
}

setValidity("Simulation", validSimulation)


## Simulation method
if(!isGeneric("simulate")) setGeneric("simulate",function(x,...)standardGeneric("simulate"))

setMethod("simulate",signature(x = "Simulation"),
          function(x){
            if(!is.null(Data(x)))
              return(invisible())
            setRNG(seed(x)) 
            eval.parent(substitute(x@Data<-matrix(r(distribution(x))(x@runs*x@samplesize),nrow=x@runs,ncol=x@samplesize))) 
            return(invisible())
          })

setMethod("savedata", "Simulation", function(object){
  if(is.null(filename(object))) stop("This simulation has to be given a filename before it can be saved to harddisk")
  
  name <- as.character(substitute(object))
  
  eval.parent(parse(text=paste(name,"@Data <- NULL",sep=""))) 
  eval.parent(substitute(save(object, file = filename(object))))
})

setMethod("plot","Simulation",
          function(x,y=NULL,...){
            if(is.null(Data(x)))
              stop("No Data found -> simulate first")
            
            y0<-1:runs(x)
            matplot(y0,Data(x),xlab="run-index",ylab="data",type="p",pch="*",col="blue")
          })


setMethod("summary","Simulation",
          function(object,...){
            if(is.null(Data(object)))
              stop("No Data found -> simulate first")
            
            cat("name of simulation: ",filename(object),"\n")
            cat("number der runs: ",runs(object),"\n")
            cat("size of sample: ",samplesize(object),"\n")
            y0<-1:(min(6,runs(object)))
            x0<-Data(object)[y0,]
            if(runs(object) == 1) apply(t(x0),1,summary)
            else apply(x0,1,summary)
          })


setMethod("print","Simulation",
          function(x,...){
            cat("filename of Simulation: ",filename(x),"\n")
            cat("number of runs: ",runs(x),"\n")
            cat("size of sample: ",samplesize(x),"\n")
            cat("Distribution:\n")
            print(distribution(x))
          })


