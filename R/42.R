#########

## about Dataclass: slot Data
##
##
## ith run in ith line of datamatrix
## jth samples of each run in jth column of datamatrix


################################
##
## Class: Dataclass
##
################################

setClass("Dataclass",
         representation(filename = "vectororNULL",
                        Data = "vectororNULL",
                        runs = "numeric",
                        samplesize = "numeric"),
         prototype=list(filename = NULL, Data = NULL, runs = 0, samplesize = 0))

## Access methods
if(!isGeneric("filename")) setGeneric("filename", function(object) standardGeneric("filename"))
if(!isGeneric("Data")) setGeneric("Data", function(object) standardGeneric("Data"))
if(!isGeneric("runs")) setGeneric("runs", function(object) standardGeneric("runs"))
if(!isGeneric("samplesize")) setGeneric("samplesize", function(object) standardGeneric("samplesize"))
setMethod("filename", "Dataclass", function(object) object@filename)
setMethod("Data", "Dataclass", function(object) object@Data)
setMethod("runs", "Dataclass", function(object) object@runs)
setMethod("samplesize", "Dataclass", function(object) object@samplesize)
## Replace Methoden
if(!isGeneric("filename<-")) setGeneric("filename<-", function(object, value) standardGeneric("filename<-"))
if(!isGeneric("Data<-")) setGeneric("Data<-", function(object, value) standardGeneric("Data<-"))
if(!isGeneric("runs<-")) setGeneric("runs<-", function(object, value) standardGeneric("runs<-"))
if(!isGeneric("samplesize<-")) setGeneric("samplesize<-", function(object, value) standardGeneric("samplesize<-"))
setReplaceMethod("filename", "Dataclass", function(object, value){ object@filename <- value; object})
setReplaceMethod("Data", "Dataclass",
                 function(object, value){
                   object <- new("Dataclass",
                                 filename = filename(object),
                                 Data = value)                                              
                   object
                 })


## Initialize methods
setMethod("initialize", "Dataclass",
          function(.Object, filename = NULL, Data) {
            .Object@filename <- filename
            .Object@Data <- as.matrix(Data)
            .Object@runs <- nrow(as.matrix(Data))
            .Object@samplesize <- ncol(as.matrix(Data))
            .Object
          })

## Save method
if(!isGeneric("savedata")) setGeneric("savedata", function(object) standardGeneric("savedata"))

setMethod("savedata", "Dataclass", function(object){
  if(is.null(filename(object))) stop("This Dataclass object has to be given a filename before it can be saved to harddisk")
  
  
  eval.parent(substitute(save(object, file = filename(object))))
  
  name <- as.character(substitute(object))
  namecomment <- paste(name,".comment",sep="")
  filenamecomment <- paste(filename(object),".comment",sep="")
  
  eval.parent(parse(text=paste(namecomment," <- ",name,sep=""))) 
  eval.parent(parse(text=paste(namecomment,"@Data <- NULL",sep=""))) 
  eval.parent(parse(text=paste("save(",namecomment,", file = \"",filenamecomment,"\")",sep=""))) 
  eval.parent(parse(text=paste("rm(",namecomment,")",sep="")))
})

## Load Method for comments
cload <- function(filename){
  eval.parent(parse(text=paste("load(\"",filename,".comment\")", sep = "")))
}


## plot method

setMethod("plot","Dataclass",
          function(x,y=NULL,...){
            y0<-1:runs(x)
            matplot(y0,Data(x),xlab="Runindex",ylab="data",type="p",pch="*",col="blue")
          })

##summary

setMethod("summary","Dataclass",
          function(object,...){
            cat("filename of Dataclass: ",filename(object),"\n")
            cat("number of runs: ",runs(object),"\n")
            cat("size of sample: ",samplesize(object),"\n")
            if(is.null(Data(object))) simulate(object)
            y0<-1:(min(6,runs(object)))
            x0<-Data(object)[y0,]
            if(runs(object) == 1) apply(t(x0),1,summary)
            else apply(x0,1,summary)
          })


setMethod("print", "Dataclass",
          function(x, ...){
            cat("filename of Dataclass: ",filename(x),"\n")
            cat("number of runs: ",runs(x),"\n")
            cat("size of sample: ",samplesize(x),"\n")
          })
