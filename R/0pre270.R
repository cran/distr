### for working under R < 2.8.0
if(getRversion()<'2.8.0'){
    devNew <- function(...){
        if(length(dev.list())>0)
        get(getOption("device"))(...)
    }
}else{
    devNew <- function(...){
        if(length(dev.list())>0)
           dev.new(...)
    }
}

