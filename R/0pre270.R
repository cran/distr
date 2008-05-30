### for working under R < 2.8.0
if(getRversion()<'2.8.0'){
    devNew <- function(...){
        get(getOption("device"))(...)
    }
}else{
    devNew <- function(...){
        dev.new(...)
    }
}

