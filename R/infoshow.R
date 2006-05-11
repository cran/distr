## borrowed from Andy Liaw's rfNews()


infoShow <- function(package, filename)
   {file.show(file.path(system.file(package = package), filename))}

NEWS<-function(package) 
{
    infoShow(package, filename="NEWS")
}

distrARITH<-function() 
{
    infoShow(package = "distr", filename="ARITHMETICS")
}
