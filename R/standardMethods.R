##########################################################################################################

## standardMethods

## generiert Standard Methoden zu einer bereits existierenden Klasse

## args:
##
## class         -       String, der den Klassennamen enthält
## writetofile   -       Logischer Wert, TRUE, falls die Ausgabe auch in eine Datei geschrieben werden soll
## directory     -       Zielverzeichnis für die Datei, falls eine erstellt werden soll

## value:
##
## keine Rückgabewerte

## Details:
##
## Beispiele sagen (manchmal) mehr als tausend Worte - siehe unten
## Falls die Ausgabe auch auf File geschrieben werden soll, so erstellt die Funktion eine Datei
## <classname>_StandardMethods.txt im angegebenen Verzeichnis directory

############################################################################################################

standardMethods <- function(class, writetofile = FALSE, directory){
  ClassRep <- getClassDef(class)
  SlotNames <- names(getSlots(ClassRep))
  nrSlots <- length(SlotNames)
  
  if(!nrSlots) return()
  
  part0 <- "if(!isGeneric(\""
  part1 <- "\")) setGeneric(\""
  part2 <- "\", function(object) standardGeneric(\""
  part3 <- "\"))"
  
  for(i in 1:nrSlots){
    string <- paste(part0,SlotNames[i],part1,SlotNames[i],part2,SlotNames[i],part3, "\n", sep = "")
    if(writetofile) cat(string, file = paste(directory, class, "_StandardMethods.txt", sep=""), append = FALSE)
    cat(string, sep = "")
  }    
  
  part1 <- "setMethod(\""
  part2 <- "\", \""
  part3 <- "\", function(object) object@"
  part4 <- ")"
  
  for(i in 1:nrSlots){
    string <- paste(part1,SlotNames[i],part2,class, part3, SlotNames[i],part4, "\n", sep = "")
    if(writetofile) cat(string, file = paste(directory, class, "_StandardMethods.txt", sep=""), append = TRUE)
    cat(string, sep = "")
  }    
  
  part0 <- "if(!isGeneric(\""
  part1 <- "<-\")) setGeneric(\""
  part2 <- "<-\", function(object, value) standardGeneric(\""
  part3 <- "<-\"))"
  
  for(i in 1:nrSlots){
    string <- paste(part0,SlotNames[i],part1,SlotNames[i],part2,SlotNames[i],part3, "\n", sep = "")
    if(writetofile) cat(string, file = paste(directory, class, "_StandardMethods.txt", sep=""), append = TRUE)
    cat(string, sep = "")
  }    
  
  part1 <- "setReplaceMethod(\""
  part2 <- "\", \""
  part3 <- "\", function(object, value){ object@"
  part4 <- " = value; object})"
  
  for(i in 1:nrSlots){
    string <- paste(part1,SlotNames[i],part2,class, part3, SlotNames[i],part4, "\n", sep = "")
    if(writetofile) cat(string, file = paste(directory, class, "_StandardMethods.txt", sep=""), append = TRUE)
    cat(string, sep = "")
  }    
}            



## Beispiel

##setClass("testclass", representation(a = "numeric", b = "character"))
##standardMethods("testclass")
##directory = "C:/Dokumente und Einstellungen/X/Eigene Dateien/Studium/R/SWP/"
##standardMethods("testclass", writetofile = TRUE, directory = directory)
