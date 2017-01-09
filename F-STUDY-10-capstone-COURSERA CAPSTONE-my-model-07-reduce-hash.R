





setwd("C:\\Users\\xandi\\Documents\\COURSERA-CAPSTONE\\Text-Files-Given\\en_US")

setwd("D:\\TMP\\Coursera-Capstone\\Coursera-SwiftKey\\final\\en_US")

hashData <- readRDS(file="hash- 2 .dat")






prepareTables <- function(){
  predictionTables <<- list() 
  for(i in 1:ngramDepth){
    #predictionTables[[i]]  <<- data.frame( "nGramString" = character(), "searchString" = character(), "prediction" = character(), "frequency" = integer(), stringsAsFactors=FALSE)
    filename <- paste("hash-", i, ".dat",  collapse="") 
    if(!file.exists(filename)){
      print("creating NEW hash maps")
      predictionTables[[i]] <<- new.env()
    }else{
      print("creating hash maps from existing file")
      
      predictionTables[[i]] <<- readRDS(file=filename)
    }
  }
}

hashData <- predictionTables[[2]]

for(v in ls(predictionTables[[2]])){
  print(v)
  print(predictionTables[[2]][v])
  
}


#--------------------------------------------------

printMe <- function(x){
  
  
}

#--------------------------------------------------

hashReducer<- function(x, fileName){
  print(Sys.time())
  hashTmp <- new.env()
  counterInitial <- 0
  
  for(v in ls(x)){
    counterInitial <- counterInitial + 1
    printMe("--------------------------------")
    printMe(v)
    
    tmpString <- v
    counter <- x[[v]]
    
    s <- unlist(strsplit(tmpString, "_"))
    l <- length(s)
    newKey <- paste(s[1:l-1], collapse = "_")
    
    sLast <- s[l:l]
    
    tst <- is.null(hashTmp[[newKey]])
    
    if(tst){
      printMe("adding value to list")
      #key is not in list and therefore we add
      newVal <- paste(sLast, counter, collapse = "_", sep="_")
      printMe(paste("newKey:", newKey, collapse = " "))
      printMe(paste("newVal:", newVal, collapse = " "))
      hashTmp[[newKey]] <- newVal
    }else{
      # compare the values
      checkString <- hashTmp[[newKey]]
      printMe(paste("checkString:", checkString, collapse = " "))
      
      checkS <- unlist(strsplit(checkString, "_"))
      
      checkNum <- as.numeric(checkS[2:2])
      newNum <- as.numeric(counter)
      
      printMe(paste("checkNum:", checkNum, collapse = " "))
      printMe(paste("newNum:", newNum, collapse = " "))
      
      if(newNum > checkNum){
        printMe("--------------------------------")
        printMe("updating value")
        #use the new string because it has higher score
        newVal <- paste(sLast, counter, collapse = "_", sep="_")
        hashTmp[[newKey]] <- newVal
        printMe(paste("newKey:", newKey, collapse = " "))
        printMe(paste("checkNum:", checkNum, collapse = " "))
        printMe(paste("newNum:", newNum, collapse = " "))
        printMe(paste("newVal:", newVal, collapse = " "))
        
      }else{
        # print("--------------------------------")
        # print("ignoring value")
      }
      
    }
    
  }
  
  counterReduced <- 0
  for(v in ls(hashTmp)){
    counterReduced <- counterReduced + 1
  }
  
  print(paste("counterInitial hash:", counterInitial, collapse = " "))
  print(paste("reduced hash:", counterReduced, collapse = " "))
  print(Sys.time())
  
  return(hashTmp) 
}
#--------------------------------------------------

hashReducerSimple<- function(x, fileName){
  
  counterMax <- as.numeric(0) 
  outString <- ""
  hashTmp <- new.env()
  
  for(v in ls(x)){
    
    counter <- x[[v]]
    newNum <- as.numeric(counter)
    
    if(newNum > counterMax){
      outString <- v
      counterMax <- newNum
    }
    
  }
  
  newVal <- paste(outString, counterMax,  sep="_", collapse = "_")
  hashTmp[[outString]] <- newVal
  return(hashTmp) 
}

#--------------------------------------------------

#export hash

hashToStringToFile<- function(x, fileName){
  
  #file.remove(fileName)
  
  sink(fileName)
  
  for(v in ls(x)){
    cat(v)
    cat(", ")
    cat(x[[v]])
    cat("\r\n")
  }
  
  sink()
  
}
#--------------------------------------------------


x <- hashReducer(predictionTables[[2]], asdf)

hashToStringToFile(x, "test-reduced-hash.txt")

for(i in 1:1){
  
  print("--------------------------------------") 
  print(Sys.time())
  print(paste("Loop: ", i, sep="",  collapse=""))
  
  filenameInput <- paste("hash-", i, ".dat", sep="",  collapse="") 
  filenameOutput <- paste("hash-Reduced-", i, ".RDA", sep="",  collapse="") 
  # filenameOutputTxt <- paste("hash-Reduced-", i, ".txt", sep="",  collapse="") 
  
  input <- readRDS(file=filenameInput)
  
  if(i < 2){
    out <- hashReducerSimple(input, "asdfa")
  }else{
    out <- hashReducer(input, "asdfa")
  }
  
  
  saveRDS(out, file=filenameOutput)
  
  # hashToStringToFile(out, filenameOutputTxt)
  
  print(Sys.time())
  
}



x <- readRDS(file="hash-6.dat")
print(Sys.time())
vector = c()
i <- 1
y <- 0
for(v in ls(x)){
  # x
  #print(i) 
  vector[i] <- x[[v]]
  i <- i + 1
  
  if(y > 1000){
    print(i)
    y <- 0
  }else{
    y <- y + 1
  }
  
}
print(i) 
print(Sys.time())


hash-Reduced-1.RDA

x <- readRDS(file="hash-Reduced-1.RDA")
for(v in ls(x)){
  # x
  print(v) 
  print(x[[v]])
  
}


