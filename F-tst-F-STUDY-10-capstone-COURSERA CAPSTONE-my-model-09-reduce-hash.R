
setwd("C:\\Users\\xandi\\Documents\\COURSERA-CAPSTONE\\Text-Files-Given\\en_US")

#setwd("D:\\TMP\\Coursera-Capstone\\Coursera-SwiftKey\\final\\en_US")


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

for(i in 1:6){
  
  print("--------------------------------------") 
  print(Sys.time())
  print(paste("Loop: ", i, sep="",  collapse=""))
  
  filenameInput <- paste("hash-", i, ".RDA", sep="",  collapse="") 
  filenameOutput <- paste("hash-Reduced-", i, ".RDA", sep="",  collapse="") 
  # filenameOutputTxt <- paste("hash-Reduced-", i, ".txt", sep="",  collapse="") 
  
  print("reading...")
  input <- readRDS(file=filenameInput)
  
  if(i < 2){
    print("reducing simple...")
    out <- hashReducerSimple(input, "asdfa")
  }else{
    print("reducing... ")
    out <- hashReducer(input, "asdfa")
  }
  
  print("save RDS...  ")
  saveRDS(out, file=filenameOutput)
  
  # hashToStringToFile(out, filenameOutputTxt)
  
  print(Sys.time())
  
}




