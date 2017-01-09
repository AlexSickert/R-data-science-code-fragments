
setwd("C:\\Users\\xandi\\Documents\\COURSERA-CAPSTONE\\Text-Files-Given\\en_US")

# C:\Users\xandi\Documents\COURSERA-CAPSTONE\Text-Files-Given\en_US

# java-2-gram-consolidated
library(R.utils)

i = 3

for(i in 2:6){

  gc()
  
  
  print("--------------------------------------") 
  print(Sys.time())
  print(paste("Loop: ", i, sep="",  collapse=""))
  
  # java-5-gram-reduced
  filenameInput <- paste("java-", i, "-gram-consolidated.txt", sep="",  collapse="") 
  filenameOutput <- paste("hash-Reduced-", i, ".RDA", sep="",  collapse="") 
  
  
  print("reading...")
  # input <- readRDS(file=filenameInput)
  input <- read.csv(file=filenameInput, sep=";", stringsAsFactors=F, quote="", comment.char="", header = FALSE)
  
  head(input)
  
  # conn <- file(filenameInput,open="r")  # Precondition: set the working directory to your directory by using 
  # input <-readLines(conn)
  
  print("converting ")
  
  outEnv <- new.env(hash=TRUE)
  
  print("number of lines:")
  print(nrow(input))
  
  counter = 0
  counterTotal = 0
  
  for(i in 1:nrow(input)) {
    row <- input[i,]
    # print(row)
    key <- row[1,1]
    val <- row[1,2]
    if(nchar(key) > 0){
      outEnv[[key]] <- val
    }
    
    if(counter == 1000){
      counter <- 0
      print(counterTotal)
      gc()
      
    }
    counter <- counter + 1
    counterTotal = counterTotal + 1
    
    
    # do stuff with row
  }
  
  print("save RDS...  ")
  saveRDS(outEnv, file=filenameOutput)
  
  print(Sys.time())
}




