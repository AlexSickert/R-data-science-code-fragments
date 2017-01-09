
# -------------------------------------------------------------------------
# variables
# -------------------------------------------------------------------------

result <- ""
info <-""
depth <- 2
predictionHashMaps <<- list() 

setwd("C:\\Users\\xandi\\Google Drive\\COURSERA CAPSTONE")

# -------------------------------------------------------------------------
# load the data 
# -------------------------------------------------------------------------

for (i in 1:depth)
{
  fileName <- paste("hash-Reduced-", i, ".RDA", sep="",  collapse="")
  predictionHashMaps[[i]] <<- readRDS(file=fileName)
  
} 

# -------------------------------------------------------------------------
# 
# -------------------------------------------------------------------------
predict <- function(x){
  
  # clean the string
  
  # split the string
  
  # create waring that certain words are not in vocalbulary
  
  # combine the string to xxx_xxx
  
  #  preset values 
  
  result <<- ""
  info <<-"error"
  
  for (k in depth:1)
  {
    
    # here we need to cut the string in pieces that might match to the hash length !!!!
    
    found <- predictWithOneHashMap(predictionHashMaps[[k]], x)
    
    if(found){
      info <<- k
      break
    }
    
  } 
  
  print(paste("Result: ", result, sep="",  collapse=""))
  print(paste("Infol: ", info, sep="",  collapse=""))
  
  
}

# -------------------------------------------------------------------------
# 
# -------------------------------------------------------------------------
predictWithOneHashMap <- function(h, s){
  
  x <- h[[s]]
  t <- is.null(x) 
  
  if(t){
    found <- FALSE
  }else{
    found <- TRUE
    r <- unlist(strsplit(x, "_"))
    result <<- r[1:1]
    
  }
  
  return(found)
}
# -------------------------------------------------------------------------
# 
# -------------------------------------------------------------------------

predict("bazxdvarack")




